; Loader4.asm - Module loader v4
; Written in 2013 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

include 'include\Header.inc'

org $7E00

use32

Rebase:
	mov ax, 2 * 8
	mov ds, ax

	xor ecx, ecx
	.Loop1:
		mov eax, [ds:$37E00 + ecx]
		mov [ds:$7E00 + ecx], eax
		add ecx, 4
		cmp ecx, $1000
		jb .Loop1

	xor ecx, ecx
	.Loop2:
		mov eax, [ds:$40000 + ecx]
		mov [ds:$30000 + ecx], eax
		add ecx, 4
		cmp ecx, $10000
		jb .Loop2

	lgdt [GDT_Desc]
	jmp $8:Enter_Long_mode

Const:
	Lvl4_page_map_table = $2000
	First_page_directory_pointer_table = $3000
	First_page_directory = $4000
	First_page_table = $5000

	IA32_EFER_MSR = $C0000080

Enter_Long_mode:
	mov ax, 2 * 8
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov esp, $2000

	mov eax, cr4
	bts eax, 5
	mov cr4, eax

	mov ebx, Lvl4_page_map_table
	mov cr3, ebx

	xor eax, eax
	xor ecx, ecx
	.Zero_fill_PML4:
		mov [ebx + ecx], eax
		add ecx, 4
		cmp ecx, $1000
		jb .Zero_fill_PML4

	mov [ebx], dword First_page_directory_pointer_table + 1
	mov ebx, First_page_directory_pointer_table

	xor ecx, ecx
	.Zero_fill_page_directory_pointer_table:
		mov [ebx + ecx], eax
		add ecx, 4
		cmp ecx, $1000
		jb .Zero_fill_page_directory_pointer_table

	mov [ebx], dword First_page_directory + 1
	mov ebx, First_page_directory

	xor ecx, ecx
	.Zero_fill_page_directory:
		mov [ebx + ecx], eax
		add ecx, 4
		cmp ecx, $1000
		jb .Zero_fill_page_directory

	mov [ebx], dword First_page_table + 1
	mov ebx, First_page_table

	xor ecx, ecx
	inc eax
	.Identity_map_120000h_bytes:
		mov [ebx + ecx], eax
		mov [ebx + ecx + 4], dword 0
		add ecx, 8
		add eax, $1000
		cmp ecx, $900
		jb .Identity_map_120000h_bytes

	xor eax, eax
	.Zero_the_rest:
		mov [ebx + ecx], eax
		add ecx, 4
		cmp ecx, $1000
		jb .Zero_the_rest

	mov ecx, IA32_EFER_MSR
	rdmsr
	bts eax, 8
	wrmsr

	mov eax, cr0
	bts eax, 31
	mov cr0, eax

	mov ax, 5 * 8
	ltr ax

	jmp 24:Begin

include 'Loader4_GDT.inc'

use64

Begin:
	mov rbx, $B8000
	mov [rbx], byte '6'
	mov [rbx + 1], byte 1010b
	mov [rbx + 2], byte '4'
	mov [rbx + 3], byte 1010b

Init_core_system:
	mov rax, $30000
	call rax

	push 1
	mov rax, Procedure_INT0
	push rax
	invoke IException, Install_ISR

	hlt

Procedure_INT0:
	hlt