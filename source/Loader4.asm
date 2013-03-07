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
	mov fs, ax

	xor ecx, ecx
	.Loop1:
		mov eax, [fs:$37E00 + ecx]
		mov [fs:$7E00 + ecx], eax
		add ecx, 4
		cmp ecx, $1000
		jb .Loop1

	xor ecx, ecx
	.Loop2:
		mov eax, [fs:$40000 + ecx]
		mov [fs:$20000 + ecx], eax
		add ecx, 4
		cmp ecx, $10000
		jb .Loop2

	lgdt [fs:GDT_Desc]
	jmp $8:Begin

Begin:
	mov ax, 2 * 8
	mov fs, ax

	mov ax, 4 * 8
	mov ds, ax
	mov es, ax

	mov ax, 5 * 8
	mov ss, ax
	mov esp, $FFFFFFE0

	mov ax, 6 * 8
	mov gs, ax
	mov ebp, 16

	mov [ds:8], byte ' '

	mov [fs:$4000], dword 0
	mov [fs:$4004], dword GDT
	mov [fs:$4008], dword 0
	mov [fs:$400C], dword 0
	mov [fs:$4010], dword 0
	mov [fs:$4014], dword Function_Cardinal_to_HexStr_32

	call Init_core_system

Halt32:
	hlt
	jmp Halt32

Main_thread:
	mov [fs:$B8000], byte '$'
	mov [fs:$B8001], byte 1010b
	jmp Halt32

GDT:
	.gdt_null:	; 0 - Null
		dq 0
	.gdt_code:	; 1 - CS
		dw $01FF
		dw 0
		db 0
		db 10011010b
		db 11000000b
		db 0
	.gdt_gdata:	; 2 - FS
		dw $FFFF
		dw 0
		db 0
		db 10010010b
		db 11001111b
		db 0
	.ldt1:		 ; 3 - Application LDT
		dw $1F
		dw 0
		db 0
		db 10000010b
		db 01000000b
		db 0
	.data:		; 4 - DS
		dw $1
		dw $2000
		db $1
		db 10010010b
		db 11000000b
		db 0
	.stack1:	; 5 - SS
		dw $FFFE
		dw $1000
		db $1
		db 10010110b
		db 11001111b
		db 0
	.stack2:	; 6 - GS
		dw $0
		dw $1000
		db $1
		db 10010010b
		db 11000000b
		db 0
	.tss:		; 7 - TSS
		dw 103
		dw TSS
		db 0
		db 10001001b
		db 00000000b
		db 0
	gdt_end:

GDT_Desc:
	dw gdt_end - GDT - 1
	dd GDT

TSS:
	.Link dd 0
	.ESP0 dd 0
	.SS0 dd 5 * 8
	.ESP1 dd 0
	.SS1 dd 0
	.ESP2 dd 0
	.SS2 dd 0
	.CR3 dd $13000
	.EIP dd 0
	.EFLAGS dd 0
	.EAX dd 0
	.ECX dd 0
	.EDX dd 0
	.EBX dd 0
	.ESP dd 0
	.EBP dd 0
	.ESI dd 0
	.EDI dd 0
	.ES dd 0
	.CS dd 0
	.SS dd 0
	.DS dd 0
	.FS dd 0
	.GS dd 0
	.LDT dd 3 * 8
	.Trap dw 0
	.IO_map dw 104

Function_Cardinal_to_HexStr_32:
	.Num equ dword [gs:ebp - 8]
	.HexStr equ dword [gs:ebp - 4]

	push ebp
	add ebp, 8
	push ebx
	push ecx
	push edx
	push edi

	mov edx, .Num
	xor ebx, ebx
	mov edi, .HexStr

	mov cl, 7
	.Loop:
	mov eax, edx
	shl cl, 2
	shr eax, cl
	shr cl, 2
	and al, $F

	cmp al, $A
	jae .j1
	add al, '0' - 0
	jmp .j2
	.j1: add al, 'A' - $A
	.j2: inc ebx

	mov [ds:edi + ebx - 1], al

	.Continue_loop:
	dec cl
	jns .Loop

	.Return:
	xor eax, eax
	pop edi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	restore .Num
	restore .HexStr

Function_Write_string:
	.Str equ dword [gs:ebp - 12] ; Str : FS_address
	.Count equ dword [gs:ebp - 4] ; Count : Card32

	push ebp
	add ebp, 8

	.Return:
	xor eax, eax

	pop ebp
	ret

	restore .Str
	restore .Count

include 'Loader3_p3.inc'