; Loader3.asm - Early stage module loader v3 - PXE Boot
; Written in 2012 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

include 'include\Header.inc'
include 'include\Errcode.inc'

org $7C00

use16

PXE_Loader:
	call Procedure_PXE_Start

	push Var.Loader4_Module
	push dword $37E00
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Excp_Module
	push dword $40000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Memory_Module
	push dword $41000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Module_Module
	push dword $42000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.8259A_Module
	push dword $43000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Thread_Module
	push dword $44000
	call Function_Download_File
	test ax, ax
	jnz Abort

	call Procedure_PXE_Finish

	push ds
	xor ax, ax
	mov ds, ax
	mov [$F000], ax
	mov [$F002], ax
	mov ax, $E801
	int $15
	test ax, ax
	jz .Use_CX
	mov [$F000], ax
	mov [$F002], bx
	jmp .j1
	.Use_CX:
	mov [$F000], cx
	mov [$F002], dx
	.j1: pop ds

Switch_to_Protected_Mode:
	cli
	call enable_A20
	xor ax, ax
	mov ds, ax
	lgdt [ds:GDT_Desc]
	mov eax, cr0
	or eax, 1
	mov cr0, eax
	jmp $8:Begin

include 'Loader3_p1.inc'
include 'Loader3_p2.inc'

Var:
	.Loader4_Module db 11,0,'Loader4.bin'
	.Excp_Module db 8,0,'Excp.bin'
	.Memory_Module db 10,0,'Memory.bin'
	.Module_Module db 10,0,'Module.bin'
	.8259A_Module db 9,0,'8259A.bin'
	.Thread_Module db 10,0,'Thread.bin'

Halt:
	hlt
	jmp Halt

Abort:
	call Print_Failure
	jmp Halt

enable_A20:
	call .a20wait
	mov al, $AD
	out $64, al

	call .a20wait
	mov al, $D0
	out $64, al

	call .a20wait2
	in al, $60
	push ax

	call .a20wait
	mov al, $D1
	out $64, al

	call .a20wait
	pop ax
	or al, 2
	out $60, al

	call .a20wait
	mov al, $AE
	out $64, al

	call .a20wait
	ret

.a20wait:
	in al, $64
	test al, 2
	jnz .a20wait
	ret
.a20wait2:
	in al, $64
	test al, 1
	jz .a20wait2
	ret

GDT:
	.gdt_null:	; 0 - Null
		dq 0
	.gdt_code:	; 1 - Code
		dw $FFFF
		dw 0
		db 0
		db 10011010b
		db 11001111b
		db 0
	.gdt_gdata:	; 2 - Data
		dw $FFFF
		dw 0
		db 0
		db 10010010b
		db 11001111b
		db 0
GDT_end:
GDT_Desc:
	dw GDT_end - GDT
	dd GDT

use32

Begin:
	jmp dword $37E00
