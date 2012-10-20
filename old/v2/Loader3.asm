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

	push Var.System_Module
	push dword $10000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Video_Module
	push dword $11000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Utility_Module
	push dword $12000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Interrupt_Module
	push dword $13000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Thread_Module
	push dword $14000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Keyboard_Module
	push dword $15000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Console_Module
	push dword $16000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Convert_Module
	push dword $17000
	call Function_Download_File
	test ax, ax
	jnz Abort

	call Procedure_PXE_Finish

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
	.Interrupt_Module db 9,0,'8259A.bin'
	.System_Module db 10,0,'System.bin'
	.Video_Module db 7,0,'VGA.bin'
	.Utility_Module db 11,0,'Utility.bin'
	.Thread_Module db 10,0,'Thread.bin'
	.Keyboard_Module db 8,0,'8042.bin'
	.Console_Module db 11,0,'Console.bin'
	.Convert_Module db 11,0,'Convert.bin'

	.Text db 7,0,'Success'
	.UTF32_Str dd 'H','e','l','l','o',' ','W','o','r','l','d'
	.Console dd 0

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
	gdt_null:
		dq 0
	gdt_code:
		dw $01FF
		dw 0
		db 0
		db 10011010b
		db 11001111b
		db 0
	gdt_gdata:
		dw $FFFF
		dw 0
		db 0
		db 10010010b
		db 11001111b
		db 0
	gdt_data:
		dw $FDFF
		dw 0
		db $40
		db 10010110b
		db 11001111b
		db 0
	gdt_end:
GDT_Desc:
	dw gdt_end - GDT
	dd GDT

use32

Begin:
	mov ax, $18
	mov ds, ax
	mov ss, ax
	mov es, ax

	mov ax, $10
	mov fs, ax

	mov esp, $FFFFFFFF - $17FFFF

	call Init_base_system

include 'Loader3_p3.inc'

Halt32:
	hlt
	jmp Halt32

Thread_1:
	push $1029
	invoke IUtility.Write_Cardinal_Hex
	mov [esp - 1], byte ' '
	dec esp
	invoke IUtility.Write_Char
	repeat 17
		hlt
	end repeat
	jmp Thread_1

