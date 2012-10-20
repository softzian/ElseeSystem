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

	;push Var.Utility_Module
	;push dword $12000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

	;push Var.Interrupt_Module
	;push dword $13000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

	;push Var.Thread_Module
	;push dword $14000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

	;push Var.Keyboard_Module
	;push dword $15000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

	;push Var.Console_Module
	;push dword $16000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

	;push Var.Convert_Module
	;push dword $17000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

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
	.Text2 db 'Hello World'
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
	.gdt_null:	; 0
		dq 0
	.gdt_code:	; 1
		dw $01FF
		dw 0
		db 0
		db 10011010b
		db 11001111b
		db 0
	.gdt_gdata:	; 2
		dw $FFFF
		dw 0
		db 0
		db 10010010b
		db 11001111b
		db 0
	.gdt_data:	; 3
		dw $FDFF
		dw 0
		db $40
		db 10010110b
		db 11001111b
		db 0
	.gdt_stack1:	; 4
		dw $FFFE
		dw $5000
		db 0
		db 10010110b
		db 11001111b
		db 0
	.gdt_stack2:	; 5
		dw 0
		dw $5000
		db 0
		db 10010010b
		db 11000000b
		db 0
	.ldt:		; 6
		dw $000F
		dw 0
		db $2
		db 10000010b
		db 11000000b
		db 0
GDT_end:
GDT_Desc:
	dw GDT_end - GDT
	dd GDT

use32

Begin:
	mov ax, 2 * 8
	mov fs, ax

	mov ax, 3 * 8
	mov ds, ax
	mov es, ax

	mov ax, 4 * 8
	mov ss, ax
	mov esp, $FFFFFFE0

	mov ax, 5 * 8
	mov gs, ax
	mov [gs:0], dword 16
	mov ebp, 16

	xor eax, eax
	xor ecx, ecx
	.Loop1:
		mov [fs:$20000 + ecx], eax
		add ecx, 4
		cmp ecx, $10000
		jb .Loop1

	call Init_base_system

	mov [gs:ebp], dword Var.Text2
	mov [gs:ebp + 4], dword $FFF00000
	mov [gs:ebp + 8], dword 11
	add [gs:0], dword 12
	invoke ISystem.Copy_code_to_data

	mov ebp, [gs:0]
	mov [gs:ebp], dword $FFF00000
	mov [gs:ebp + 4], word 11
	add [gs:0], dword 6
	invoke IVideo.Write_Telex

Halt32:
	hlt
	jmp Halt32

include 'Loader3_p3.inc'

