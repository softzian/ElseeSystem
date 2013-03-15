; DOSboot.asm - Boot from DOS loader
; Written in 2013 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

org $100
use16

Begin:
	push Var.Loader4_filename
	push $1000
	push $7E00
	push $3000
	call Procedure_Load_file

	push Var.Excp_filename
	push $1000
	push $0
	push $4000
	call Procedure_Load_file

	push Var.Memory_filename
	push $1000
	push $1000
	push $4000
	call Procedure_Load_file

	;push Var.Module_filename
	;push $1000
	;push $1000
	;push $4000
	;call Procedure_Load_file

	;push Var.Video_filename
	;push $1000
	;push $2000
	;push $4000
	;call Procedure_Load_file

	;push Var.8259A_filename
	;push $1000
	;push $3000
	;push $4000
	;call Procedure_Load_file

	;push Var.Thread_filename
	;push $1000
	;push $4000
	;push $4000
	;call Procedure_Load_file

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

	jmp Switch_to_Protected_Mode

Procedure_Load_file:
	.Filename equ word [ss:bp + 10]
	.Size equ word [ss:bp + 8]
	.Dest_off equ word [ss:bp + 6]
	.Dest_seg equ word [ss:bp + 4]

	push bp
	mov bp, sp

	mov ah, $3D
	xor al, al
	mov dx, .Filename
	xor cl, cl
	int $21

	jc .Error1

	push ds
	push ax

	mov bx, ax
	mov cx, .Size
	mov ax, .Dest_seg
	mov ds, ax
	mov dx, .Dest_off
	mov ah, $3F
	int $21

	jc .Error2

	.Finish:
	pop bx
	mov ah, $3E
	int $21

	pop ds

	mov ah, 9
	mov dx, .Filename
	int $21
	mov ah, 9
	mov dx, Var.Text1
	int $21

	.Return:
	pop bp
	ret 8

	.Error1:
	mov ah, 9
	mov dx, Var.Error_text1
	int $21

	mov ah, 9
	mov dx, .Filename
	int $21

	int $20

	.Error2:
	pop bx
	mov ah, $3E
	int $21

	pop ds

	mov ah, 9
	mov dx, Var.Error_text2
	int $21

	mov ah, 9
	mov dx, .Filename
	int $21

	int $20

	restore .Filename
	restore .Dest_off
	restore .Dest_seg

Var:
	.Loader4_filename db 'Loader4.bin',0,'$'
	.Excp_filename db 'Excp.bin',0,'$'
	.Memory_filename db 'Memory.bin',0,'$'
	.Module_filename db 'Module.bin',0,'$'
	.Video_filename db 'CoreVGA.bin',0,'$'
	.8259A_filename db '8259A.bin',0,'$'
	.Thread_filename db 'Thread.bin',0,'$'
	.Error_text1 db 'Can not open file $'
	.Error_text2 db 'Can not read file $'
	.Text1 db 'is loaded',13,10,'$'

Switch_to_Protected_Mode:
	cli
	call enable_A20

	xor ebx, ebx
	mov bx, ds
	shl ebx, 4

	add [GDT_Desc.gdtaddr], ebx

	xor ax, ax
	mov es, ax
	mov [es:0], byte $EA
	mov [es:1], byte $00
	mov [es:2], byte $7E
	mov [es:3], byte $03
	mov [es:4], byte $00
	mov [es:5], byte $08
	mov [es:6], byte $00

	lgdt [GDT_Desc]
	mov eax, cr0
	or eax, 1
	mov cr0, eax
	jmp $8:$0

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
gdt_end:

GDT_Desc:
	dw gdt_end - GDT - 1
	.gdtaddr dd GDT