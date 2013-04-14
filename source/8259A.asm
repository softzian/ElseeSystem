; 8259A.asm - 8259A PIC Module
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
use64

; IInterrupt
; Function 1: Install_IRQ_direct_handler (IRQ : Byte; Entry_point : Address)
; Function 2: Mask_all_IRQ
; Function 3: Enable_IRQ (IRQ : Byte)
; Function 4: Disable_IRQ (IRQ : Byte)
; Function 5: Send_EOI

jmp near Function_Init
dq Header
Interface:
	dq Function_Install_IRQ_direct_handler
	dq Function_Mask_all_IRQ
	dq Function_Enable_IRQ
	dq Function_Disable_IRQ
	dq Function_Send_EOI
Header:
	.Module_addr dq 0
	.Module_id dq 4, 0

Const:

Function_Init:
	mov rbx, rax
	lea rsi, [rax + Interface]
	mov rax, IInterrupt
	mov [rax], rbx
	mov [rax + 8], rsi
	mov [Header.Module_addr], rbx

	xor rax, rax
	.Loop:
		add [rsi + rax], rbx
		add rax, 8
		cmp rax, Header - Interface
		jb .Loop

	push rbx
	push qword [Header.Module_id]
	push qword [Header.Module_id + 8]
	invoke IModule, Register_module

	.Return:
	xor rax, rax
	ret

Function_Init_PIC:
	mov al, $11
	out $20, al
	out $A0, al

	mov al, $20
	out $21, al
	mov al, $28
	out $A1, al

	mov al, 4
	out $21, al
	mov al, 2
	out $A1, al

	mov al, 1
	out $21, al
	out $A1, al

	xor rax, rax
	ret

Function_Install_IRQ_direct_handler: ; Function 1
	.IRQ equ byte [rbp + 24]
	.Entry_point equ qword [rbp + 16]

	push rbp
	mov rbp, rsp

	movzx rax, .IRQ
	cmp rax, 16
	jae .Error1

	add rax, $20
	push rax
	push 2
	push .Entry_point
	invoke IException, Install_ISR

	.Return:
	pop rbp
	ret 16

	Raise_error 1, 1, 4

	restore .IRQ
	restore .Entry_point

Function_Mask_all_IRQ: ; Function 2
	push rbp
	mov rsp, rbp

	mov al, 11111011b
	mov [Static.IRQMask1], al
	out $21, al
	mov al, 11111111b
	mov [Static.IRQMask2], al
	out $A1, al

	xor rax, rax

	.Return:
	pop rbp
	ret

Function_Enable_IRQ:
	.IRQ equ byte [rbp + 16]

	push rbp
	mov rsp, rbp

	movzx r11, .IRQ

	mov ax, word [Static.IRQMask1]
	btr ax, r11w
	mov word [Static.IRQMask1], ax

	out $21, al
	mov al, ah
	out $A1, al

	xor rax, rax

	.Return:
	pop rbp
	ret 8

	restore .IRQ

Function_Disable_IRQ:
	.IRQ equ byte [rbp + 16]

	push rbp
	mov rbp, rsp

	movzx r11, .IRQ

	mov ax, word [fs:ebx + Static.IRQMask1]
	bts ax, r11w
	mov word [fs:ebx + Static.IRQMask1], ax

	out $21, al
	mov al, ah
	out $A1, al

	xor rax, rax

	.Return:
	pop rbp
	ret 8

	restore .IRQ

Function_Send_EOI:
	.IRQ equ byte [rsp + 8]

	mov al, 100000b
	out $20, al

	cmp .IRQ, 8
	jb .Return

	out $A0, al

	.Return:
	ret 8

	restore .IRQ

Static:
	.IRQMask1 db 0
	.IRQMask2 db 0