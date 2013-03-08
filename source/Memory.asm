; Memory.asm - Memory manager module
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

; IMemory
; Function 1: Install_ISR (INT_num, Stack : Byte; ISR_entry : Address)

jmp near Function_Init
dq Header
Interface:
	dq Function_Install_ISR
	dq Function_Write

Header:

Const:
	System_data = $10000
	IDT = $6000

Function_Init:
	push rbx
	push rdi
	push rsi

	mov rbx, rax
	lea rsi, [rax + Interface]
	mov rax, IException
	mov [rax], rbx
	mov [rax + 8], rsi

	xor rax, rax
	.Loop:
		add [rsi + rax], ebx
		add rax, 8
		cmp rax, Header - Interface
		jb .Loop

	mov rsi, IDT
	xor rax, rax
	.Loop2:
		mov [rsi + rax], dword $180000
		mov [rsi + rax + 4], dword $E00
		mov [rsi + rax + 8], dword 0
		mov [rsi + rax + 12], dword 0
		add rax, 16
		cmp rax, $1000
		jb .Loop2

	lidt [IDTR]

	pop rsi
	pop rdi
	pop rbx
	ret


IDTR:
	.Limit dw 4095
	.Base dq IDT

Function_Install_ISR: ; Function 1
	.INT_num equ byte [ebp + 25] ; INT_num : Byte
	.Stack equ byte [ebp + 24] ; Stack : Byte
	.ISR_entry equ qword [ebp + 16] ; ISR_entry : Address

	push rbp
	mov rbp, rsp
	push rbx

	mov al, .Stack
	cmp al, 7
	ja .Error1

	mov rbx, IDT
	movzx rax, .INT_num
	shl rax, 4
	add rbx, rax

	mov rax, .ISR_entry
	mov [rbx], ax
	shr rax, 16
	mov [rbx + 6], ax
	shr rax, 16
	mov [rbx + 8], eax

	bts word [rbx + 4], 15
	mov al, .Stack
	mov [rbx + 4], al

	xor rax, rax

	.Return:
	pop rbx
	pop rbp
	ret 16

	.Error1:
	cli
	hlt

	restore .INT_num
	restore .ISR_entry
	restore .Stack

Function_Write: ; Function 1
	.Text equ qword [ebp + 24] ; Text : Array of Ansi_char
	.Count equ byte [ebp + 16] ; Count : Byte

	push rbp
	mov rbp, rsp
	push rcx
	push rsi
	push rdi

	mov cl, .Count
	mov rsi, .Text
	movzx rdi, word [Static.Video_cursor]

	.Loop:
		mov al, [rsi]
		mov [$B8000 + rdi * 2], al
		mov [$B8000 + rdi * 2 + 1], byte 1111b

		inc rsi
		inc rdi
		cmp rdi, 2000
		jb .Next

		xor rdi, rdi

		.Next:
		dec cl
		jnz .Loop

	mov [Static.Video_cursor], di

	xor rax, rax

	.Return:
	pop rdi
	pop rsi
	pop rcx
	pop rbp
	ret 16

	restore .Text
	restore .Count

Static:
	.Video_cursor dw 0