; Excp.asm - Exception handling module
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

; IException
; Function 1: Install_ISR (INT_num, Stack : Byte; ISR_entry : Address)

jmp near Function_Init
dq Header
Interface:
	dq Function_Install_ISR

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
		cmp rax, 1 * 8
		jb .Loop

	mov rsi, IDT
	xor rax, rax
	.Loop2:
		mov [rsi + rax], dword $80000
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