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
; Function 2: Write (var Text : Array of Ansi_char; Count : Byte)
; Function 3: Write_line (var Text : Array of Ansi_char; Count : Byte)
; Function 4: Card64_to_hex (Num : Card64; out Hex_str : Array [16] of Ansi_char)
; Function 5: Card64_to_decimal (Num : Card64; out Dec_str : Array [20] of Ansi_char) : Card64;

jmp near Function_Init
dq Header
Interface:
	dq Function_Install_ISR
	dq Function_Write
	dq Function_Write_line

	dq Function_Card64_to_hex
	dq Function_Card64_to_decimal
Header:
	.Module_addr dq 0
	.Module_id dq 1, 0

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

	mov [Header.Module_addr], rbx
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
	.INT_num equ byte [rbp + 32] ; INT_num : Byte
	.Stack equ byte [rbp + 24] ; Stack : Byte
	.ISR_entry equ qword [rbp + 16] ; ISR_entry : Address

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
	ret 24

	.Error1:
	cli
	hlt

	restore .INT_num
	restore .ISR_entry
	restore .Stack

Function_Write: ; Function 2
	.Text equ qword [rbp + 24] ; var Text : Array of Ansi_char
	.Count equ byte [rbp + 16] ; Count : Byte

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

Function_Write_line: ; Function 3
	.Text equ [rbp + 24] ; var Text : Array of Ansi_char
	.Count equ [rbp + 16] ; Count : Byte

	push rbp
	mov rbp, rsp

	push qword .Text
	push qword .Count
	call Function_Write

	test rax, rax
	jnz .Return

	push rbx
	mov ax, [Static.Video_cursor]
	mov bl, 80
	div bl
	test ah, ah
	jz .Finish

	inc al
	mul bl
	mov [Static.Video_cursor], ax

	.Finish:
	pop rbx
	xor rax, rax

	.Return:
	pop rbp
	ret 16

	restore .Text
	restore .Count

Function_Card64_to_hex:
	.Num equ qword [rbp + 24] ; Num : Card64
	.Hex_str equ qword [rbp + 16] ; out Hex_str : Array [16] of Ansi_char

	push rbp
	mov rbp, rsp

	push rbx
	push rcx

	mov rax, .Num
	mov rbx, .Hex_str
	add rbx, 15
	mov cl, 16

	.Loop:
	mov ch, al
	and ch, $F

	cmp ch, $A
	jae .j1
	add ch, '0'
	jmp .j2
	.j1: add ch, 'A' - $A
	.j2: mov [rbx], ch
	dec rbx

	.Continue_loop:
	shr rax, 4
	dec cl
	jnz .Loop

	.Return:
	xor rax, rax

	pop rcx
	pop rbx

	pop rbp
	ret 16

	restore .Num
	restore .Hex_str

Function_Card64_to_decimal:
	.Num equ qword [rbp + 24] ; Num : Card64
	.Dec_str equ qword [rbp + 16] ; out Dec_str : Array [20] of Ansi_char

	push rbp
	mov rbp, rsp

	push rbx
	push rcx
	push rdx

	mov rbx, .Dec_str
	xor rcx, rcx
	.Loop1:
		mov [rbx + rcx], byte '0'
		inc cl
		cmp cl, 20
		jb .Loop1

	mov rax, .Num
	xor r15, r15
	add rbx, 19
	mov cl, 10
	xor rdx, rdx

	.Loop2:
	div rcx

	add dl, '0'
	mov [rbx], dl

	dec rbx
	inc r15

	test rax, rax
	jz .Return
	xor dl, dl
	jmp .Loop2

	.Return:
	xor rax, rax
	pop rdx
	pop rcx
	pop rbx

	pop rbp
	ret 16

	restore .Num
	restore .HexStr

Static:
	.Video_cursor dw 0