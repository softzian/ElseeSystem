; Thread.asm - Memory manager module
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

; IModule
; Function 1: Register_module (Module_addr : Address; Module_id : Card128) : Card64
; Function 2: Find_module (Module_id : Card128) : Card64
; Function 3: Add_address_space (Module_handle : Card64) : Card64
; Function 4: Switch_address_space (Module_handle : Card64; Address_space_id : Card64)

jmp near Function_Init
dq Header
Interface:
	dq Function_Register_module
	dq Function_Find_module
Header:
	.Module_addr dq 0

Const:
	System_data = $10000

	Lvl4_page_table = $2000
	Window_page = $120000

	; MTE is Module table entry
	MTE_Flag = 0
	MTE_Module_addr = 8
	MTE_Module_id = 16
	MTE_Module_addr_space_table = 32
	MTE_Module_thread_space_table = 40

Function_Init:
	mov rbx, rax
	lea rsi, [rax + Interface]
	mov rax, IModule
	mov [rax], rbx
	mov [rax + 8], rsi
	mov [Header.Module_addr], rbx

	xor rax, rax
	.Loop:
		add [rsi + rax], rbx
		add rax, 8
		cmp rax, Header - Interface
		jb .Loop

	push 2
	invoke IMemory, Allocate

	test rax, rax
	jnz .Error1

	mov rsi, r15
	mov [Static.Module_table], r15

	; Clear module table
	xor rcx, rcx
	.Loop2:
		mov [rsi + rcx], dword 0
		add rcx, 4
		cmp rcx, $4000
		jb .Loop2

	; Create first entry for this module
	mov rax, 1
	mov [rsi + MTE_Flag], rax
	mov [rsi + MTE_Module_addr], rbx
	mov [rsi + MTE_Module_id], rax

	.Return:
	xor rax, rax
	ret

	.Error1:
	mov rax, Static.Text1
	add rax, [Header.Module_addr]
	push rax
	push 28
	invoke IException, Write
	cli
	hlt

Function_Register_module:
	.Module_addr equ qword [rbp + 32]
	.Module_id1 equ qword [rbp + 24]
	.Module_id2 equ qword [rbp + 16]

	push rbp
	mov rbp, rsp

	mov r13, .Module_addr
	test r13, $FFF
	jnz .Error1

	cmp r13, $7F00000000000000
	jae .Error1

	mov r11, [Static.Module_table]
	xor r12, r12
	.Find_free_entry:
		mov rax, [r11 + r12]
		test rax, rax
		jz .Found
		add r12, 64
		cmp r12, $4000
		jb .Find_free_entry

	.Found:
	mov rax, .Module_addr
	mov [r11 + r12 + MTE_Module_addr], r13
	mov rax, .Module_id1
	mov [r11 + r12 + MTE_Module_id1], rax
	mov rax, .Module_id2
	mov [r11 + r12 + MTE_Module_id + 8], rax

	xor rax, rax

	.Return:
	pop rbp
	ret 24

	.Error1:
	mov rax, 1
	Error_return

	restore .Module_addr
	restore .Module_id1
	restore .Module_id2

Function_Find_module:
	.Module_id1 equ qword [rbp + 24]
	.Module_id2 equ qword [rbp + 16]

	push rbp
	mov rbp, rsp

	mov r11, [Static.Module_table]
	xor r15, r15
	.Find_entry:
		mov rax, [r11 + r15]
		cmp rax, .Module_id1
		jne .Next

		mov rax, [r11 + r15 + 8]
		cmp rax, .Module_id2
		je .Found

		.Next:
		add r15, 64
		cmp r15, $4000
		jb .Find_entry
		jmp .Not_found

	.Found:
	shr r15, 6
	inc r15
	xor rax, rax

	.Return:
	pop rbp
	ret 16

	.Not_found:
	mov rax, 1
	jmp .Return

	restore .Module_id1
	restore .Module_id2

Static:
	.Module_table dq 0
	.Text1 db 'Cannot allocate module table'