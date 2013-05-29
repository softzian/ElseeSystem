; Module.asm - Memory manager module
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
; Function 3: New_address_space (Module_handle : Card64) : Card64
; Function 4: Switch_address_space (Module_handle : Card64; Address_space_id : Card64)
; Function 5: Get_thread_table (Module_handle : Card64) : Address
; Function 6: Set_thread_table (Module_handle : Card64; Thread_table : Address)

jmp near Function_Init
dq Header
Interface:
	dq Function_Register_module
	dq Function_Find_module
	dq Function_New_address_space
	dq Function_Switch_address_space
	dq Function_Get_thread_table
	dq Function_Set_thread_table
Header:
	.Module_addr dq 0
	.Module_id dq 3, 0

Const:
	System_data = $10000

	SizeOf_Module_table = $4000

	; MTE is Module table entry
	MTE_Flag = 0
	MTE_Module_addr = 8
	MTE_Module_id = 16
	MTE_Module_addr_space_table = 32
	MTE_Module_thread_table = 40

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

	invoke IMemory, Allocate_16KB

	mov rsi, r15
	mov [Static.Module_table], r15

	; Clear module table
	xor rcx, rcx
	.Loop2:
		mov [rsi + rcx], dword 0
		add rcx, 4
		cmp rcx, SizeOf_Module_table
		jb .Loop2

	; Create first entry for this module
	mov rax, 1
	mov [rsi + MTE_Flag], rax
	mov [rsi + MTE_Module_addr], rbx
	mov [rsi + MTE_Module_id], byte 3

	.Return:
	xor rax, rax
	ret

Function_Register_module: ; Function 1
	.Module_addr equ qword [rbp + 32]
	.Module_id1 equ qword [rbp + 24]
	.Module_id2 equ qword [rbp + 16]

	push rbp
	mov rbp, rsp

	mov r13, .Module_addr
	test r13, $FFF
	jnz .Error1

	mov rax, $7F00000000000000
	cmp r13, rax
	jae .Error1

	mov r11, [Static.Module_table]
	xor r12, r12
	.Find_free_entry:
		mov rax, [r11 + r12 + MTE_Flag]
		test rax, rax
		jz .Found
		add r12, 64
		cmp r12, SizeOf_Module_table
		jb .Find_free_entry

	.Found:
	inc rax
	mov [r11 + r12 + MTE_Flag], rax
	mov rax, .Module_addr
	mov [r11 + r12 + MTE_Module_addr], r13
	mov rax, .Module_id1
	mov [r11 + r12 + MTE_Module_id], rax
	mov rax, .Module_id2
	mov [r11 + r12 + MTE_Module_id + 8], rax
	xor rax, rax
	mov [r11 + r12 + MTE_Module_addr_space_table], rax
	mov [r11 + r12 + MTE_Module_thread_table], rax

	mov r15, r12
	shr r15, 6
	inc r15

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

Function_Find_module: ; Function 2
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
		cmp r15, SizeOf_Module_table
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

Function_New_address_space: ; Function 3
	.Module_handle equ qword [rbp + 16]

	push rbp
	mov rbp, rsp
	push rbx
	push rcx

	mov rbx, [Static.Module_table]
	mov rcx, .Module_handle

	test rcx, rcx
	jz .Error1

	cmp rcx, SizeOf_Module_table / 64
	ja .Error1

	dec rcx
	shl rcx, 6

	mov rax, [rbx + rcx + MTE_Module_addr]
	test rax, rax
	jz .Error5

	mov r15, [rbx + rcx + MTE_Module_addr_space_table]
	test r15, r15
	jnz .Continue

	.Create_table:
	invoke IMemory, Allocate_16KB

	test rax, rax
	jnz .Error2

	mov [rbx + rcx + MTE_Module_addr_space_table], r15

	.Continue:
	mov rbx, r15
	xor rcx, rcx

	.Find_free_entry:
		mov rax, [rbx + rcx]
		test rax, rax
		jz .Found
		add rcx, 8
		cmp rcx, $1000
		jb .Find_free_entry
		jmp .No_free

	.Found:
	invoke IMemory, New_address_space
	mov [rbx + rcx], r15

	shr rcx, 8
	inc rcx
	mov r15, rcx

	xor rax, rax

	.Return:
	pop rcx
	pop rbx
	pop rbp
	ret 8

	.No_free:
	mov rax, -1
	jmp .Return

	.Error1:
	mov rax, 1
	Error_return

	.Error2:
	mov rax, 2
	Error_return

	.Error3:
	mov rax, 3
	Error_return

	.Error4:
	mov rax, 4
	Error_return

	.Error5:
	mov rax, 5
	Error_return

	restore .Module_handle

Function_Switch_address_space: ; Function 4
	.Module_handle equ qword [rbp + 24]
	.Address_space_id equ qword [rbp + 16]

	push rbp
	mov rbp, rsp

	mov r11, [Static.Module_table]

	mov r12, .Module_handle
	test r12, r12
	jz .Error1

	cmp r12, SizeOf_Module_table / 64
	ja .Error1

	dec r12
	shl r12, 6

	mov rax, [r11 + r12 + MTE_Module_addr]
	test rax, rax
	jz .Error2

	mov r11, [r11 + r12 + MTE_Module_addr_space_table]
	test r11, r11
	jz .Error3

	mov r12, .Address_space_id

	cmp r12, $1000 / 8
	ja .Error4	; Address space id out of table range

	test r12, r12
	jnz .Switch_address_space      ; If Address space id = 0, that mean no address space to switch

	; In that case, unmap the current address space
	push 0
	invoke IMemory, Switch_address_space

	jmp .Return

	.Switch_address_space:
	dec r12
	shl r12, 3

	mov rax, [r11 + r12]
	test rax, rax
	jz .Error5

	push qword [r11 + r12]
	invoke IMemory, Switch_address_space

	xor rax, rax

	.Return:
	pop rbp
	ret 16

	Raise_error 1, 4, Header.Module_id
	Raise_error 2, 4, Header.Module_id
	Raise_error 3, 4, Header.Module_id
	Raise_error 4, 4, Header.Module_id
	Raise_error 5, 4, Header.Module_id

	restore .Module_handle
	restore .Address_space_id

Function_Get_thread_table:
	.Module_handle equ qword [rbp + 16]

	push rbp
	mov rbp, rsp

	mov r11, [Static.Module_table]
	mov r12, .Module_handle

	dec r12
	shl r12, 6

	mov r15, [r11 + r12 + MTE_Module_thread_table]

	test r15, r15
	jz .Not_exist

	xor rax, rax

	.Return:
	pop rbp
	ret 8

	.Not_exist:
	mov rax, -1
	jmp .Return

	restore .Module_handle

Function_Set_thread_table:
	.Module_handle equ qword [rbp + 24]
	.Thread_table equ qword [rbp + 16]

	push rbp
	mov rbp, rsp

	mov r11, [Static.Module_table]
	mov r12, .Module_handle

	dec r12
	shl r12, 6

	mov rax, .Thread_table
	mov [r11 + r12 + MTE_Module_thread_table], rax

	xor rax, rax

	.Return:
	pop rbp
	ret 16

	restore .Module_handle
	restore .Thread_table

Static:
	.Module_table dq 0