; Thread.asm - Threading module
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

; IThread
; Function 1: New_thread (Module, Addr_space : Card64; Entry_point : Address) : Card64
; Function 2: Start_thread (Module, Thread : Card64)
; Function 3: Enable_threading
; Function 4: Disable_threading

jmp near Function_Init
dq Header
Interface:
	dq Function_New_thread
	dq Function_Start_thread
	dq Function_Enable_threading
	dq Function_Disable_threading
Header:
	.Module_addr dq 0
	.Module_id dq 5, 0
Const:
	System_data = $10000

	Lvl4_page_table = $2000
	Window_page = $122000

	Thread_stack = $FFFFFF8000000000

	SizeOf_Thread_table = $1000
	SizeOf_TTE = 64
	TTE_State = 0
	TTE_Module = 8
	TTE_Addr_space = 16
	TTE_Lvl3_page_table = 24
	TTE_RSP = 32
	TTE_Next_thread = 40
	TTE_Prev_thread = 48

	RUNNING = 1
	BLOCKED = 2
	READY = 3

	ENABLE_THREADING_FLAG = 0

Function_Init:
	mov rbx, rax
	lea rsi, [rax + Interface]
	mov rax, IThread
	mov [rax], rbx
	mov [rax + 8], rsi
	mov [Header.Module_addr], rbx

	xor rax, rax
	.Loop:
		add [rsi + rax], rbx
		add rax, 8
		cmp rax, Header - Interface
		jb .Loop

	mov rax, Procedure_IRQ0_handler
	add rax, rbx
	push 0
	push rax
	invoke IPIC, Install_IRQ_handler

	push 0
	invoke IPIC, Enable_IRQ

	.Return:
	xor rax, rax
	ret

Function_New_thread:
	.Module equ qword [rbp + 32]
	.Addr_space equ qword [rbp + 24]
	.Entry_point equ qword [rbp + 16]

	.Result equ qword [rbp - 8]

	enter 8, 0
	push rbx

	bts word [Static.Lock], 0

	mov rbx, .Module

	push rbx
	invoke IModule, Get_thread_table

	test rax, rax
	jz .Find_free_entry

	.Create_thread_table:
	push SizeOf_Thread_table / $1000
	invoke IMemory, Allocate

	push r15

	push rbx
	push r15
	invoke IModule, Set_thread_table

	pop r15
	xor rax, rax
	xor r11, r11
	.Clear_thread_table:
		mov [r15 + r11], rax
		add r11, 8
		cmp r11, SizeOf_Thread_table
		jb .Clear_thread_table

	.Find_free_entry:
	xor r11, r11
	.Loop1:
		mov rax, [r15 + r11 + TTE_State]
		test rax, rax
		jz .Found
		add r11, SizeOf_TTE
		cmp r11, SizeOf_Thread_table
		jb .Loop1
		jmp .Error1

	; Found a free entry, next is creating a new thread
	.Found:
	mov [r15 + r11 + TTE_Module], rbx
	mov rax, BLOCKED
	mov [r15 + r11 + TTE_State], rax
	lea rbx, [r15 + r11]
	mov .Result, r11

	mov rax, .Addr_space
	mov [rbx + TTE_Addr_space], rax

	invoke IMemory, Allocate_physical_page

	mov [rbx + TTE_Lvl3_page_table], r15

	push r15
	push Window_page
	push $100
	invoke IMemory, Map_one_page

	xor r11, r11
	.Clear_lvl3_page_table:
		mov [Window_page + r11], dword 0
		add r11, 4
		cmp r11, $1000
		jb .Clear_lvl3_page_table

	invoke IMemory, Allocate_physical_page

	mov rax, r15
	or rax, 3
	mov r11, Window_page
	mov [r11 + 511 * 8], rax

	push r15
	push r11
	push $100
	invoke IMemory, Map_one_page

	xor r11, r11
	.Clear_lvl2_page_table:
		mov [Window_page + r11], dword 0
		add r11, 4
		cmp r11, $1000
		jb .Clear_lvl2_page_table

	invoke IMemory, Allocate_physical_page

	mov rax, r15
	or rax, 3
	mov r11, Window_page
	mov [r11 + 511 * 8], rax

	push r15
	push r11
	push $100
	invoke IMemory, Map_one_page

	xor r11, r11
	.Clear_lvl1_page_table:
		mov [Window_page + r11], dword 0
		add r11, 4
		cmp r11, $1000
		jb .Clear_lvl1_page_table

	invoke IMemory, Allocate_physical_page

	mov rax, r15
	or rax, 3
	mov r11, Window_page
	mov [r11 + 511 * 8], rax

	push r15
	push r11
	push $100
	invoke IMemory, Map_one_page

	mov r11, Window_page + $FF8
	mov rax, .Entry_point
	mov [r11], rax
	mov r12, 15
	xor rax, rax
	.Loop2:
		sub r11, 8
		mov [r11], rax
		dec r12
		jnz .Loop2

	mov rax, $FFFFFFFFFFFFFF80
	mov [rbx + TTE_RSP], rax

	xor rax, rax
	mov r15, .Result
	shr r15, 6
	inc r15

	btr word [Static.Lock], 0

	.Return:
	pop rbx
	leave
	ret 24

	Raise_error 1, 1, Header.Module_id

	restore .Module
	restore .Addr_space
	restore .Entry_point
	restore .Result

Function_Start_thread:
	.Module equ qword [rbp + 24]
	.Thread equ qword [rbp + 16]

	enter 0, 0

	mov rax, .Module
	push rax
	invoke IModule, Get_thread_table

	mov r11, .Thread
	dec r11
	shl r11, 6
	add r11, r15

	mov rax, READY
	mov [r11 + TTE_State], rax

	mov rax, [Static.First_thread]
	test rax, rax
	jz .First_thread_case

	mov r12, [Static.Last_thread]
	mov [r12 + TTE_Next_thread], r11
	mov [r11 + TTE_Prev_thread], r12
	mov [Static.Last_thread], r11

	mov r12, [Static.First_thread]
	mov [r12 + TTE_Prev_thread], r11
	mov [r11 + TTE_Next_thread], r12
	jmp .Finish

	.First_thread_case:
	mov [Static.First_thread], r11
	mov [Static.Last_thread], r11
	mov [r11 + TTE_Prev_thread], r11
	mov [r11 + TTE_Next_thread], r11

	.Finish:
	xor rax, rax

	.Return:
	leave
	ret 16

	restore .Module
	restore .Thread

Function_Enable_threading:
	bts qword [Static.Flag], ENABLE_THREADING_FLAG
	ret

Function_Disable_threading:
	btr qword [Static.Flag], ENABLE_THREADING_FLAG
	ret

; ------------------------------------------------------------------------ ;
;                             PRIVATE FUNCTIONS                            ;
; ------------------------------------------------------------------------ ;

Switch_thread:
	mov rax, [Static.Next_thread_to_be_run]
	mov [Static.Current_thread], rax

	push qword [Static.RIP]
	push rbp
	push qword [Static.RAX]
	push rbx
	push rcx
	push rdx
	push rsi
	push rdi
	push r8
	push r9
	push r10
	push qword [Static.R11]
	push qword [Static.R12]
	push qword [Static.R13]
	push r14
	push qword [Static.R15]
	mov [rax + TTE_RSP], rsp

	mov rbx, rax
	push qword [rbx + TTE_Module]
	push qword [rbx + TTE_Addr_space]
	invoke IModule, Switch_address_space

	mov rcx, Lvl4_page_table
	mov rax, [rbx + TTE_Lvl3_page_table]
	or rax, 3
	mov [rcx + 511 * 8], rax

	mov cr3, rcx

	mov rsp, [rbx + TTE_RSP]
	Load_all_registers
	ret

; ------------------------------------------------------------------------ ;
;                             INTERRUPT HANDLER                            ;
; ------------------------------------------------------------------------ ;

Procedure_IRQ0_handler:
	; Save registers
	mov [Static.RAX], rax
	mov [Static.R11], r11
	mov [Static.R12], r12
	mov [Static.R13], r13
	mov [Static.R15], r15

	bt word [Static.Lock], 0
	jc .Return_failed

	bt qword [Static.Flag], ENABLE_THREADING_FLAG
	jnc .Return_failed

	; Check if current thread is existed
	mov r11, [Static.Current_thread]
	test r11, r11
	jz .Case_2

	.Case_1:	; If current thread is existed
	mov r12, [r11 + TTE_Next_thread]
	mov rax, READY
	.Find_ready_thread:
		cmp r12, r11
		je .Return_failed ; Can not found any ready thread
		cmp [r12 + TTE_State], rax
		je .Found
		mov r12, [r12 + TTE_Next_thread]
		jmp .Find_ready_thread

	.Found:
	mov rax, [rsp]
	mov [Static.RIP], rax
	mov rax, [Header.Module_addr]
	add rax, Switch_thread
	mov [rsp], rax
	mov [Static.Next_thread_to_be_run], r12

	push 0
	invoke IPIC, Send_EOI

	iret

	; This case happen only in first run
	.Case_2:
	mov rax, [Static.First_thread]
	test rax, rax
	jz .Return_failed

	mov rbx, rax
	mov [Static.Current_thread], rax

	mov rax, .Start_first_thread
	add rax, [Header.Module_addr]
	mov [rsp], rax

	push 0
	invoke IPIC, Send_EOI
	iret

	.Start_first_thread:
	push qword [rbx + TTE_Module]
	push qword [rbx + TTE_Addr_space]
	invoke IModule, Switch_address_space

	mov rcx, Lvl4_page_table
	mov rax, [rbx + TTE_Lvl3_page_table]
	or rax, 3
	mov [rcx + 511 * 8], rax
	mov cr3, rcx

	mov rsp, [rbx + TTE_RSP]
	Load_all_registers
	ret

	.Return_failed:
	push 0
	invoke IPIC, Send_EOI

	mov rax, [Static.RAX]
	mov r11, [Static.R11]
	mov r12, [Static.R12]
	mov r13, [Static.R13]
	mov r15, [Static.R15]

	iret

Static:
	.Lock dw 0
	.Flag dq 0
	.First_thread dq 0
	.Last_thread dq 0
	.Current_thread dq 0
	.RIP dq 0
	.RAX dq 0
	.R11 dq 0
	.R12 dq 0
	.R13 dq 0
	.R15 dq 0
	.Next_thread_to_be_run dq 0