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

	mov

	.Return:
	xor rax, rax
	ret

Static:
	.Text1 db 'Out of physical memory! Please reboot'
	.Text2 db 'Halt - Page fault'