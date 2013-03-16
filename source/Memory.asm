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
; Function 1: Allocate (Size : Card64) : Address
; Function 2: Free (Ptr : Address)

jmp near Function_Init
dq Header
Interface:
	dq Function_Allocate
	dq Function_Free
Header:
	.Module_addr dq 0
Const:
	System_data = $10000
	Total_RAM = 0
	Free_RAM = 8
	First_free_phy_page = 16
	Page_fault_count = 24

	Lvl4_page_table = $2000
	First_page_directory_pointer_table = $3000
	First_page_directory = $4000
	First_page_table = $5000

	; Use 4K from $11000 for physical memory table (bitmap)
	Physical_memory_table = $11000

	; We will reserved the area from 7F80 0000 0000 to 7FFF FFFF FFFF for
	; the virtual memory allocator's data (this allocator only manage lower half)
	; That means using the entire entry 255 of Lvl4 table
	Virtual_allocator = $7F8000000000

	; We used one virtual page ($120000) as a window to look at physical memory
	Window_page = $120000

Function_Init:
	mov rbx, rax
	lea rsi, [rax + Interface]
	mov rax, IMemory
	mov [rax], rbx
	mov [rax + 8], rsi
	mov [Header.Module_addr], rbx

	xor rax, rax
	.Loop:
		add [rsi + rax], rbx
		add rax, 8
		cmp rax, Header - Interface
		jb .Loop

	jmp Init_physical_allocator

	.Return:
	xor rax, rax
	ret

Init_physical_allocator:
	mov r8, System_data
	movzx rax, word [dword $F000]
	shr rax, 2
	mov [r8 + Total_RAM], rax
	mov [r8 + Free_RAM], rax
	xor rax, rax
	mov [r8 + Page_fault_count], rax

	xor rcx, rcx
	mov rsi, Physical_memory_table
	.Loop1:
		mov [rsi + rcx * 8], rax
		inc rcx
		cmp rcx, $1000 / 8
		jb .Loop1

	; Reserved the first $30000
	xor rcx, rcx
	.Loop2:
		bts qword [rsi], rcx
		inc rcx
		cmp rcx, $30
		jb .Loop2

	; Reserved the area between $9F000 and $120000
	mov rdx, $10
	mov rcx, $1F
	mov rax, $81
	.Loop3:
		bts qword [rsi + rdx], rcx
		inc rcx
		cmp rcx, $40
		jb .Next3

		xor rcx, rcx
		add rdx, 8

		.Next3:
		dec rax
		jnz .Loop3

	sub qword [r8 + Free_RAM], $B1

	push 14
	push 1
	mov rax, rbx
	add rax, Page_fault_handler
	push rax
	invoke IException, Install_ISR

Init_virtual_memory_allocator:
	; Preparing page tables for Virtual memory allocator area
	call Allocate_one_physical_page

	push r15
	mov rax, Virtual_allocator
	push rax
	call Map_one_page

	; Init data for Virtual memory allocator
	mov rdx, Virtual_allocator
	mov rax, $100000000
	mov [rdx], rax
	mov [rdx + 8], rax

	jmp Function_Init.Return

Allocate_one_physical_page:
	mov rax, System_data
	mov rax, [rax + Total_RAM]

	mov r11, Physical_memory_table
	xor r12, r12
	xor r13, r13
	.Find_free_page:
		bt qword [r11 + r12], r13
		jnc .Found

		inc r13
		cmp r13, $40
		jb .Next

		xor r13, r13
		add r12, 8

		.Next:
		dec rax
		jnz .Find_free_page
		jmp .Out_of_memory

	.Found:
	bts qword [r11 + r12], r13

	shl r12, 3
	add r12, r13
	mov r15, r12
	shl r15, 12

	mov rax, System_data
	dec qword [rax + Free_RAM]
	xor rax, rax

	.Return:
	ret

	.Out_of_memory:
	mov rbx, [Header.Module_addr]
	add rbx, Static.Text2
	push rbx
	push 37
	invoke IException, Write
	cli
	hlt

Free_one_physical_page:
	.Phy_page equ qword [rbp + 16] ; Phy_page : Physical_address

	push rbp
	mov rbp, rsp

	mov r13, .Phy_page
	test r13, $FFF
	jnz .Error1	; Invalid address

	shr r13, 12
	mov rax, System_data
	cmp r13, [rax + Total_RAM]
	jae .Error2	; Address > Max memory

	inc qword [rax + Free_RAM]

	mov r12, r13
	shr r12, 6
	shl r12, 6
	sub r13, r12
	shr r12, 3

	mov r11, Physical_memory_table
	btr qword [r11 + r12], r13

	xor rax, rax

	.Return:
	pop rbp
	ret 8

	.Error1:
	mov rax, 1
	Error_return

	.Error2:
	mov rax, 2
	Error_return

	restore .Phy_page

Map_one_page:
	.Phy_page equ qword [rbp + 24] ; Phy_page : Physical_address
	.Vir_page equ qword [rbp + 16] ; Vir_page : Virtual_address

	push rbp
	mov rbp, rsp
	push rbx
	push rcx

	mov rbx, .Vir_page
	test rbx, $FFF
	jnz .Error1	; Invalid address

	mov rcx, .Phy_page
	test rcx, $FFF
	jnz .Error1

	cmp rbx, $200000
	jae .Case2

	.Case1:
	shr rbx, 12 - 3
	or rcx, 1
	mov [First_page_table + rbx], rcx
	jmp .Finish

	.Case2:
	mov rax, Lvl4_page_table
	push rax
	mov rax, Window_page
	push rax
	call Map_one_page

	mov rax, rbx
	shr rax, 12 + 9 * 3
	and rax, $1FF

	push rax
	call Go_to_lower_page_table

	mov rax, rbx
	shr rax, 12 + 9 * 2
	and rax, $1FF

	push rax
	call Go_to_lower_page_table

	mov rax, rbx
	shr rax, 12 + 9 * 1
	and rax, $1FF

	push rax
	call Go_to_lower_page_table

	shr rbx, 12
	and rbx, $1FF
	or rcx, 1
	mov rax, Window_page

	mov [rax + rbx * 8], rcx

	.Finish:
	mov rax, .Vir_page
	invlpg [rax]
	xor rax, rax

	.Return:
	pop rcx
	pop rbx
	pop rbp
	ret 16

	.Error1:
	mov rax, -1
	Error_return

	restore .Phy_page
	restore .Vir_page

Get_phy_addr:
	.Vir_page equ qword [rbp + 16] ; Vir_page : Virtual_address

	push rbp
	mov rbp, rsp

	mov rax, .Vir_page
	test rax, $FFF
	jnz .Error1	; Invalid address

	cmp rax, $200000
	jae .Case2

	.Case1:
	shr rax, 12 - 3
	mov r15, [First_page_table + rax]
	and r15, MASK_12_LOW_BITS
	jmp .Finish

	.Case2:


	.Finish:
	xor rax, rax

	.Return:
	pop rbp
	ret 8

	.Error1:
	mov rax, -1
	Error_return

	restore .Vir_page

Go_to_lower_page_table:
	.Index equ qword [rbp + 16] ; Index : Card64

	push rbp
	mov rbp, rsp

	mov rax, .Index
	cmp rax, $1FF
	ja .Error1

	mov r11, Window_page
	bt qword [r11 + rax * 8], 0
	jnc .Create_page_table

	mov rax, [r11 + rax * 8]
	and rax, MASK_12_LOW_BITS

	push rax
	push r11
	call Map_one_page

	jmp .Finish

	.Create_page_table:
	call Allocate_one_physical_page

	or r15, 1
	mov r11, Window_page
	mov rax, .Index
	mov [r11 + rax * 8], r15

	btr r15, 0
	push r15
	push r11
	call Map_one_page

	mov r11, Window_page
	xor rax, rax
	.Clear_page_table:
		mov [r11 + rax], dword 0
		add rax, 4
		cmp rax, $1000
		jb .Clear_page_table

	.Finish:
	xor rax, rax

	.Return:
	pop rbp
	ret 8

	.Error1:
	mov rax, -1
	Error_return

	restore .Index

Function_Allocate:	; Size is in 4K block
	.Size equ [rbp + 16] ; Size : Card64

	push rbp
	mov rbp, rsp
	push rbx

	mov rbx, Virtual_allocator
	mov rax, .Size
	shl rax, 12

	mov r15, [rbx]
	add [rbx], rax
	xor rax, rax

	pop rbp
	ret 8

Function_Free:
	xor rax, rax
	ret 8

Page_fault_handler:
	bt qword [rsp], 0
	jc .Halt

	Save_all_registers

	mov rbx, System_data
	inc qword [rbx + Page_fault_count]

	call Allocate_one_physical_page

	push r15
	mov rax, cr2
	and rax, MASK_12_LOW_BITS
	push rax
	call Map_one_page

	.Return:
	Load_all_registers
	add rsp, 8
	iret

	.Halt:
	mov rbx, [Header.Module_addr]
	add rbx, Static.Text2
	push rbx
	push 17
	invoke IException, Write
	cli
	hlt

Static:
	.Text1 db 'Out of physical memory! Please reboot'
	.Text2 db 'Halt - Page fault'