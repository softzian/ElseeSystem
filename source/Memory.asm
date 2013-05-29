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
; Function 1: Allocate_16KB : Address
; Function 2: Free_16KB (Ptr : Address)
; Function 3: Trace_page_table (Vir_addr : Address)
; Function 4: New_address_space : Physical_address
; Function 5: Switch_address_space (Lvl3_table : Physical_address)
; Function 6: New_thread_stack : Physical_address
; Function 7: Switch_thread_stack (Lvl3_table : Physical_address)

jmp near Function_Init
dq Header
Interface:
	dq Function_Allocate_16KB
	dq Function_Free_16KB
	dq Function_Trace_page_table
	dq Function_New_address_space
	dq Function_Switch_address_space
	dq Function_New_thread_stack
	dq Function_Switch_thread_stack
Header:
	.Module_addr dq 0
	.Module_id dq 2, 0
Const:
	Lvl4_page_table = $2000
	First_page_directory_pointer_table = $3000
	First_page_directory = $4000
	First_page_table = $5000

	; Use 4K from $11000 for physical memory table (bitmap)
	Physical_memory_table = $11000

	; Memory pool for 16KB block is from $100004000 to $102000000 (~32 MB)
	_16KB_memory_pool_table = $7F8000000000

	Temp_page = $120000

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
	mov rax, [Header.Module_addr]
	add rax, Page_fault_handler
	push rax
	invoke IException, Install_ISR

Init_virtual_memory_allocator:
	mov rdx, _16KB_memory_pool_table
	mov rax, 1
	.Loop1:
		mov [rdx + rax * 2 - 2], ax
		inc rax
		cmp rax, 2048
		jbe .Loop1

	jmp Function_Init.Return

; ------------------------------------------------------------------------ ;
;                             PUBLIC FUNCTIONS                             ;
; ------------------------------------------------------------------------ ;

Function_Allocate_16KB: ; Function 1
	push rbp
	mov rbp, rsp

	mov r11, _16KB_memory_pool_table
	movzx rax, word [r11]

	movzx r12, word [r11 + rax * 2]
	mov word [r11], r12w

	shl rax, 14
	mov r13, $100000000
	lea r15, [rax + r13]

	xor rax, rax

	.Return:
	pop rbp
	ret

	Raise_error 1, 1, Header.Module_id

Function_Free_16KB: ; Function 2
	.Ptr equ [rbp + 16] ; Ptr : Address

	enter 0, 0
	push rcx

	mov rax, .Ptr
	mov r13, $100000000
	sub rax, r13
	shr rax, 14

	mov r11, _16KB_memory_pool_table
	mov r12w, word [r11]

	mov word [r11 + rax * 2], r12w
	mov word [r11], ax

	xor rcx, rcx
	.Loop1:
		mov rax, .Ptr
		add rax, rcx

		push rax
		call Get_phy_addr

		test rax, rax
		jz .Next1

		push r15
		call Free_physical_frame

		.Next1:
		add rcx, $1000
		cmp rcx, $4000
		jb .Loop1

	xor rax, rax

	.Return:
	pop rcx
	leave
	ret 8

	restore .Ptr

Function_Trace_page_table:	; Function 3
	.Vir_addr equ qword [rbp + 16]

	enter 0, 0
	push rbx
	push rcx

	mov rcx, .Vir_addr
	shr rcx, 12 + 9 * 3
	and rcx, $1FF

	mov r11, Lvl4_page_table

	mov rbx, [r11 + rcx * 8]

	push rbx
	push 0
	invoke IException, Card64_to_hex

	push 0
	push 16
	invoke IException, Write_line

	bt rbx, 0
	jnc .Return

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	mov r11, $FFFFFFFFFFE00000
	shl rcx, 12
	add r11, rcx

	mov rcx, .Vir_addr
	shr rcx, 12 + 9 * 2
	and rcx, $1FF

	lea rcx, [r11 + rcx * 8]
	mov rbx, [rcx]

	push rbx
	push 0
	invoke IException, Card64_to_hex

	push 0
	push 16
	invoke IException, Write_line

	bt rbx, 0
	jnc .Return

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	mov r11, $FFFFFFFFFFE00000
	sub rcx, r11
	shl rcx, 12 - 3
	mov r11, $FFFFFFFFC0000000
	add r11, rcx

	mov rcx, .Vir_addr
	shr rcx, 12 + 9
	and rcx, $1FF

	lea rcx, [r11 + rcx * 8]
	mov rbx, [rcx]

	push rbx
	push 0
	invoke IException, Card64_to_hex

	push 0
	push 16
	invoke IException, Write_line

	bt rbx, 0
	jnc .Return

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	mov r11, $FFFFFFFFC0000000
	sub rcx, r11
	shl rcx, 12 - 3
	mov r11, $FFFFFF8000000000
	add r11, rcx

	mov rcx, .Vir_addr
	shr rcx, 12
	and rcx, $1FF

	lea rcx, [r11 + rcx * 8]
	mov rbx, [rcx]

	push rbx
	push 0
	invoke IException, Card64_to_hex

	push 0
	push 16
	invoke IException, Write_line

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	.Return:
	pop rcx
	pop rbx
	leave
	ret 8

	restore .Vir_addr

Function_New_address_space:	; Function 4
	enter 0, 0

	call Allocate_physical_frame

	push r15

	push r15
	push Temp_page
	call Map_page_under_2MB

	mov rax, Temp_page
	.Clear:
		mov [rax], dword 0
		mov [rax + 4], dword 0
		add rax, 8
		cmp rax, Temp_page + $1000
		jb .Clear

	push Temp_page
	call Unmap_page

	pop r15

	.Return:
	leave
	ret

Function_Switch_address_space:	; Function 5
	.Lvl3_table equ qword [rbp + 16] ; Lvl3_table : Physical_address

	enter 0, 0

	mov r11, Lvl4_page_table
	mov rax, .Lvl3_table
	test rax, rax
	jz .Unmap_addr_space

	or rax, $3
	mov [r11 + 256 * 8], rax
	jmp .Finish

	.Unmap_addr_space:
	xor rax, rax
	mov [r11 + 256 * 8], rax

	.Finish:
	mov cr3, r11

	.Return:
	leave
	ret 8

	restore .Lvl3_table

Function_New_thread_stack: ; Function 6
	; Alloc memory for the lvl3 table of new stack
	; Map it to 258th entry in lvl4 table
	; Return the physical address of lvl3 table in r15

	enter 0, 0

	call Allocate_physical_frame

	or r15, 1
	mov rax, Lvl4_page_table
	mov [rax + 258 * 8], r15

	mov cr3, rax

	mov r11, $FFFFFFFFFFE00000 + 258 * $1000
	xor rax, rax
	.Clear_lvl3_table:
		mov [r11 + rax], dword 0
		mov [r11 + rax + 4], dword 0
		add rax, 8
		cmp rax, $1000
		jb .Clear_lvl3_table

	.Finish:
	and r15, MASK_12_LOW_BITS
	xor rax, rax

	.Return:
	leave
	ret

Function_Switch_thread_stack: ; Function 7
	; Map thread stack to 257th entry in lvl4 table

	.Lvl3_table equ qword [rsp + 8] ; Lvl3_table : Physical_address

	mov r12, [rsp]

	mov r11, Lvl4_page_table
	mov rax, .Lvl3_table
	test rax, rax
	jz .Unmap_stack

	or rax, $3
	mov [r11 + 257 * 8], rax
	jmp .Finish

	.Unmap_stack:
	xor rax, rax
	mov [r11 + 257 * 8], rax

	.Finish:
	mov cr3, r11

	.Return:
	jmp r12

	restore .Lvl3_table

; ------------------------------------------------------------------------ ;
;                             PRIVATE FUNCTIONS                            ;
; ------------------------------------------------------------------------ ;

Allocate_physical_frame:
	push rbp
	mov rbp, rsp

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
	pop rbp
	ret

	.Out_of_memory:
	mov rbx, [Header.Module_addr]
	add rbx, Static.Text2
	push rbx
	push 37
	invoke IException, Write
	cli
	hlt

Free_physical_frame:
	.Phy_frame equ qword [rbp + 16] ; Phy_frame : Physical_address

	push rbp
	mov rbp, rsp

	mov r13, .Phy_frame
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

	Raise_error 1, 4, Header.Module_id
	Raise_error 2, 4, Header.Module_id

	restore .Phy_frame

Map_page_under_2MB:
	.Phy_frame equ qword [rbp + 24] ; Phy_frame : Physical_address
	.Vir_page equ qword [rbp + 16] ; Vir_page : Address

	enter 0, 0

	mov rax, .Vir_page
	invlpg [rax]
	shr rax, 12
	and rax, $1FF
	mov r11, .Phy_frame
	and r11, MASK_12_LOW_BITS
	or r11, $101

	mov r12, First_page_table
	mov [r12 + rax * 8], r11

	.Return:
	leave
	ret 16

Map_page_in_lower_half:
	.Phy_frame equ qword [rbp + 24] ; Phy_frame : Physical_address
	.Vir_page equ qword [rbp + 16] ; Vir_page : Address

	enter 0, 0
	push rbx

	mov rbx, .Vir_page
	invlpg [rbx]

	; Get address of lvl 3 table
	mov rax, rbx
	shr rax, 12 + 9 * 3
	and rax, $1FF

	mov r11, Lvl4_page_table

	bt word [r11 + rax * 8], 0
	jnc .Alloc_lvl3_table

	mov r11, $FFFFFFFFFFE00000
	shl rax, 12
	add r11, rax

	; Get address of lvl 2 table
	mov rax, rbx
	shr rax, 12 + 9 * 2
	and rax, $1FF

	bt word [r11 + rax * 8], 0
	jnc .Alloc_lvl2_table

	lea rax, [r11 + rax * 8]
	mov r11, $FFFFFFFFFFE00000
	sub rax, r11
	shl rax, 12 - 3
	mov r11, $FFFFFFFFC0000000
	add r11, rax

	; Get address of lvl 1 table
	mov rax, rbx
	shr rax, 12 + 9
	and rax, $1FF

	bt word [r11 + rax * 8], 0
	jnc .Alloc_lvl1_table

	lea rax, [r11 + rax * 8]
	mov r11, $FFFFFFFFC0000000
	sub rax, r11
	shl rax, 12 - 3
	mov r11, $FFFFFF8000000000
	add r11, rax

	.Map_the_page:
	mov rax, rbx
	shr rax, 12
	and rax, $1FF

	mov r12, .Phy_frame
	and r12, MASK_12_LOW_BITS
	or r12, $101
	mov [r11 + rax * 8], r12

	.Return:
	pop rbx
	leave
	ret 16

	.Alloc_lvl3_table:
	call Allocate_physical_frame

	mov rax, rbx
	shr rax, 12 + 9 * 3
	and rax, $1FF

	mov r11, Lvl4_page_table

	or r15, $101
	mov [r11 + rax * 8], r15

	mov r11, $FFFFFFFFFFE00000
	shl rax, 12
	add r11, rax

	xor rax, rax
	.Clear_lvl3_table:
		mov [r11 + rax * 8], dword 0
		mov [r11 + rax * 8 + 4], dword 0
		inc rax
		cmp rax, $1000 / 8
		jb .Clear_lvl3_table

	.Alloc_lvl2_table:
	push r11

	call Allocate_physical_frame

	pop r11

	mov rax, rbx
	shr rax, 12 + 9 * 2
	and rax, $1FF

	or r15, $101
	mov [r11 + rax * 8], r15

	lea rax, [r11 + rax * 8]
	mov r11, $FFFFFFFFFFE00000
	sub rax, r11
	shl rax, 12 - 3
	mov r11, $FFFFFFFFC0000000
	add r11, rax

	xor rax, rax
	.Clear_lvl2_table:
		mov [r11 + rax * 8], dword 0
		mov [r11 + rax * 8 + 4], dword 0
		inc rax
		cmp rax, $1000 / 8
		jb .Clear_lvl2_table

	.Alloc_lvl1_table:
	push r11

	call Allocate_physical_frame

	pop r11

	mov rax, rbx
	shr rax, 12 + 9
	and rax, $1FF

	or r15, $101
	mov [r11 + rax * 8], r15

	lea rax, [r11 + rax * 8]
	mov r11, $FFFFFFFFC0000000
	sub rax, r11
	shl rax, 12 - 3
	mov r11, $FFFFFF8000000000
	add r11, rax

	xor rax, rax
	.Clear_lvl1_table:
		mov [r11 + rax * 8], dword 0
		mov [r11 + rax * 8 + 4], dword 0
		inc rax
		cmp rax, $1000 / 8
		jb .Clear_lvl1_table

	jmp .Map_the_page

	restore .Phy_frame
	restore .Vir_page

Map_page_in_process_address_space_and_thread_stack:
	.Phy_frame equ qword [rbp + 24] ; Phy_frame : Physical_address
	.Vir_page equ qword [rbp + 16] ; Vir_page : Address

	enter 0, 0
	push rbx

	mov rbx, .Vir_page
	invlpg [rbx]

	; Get address of lvl 3 table
	mov rax, rbx
	shr rax, 12 + 9 * 3
	and rax, $1FF

	mov r11, Lvl4_page_table

	mov r11, $FFFFFFFFFFE00000
	shl rax, 12
	add r11, rax

	; Get address of lvl 2 table
	mov rax, rbx
	shr rax, 12 + 9 * 2
	and rax, $1FF

	bt word [r11 + rax * 8], 0
	jnc .Alloc_lvl2_table

	lea rax, [r11 + rax * 8]
	mov r11, $FFFFFFFFFFE00000
	sub rax, r11
	shl rax, 12 - 3
	mov r11, $FFFFFFFFC0000000
	add r11, rax

	; Get address of lvl 1 table
	mov rax, rbx
	shr rax, 12 + 9
	and rax, $1FF

	bt word [r11 + rax * 8], 0
	jnc .Alloc_lvl1_table

	lea rax, [r11 + rax * 8]
	mov r11, $FFFFFFFFC0000000
	sub rax, r11
	shl rax, 12 - 3
	mov r11, $FFFFFF8000000000
	add r11, rax

	.Map_the_page:
	mov rax, rbx
	shr rax, 12
	and rax, $1FF

	mov r12, .Phy_frame
	and r12, MASK_12_LOW_BITS
	or r12, 3
	mov [r11 + rax * 8], r12

	.Return:
	pop rbx
	leave
	ret 16

	.Alloc_lvl2_table:
	push r11

	call Allocate_physical_frame

	pop r11

	mov rax, rbx
	shr rax, 12 + 9 * 2
	and rax, $1FF

	or r15, 3
	mov [r11 + rax * 8], r15

	lea rax, [r11 + rax * 8]
	mov r11, $FFFFFFFFFFE00000
	sub rax, r11
	shl rax, 12 - 3
	mov r11, $FFFFFFFFC0000000
	add r11, rax

	xor rax, rax
	.Clear_lvl2_table:
		mov [r11 + rax * 8], dword 0
		mov [r11 + rax * 8 + 4], dword 0
		inc rax
		cmp rax, $1000 / 8
		jb .Clear_lvl2_table

	.Alloc_lvl1_table:
	push r11

	call Allocate_physical_frame

	pop r11

	mov rax, rbx
	shr rax, 12 + 9
	and rax, $1FF

	or r15, 3
	mov [r11 + rax * 8], r15

	lea rax, [r11 + rax * 8]
	mov r11, $FFFFFFFFC0000000
	sub rax, r11
	shl rax, 12 - 3
	mov r11, $FFFFFF8000000000
	add r11, rax

	xor rax, rax
	.Clear_lvl1_table:
		mov [r11 + rax * 8], dword 0
		mov [r11 + rax * 8 + 4], dword 0
		inc rax
		cmp rax, $1000 / 8
		jb .Clear_lvl1_table

	jmp .Map_the_page

	restore .Phy_frame
	restore .Vir_page

Get_phy_addr:
	.Vir_addr equ qword [rbp + 16] ; Vir_addr : Address

	enter 0, 0

	mov rax, .Vir_addr

	mov r12, rax
	and r12, $FFF

	shl rax, 16
	shr rax, 16 + 12
	mov r11, $FFFFFF8000000000

	mov r15, [r11 + rax * 8]
	bt r15, 0
	jnc .Not_present

	and r15, MASK_12_LOW_BITS
	add r15, r12

	xor rax, rax

	.Return:
	leave
	ret 8

	.Not_present:
	mov rax, 1
	jmp .Return

	restore .Vir_addr

Unmap_page:
	.Vir_page equ qword [rbp + 16] ; Vir_addr : Address

	enter 0, 0

	mov rax, .Vir_page

	shl rax, 16
	shr rax, 16 + 12
	mov r11, $FFFFFF8000000000

	xor r12, r12
	mov [r11 + rax * 8], r12

	.Return:
	leave
	ret 8

	restore .Vir_page

; ------------------------------------------------------------------------ ;
;                             INTERRUPT HANDLER                            ;
; ------------------------------------------------------------------------ ;

Page_fault_handler:
	bt qword [rsp], 0
	jc .Halt

	Save_all_registers

	mov rbx, System_data
	inc qword [rbx + Page_fault_count]

	mov rcx, cr2
	and rcx, MASK_12_LOW_BITS
	cmp rcx, $200000
	jb .Alloc_under_2MB

	mov rax, $800000000000
	cmp rcx, rax
	jb .Alloc_lower_half

	jmp .Alloc_process_addr_space_or_stack

	.Alloc_under_2MB:
	call Allocate_physical_frame

	push r15
	push rcx
	call Map_page_under_2MB

	jmp .Return

	.Alloc_lower_half:
	call Allocate_physical_frame

	push r15
	push rcx
	call Map_page_in_lower_half

	jmp .Return

	.Alloc_process_addr_space_or_stack:
	mov rax, rcx
	shr rax, 12 + 9 * 3
	and rax, $1FF
	mov rdx, Lvl4_page_table
	bt qword [rdx + rax * 8], 0
	jnc .Halt

	call Allocate_physical_frame

	push r15
	push rcx
	call Map_page_in_process_address_space_and_thread_stack

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