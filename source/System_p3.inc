; System_p3.inc - Part 3 of System.asm
; Written in 2012 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

; Type
;       ModuleEntry_Type = Record
;               Module_id : Card128
;               Flag : Card32
;               Thread_table : Address
;               Address_space_table : Address
;       End
;
;       Address_space_entry = Record
;               Descriptor : Segment_descriptor
;               Lock : Card32
;               Semaphore : Card32
;       End


Module_Table = $5000	; Address of level 1 module table
; Consts of level 2 table
SizeOf_Module_Entry = 32
NumOf_Module_Entry = 256
SizeOf_Module_Table = SizeOf_Module_Entry * NumOf_Module_Entry	; 8192 bytes
SizeOf_Address_space_table = 16 * 256
; Module_entry
Module_id = 0
Module_flag = 16
Module_thread_table = 20
Module_address_space_table = 24

; Macro
macro Unlock_GDT { btr dword [fs:System_data + _Lock], GDT_lock }
macro Unlock_Module_table { btr dword [fs:System_data + _Lock], Module_table_lock }

Lock_module_table:
	lock bts dword [fs:System_data + _Lock], Module_table_lock
	jc .Wait
	ret
	.Wait:
	invoke IThread, IThread.Yield
	jmp Lock_module_table

Lock_GDT:
	lock bts dword [fs:System_data + _Lock], GDT_lock
	jc .Wait
	ret
	.Wait:
	invoke IThread, IThread.Yield
	jmp Lock_GDT

Function_Register_Module:
	.Module_id_1 equ dword [gs:ebp - 20] ; Module_id : Card128
	.Module_id_2 equ dword [gs:ebp - 16]
	.Module_id_3 equ dword [gs:ebp - 12]
	.Module_id_4 equ dword [gs:ebp - 8]
	.Module_address equ dword [gs:ebp - 4] ; Module_address : Address

	push ebp
	add ebp, 20

	push ebx
	push ecx
	push edx

	mov ecx, .Module_address
	mov eax, ecx
	shr eax, 20

	call Lock_module_table

	mov ebx, [fs:Module_Table + eax]
	test ebx, ebx
	jnz .Add_entry

	; Create level 2 table if it is not existed
	push dword [ss:_ModuleIdx]
	mov edx, [fs:ISystem]
	mov [ss:_ModuleIdx],  edx

	mov edx, eax
	mov [gs:ebp], dword SizeOf_Module_Table
	call Function_Allocate

	pop dword [ss:_ModuleIdx]

	test eax, eax
	mov eax, edx
	jnz .Error1

	mov ebx, [ss:_Result]
	mov [fs:Module_Table + eax], ebx

	xor eax, eax
	.Zero_fill:
		mov [fs:ebx + eax], byte 0
		inc eax
		cmp eax, SizeOf_Module_Table
		jb .Zero_fill

	.Add_entry:
	mov eax, ecx
	and eax, $000FFFFF
	shr eax, 12 - 5 ; div 4096 mul 32

	add ebx, eax
	mov eax, .Module_id_1
	mov [fs:ebx + Module_id], eax
	mov eax, .Module_id_2
	mov [fs:ebx + Module_id + 4], eax
	mov eax, .Module_id_3
	mov [fs:ebx + Module_id + 8], eax
	mov eax, .Module_id_4
	mov [fs:ebx + Module_id + 12], eax
	xor eax, eax
	mov [fs:ebx + Module_flag], dword 1
	mov [fs:ebx + Module_thread_table], eax

	.Return:
	Unlock_Module_table

	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, CANNOT_ALLOCATE_MEMORY
	jmp .Return

	restore .Module_address
	restore .Module_id_1
	restore .Module_id_2
	restore .Module_id_3
	restore .Module_id_4

Get_module_entry:
	; ECX = Module address
	; Output to ECX - Module entry

	mov eax, ecx
	shr eax, 20

	mov eax, [fs:Module_Table + eax]
	test eax, eax
	jz .Error ; Module not existed

	and ecx, $000FFFFF
	shr ecx, 12 - 5 ; div 4096 mul 32
	add ecx, eax

	cmp [fs:ecx + Module_flag], dword 1
	jne .Error

	xor al, al
	ret

	.Error:
	mov al, 1
	ret

Function_Set_module_thread_table:
	; ECX = Module_address
	; EDI = Thread_table_address

	call Get_module_entry
	test al, al
	jnz .Error1

	mov [fs:ecx + Module_thread_table], edi

	.Return:
	ret

	.Error1:
	mov eax, NOT_EXISTED_MODULE
	ret

Function_Get_module_thread_table:
	; ECX = Module_address
	; Output to ECX = Thread_table_address

	call Get_module_entry
	test al, al
	jnz .Error1

	mov ecx, [fs:ecx + Module_thread_table]

	.Return:
	ret

	.Error1:
	mov eax, NOT_EXISTED_MODULE
	ret

Function_Modify_GDT_entry:
	; EBX = Base
	; ECX = Limit
	; EDX = Entry

	test edx, edx
	jz .Error1

	mov eax, [fs:System_data + GDT_address]
	shl edx, 3
	add eax, edx
	mov [fs:eax + 2], bx
	shr ebx, 16
	mov [fs:eax + 4], bl
	mov [fs:eax + 7], bh

	and ecx, $FFFFF
	mov [fs:eax], cx
	shr ecx, 16
	add cl, 11000000b
	mov [fs:eax + 6], cl

	xor eax, eax

	.Return:
	ret

	.Error1:
	mov eax, INVALID_GDT_ENTRY
	ret

Function_Save_GDT_entry:
	; EDX = Entry
	; Output GDT entry to EBX and ECX

	test edx, edx
	jz .Error1

	mov eax, [fs:System_data + GDT_address]
	shl edx, 3
	add eax, edx

	mov ebx, [fs:eax + 4]
	mov ecx, [fs:eax]

	xor eax, eax

	.Return:
	ret

	.Error1:
	mov eax, INVALID_GDT_ENTRY
	ret

Function_Load_GDT_entry:
	; EDX = Entry
	; EBX and ECX = GDT entry

	test edx, edx
	jz .Error1

	mov eax, [fs:System_data + GDT_address]
	shl edx, 3
	add eax, edx

	mov [fs:eax + 4], ebx
	mov [fs:eax], ecx

	xor eax, eax

	.Return:
	ret

	.Error1:
	mov eax, INVALID_GDT_ENTRY
	ret

Function_Add_address_space:
	.Base equ dword [gs:ebp - 8]
	.Limit equ dword [gs:ebp - 4]

	push ebp
	add ebp, 8
	push ebx
	push ecx

	call Lock_module_table

	mov ecx, [ss:_ModuleIdx]
	call Get_module_entry

	mov ebx, [fs:ecx + Module_address_space_table]
	test ebx, ebx
	jnz .Find_free_entry

	; Create address space table if it is not existed
	mov [gs:ebp], dword SizeOf_Address_space_table
	call Function_Allocate

	test eax, eax
	jnz .Error1

	mov ebx, [ss:_Result]
	mov [fs:ecx + Module_address_space_table], ebx

	mov ecx, SizeOf_Address_space_table - 1
	.Zero_fill:
		mov [fs:ebx + ecx], byte 0
		loop .Zero_fill
	mov [fs:ebx], byte 0
	jmp .Found

	.Find_free_entry:
	xor ecx, ecx
	.Loop:
		cmp dword [fs:ebx + ecx + 8], 0
		je .Found
		add ecx, 16
		cmp ecx, SizeOf_Address_space_table
		jb .Loop

	.Found:
	add ebx, ecx

	mov eax, .Base
	mov [fs:ebx + 2], ax
	shr eax, 16
	mov [fs:ebx + 4], al
	mov [fs:ebx + 7], ah

	mov eax, .Limit
	and eax, $FFFFF
	mov [fs:ebx], ax
	shr eax, 16
	add al, 11000000b
	mov [fs:ebx + 6], al
	mov [fs:ebx + 5], byte 10010010b

	mov [fs:ebx + 12], dword 0
	mov [fs:ebx + 8], dword 1

	shr ecx, 4
	add ecx, [ss:_ModuleIdx]
	mov [ss:_Result], ecx
	xor eax, eax

	.Return:
	Unlock_Module_table

	pop ecx
	pop ebx
	pop ebp
	ret

	.Error1:
	mov eax, CANNOT_ALLOCATE_MEMORY
	jmp .Return

	restore .Base
	restore .Limit

Function_Get_descriptor:
	; ECX = Address_space index
	; Output to EBX and ECX = Descriptor

	test ecx, ecx
	jz .Zero_case

	mov ebx, ecx
	and ecx, $FFFFF000
	call Get_module_entry

	test al, al
	jnz .Error1

	mov eax, [fs:ecx + Module_address_space_table]
	test eax, eax
	jz .Error2

	and ebx, $FFF
	shl ebx, 4
	add eax, ebx

	mov ebx, [fs:eax + 4]
	mov ecx, [fs:eax]

	xor eax, eax
	ret

	.Zero_case:
	mov ecx, $F0000000
	mov ebx, $FFC090FF
	xor eax, eax
	ret

	.Error1:
	mov eax, NOT_EXISTED_MODULE
	ret

	.Error2:
	mov eax, NOT_EXISTED_ADDRESS_SPACE
	ret

Function_Get_address_space:
	mov ecx, [fs:System_data + DS_space + ecx * 4]
	ret

Function_Set_DS_space:
	.Addr_space equ dword [ss:esp + 12 + 4]

	push ebx
	push ecx
	push edx

	mov ecx, .Addr_space
	call Function_Get_descriptor

	test eax, eax
	jnz .Error1

	mov eax, .Addr_space
	mov edx, [fs:System_data + DS_space]
	mov [fs:System_data + DS_space], eax
	mov .Addr_space, edx

	mov edx, 3
	call Function_Load_GDT_entry
	mov ax, $3 * 8
	mov ds, ax

	.Finish:
	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx
	ret

	.Error1:
	mov eax, NOT_EXISTED_ADDRESS_SPACE
	jmp .Return

Function_Set_ES_space:
	.Addr_space equ dword [ss:esp + 12 + 4]

	push ebx
	push ecx
	push edx

	mov ecx, .Addr_space
	call Function_Get_descriptor

	test eax, eax
	jnz .Error1

	mov eax, .Addr_space
	mov edx, [fs:System_data + ES_space]
	mov [fs:System_data + ES_space], eax
	mov .Addr_space, edx

	mov edx, 4
	call Function_Load_GDT_entry
	mov ax, $4 * 8
	mov ds, ax

	.Finish:
	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx
	ret

	.Error1:
	mov eax, NOT_EXISTED_ADDRESS_SPACE
	jmp .Return

Function_Find_module:
	.Module_id_1 equ dword [gs:ebp - 16] ; Module_id : Card128
	.Module_id_2 equ dword [gs:ebp - 12]
	.Module_id_3 equ dword [gs:ebp - 8]
	.Module_id_4 equ dword [gs:ebp - 4]

	push ebp
	add ebp, 16

	push ebx
	push ecx
	push edx

	xor eax, eax
	.Loop1:
		mov ebx, [fs:Module_Table + eax]
		test ebx, ebx
		jz .Next1

		xor ecx, ecx
		.Loop2:
			cmp [fs:ebx + ecx + Module_flag], dword 1
			jne .Next2

			mov edx, [fs:ebx + ecx + Module_id]
			cmp edx, .Module_id_1
			jne .Next2
			mov edx, [fs:ebx + ecx + Module_id + 4]
			cmp edx, .Module_id_2
			jne .Next2
			mov edx, [fs:ebx + ecx + Module_id + 8]
			cmp edx, .Module_id_3
			jne .Next2
			mov edx, [fs:ebx + ecx + Module_id + 12]
			cmp edx, .Module_id_4
			je .Found

			.Next2:
			add ecx, SizeOf_Module_Entry
			cmp ecx, SizeOf_Module_Table
			jb .Loop2

		.Next1:
		add eax, 4
		cmp eax, $1000
		jb .Loop1
		jmp .Error1

	.Found:
	shl eax, 20 - 2
	shl ecx, 12 - 5
	add eax, ecx

	mov [ss:_Result], eax
	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, NOT_FOUND
	jmp .Return

	restore .Module_id_1
	restore .Module_id_2
	restore .Module_id_3
	restore .Module_id_4