; Module.asm - IModule - Module manager
; Written in 2013 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

include 'include/Header.inc'
include 'include/Errcode.inc'

Const:
	System_data = $4000	; Address of global system data block
	_Lock = 0
	GDT_address = 4
	GDT_size = 8
	Data_region_lock = 12
	Code_region_lock = 14
	DS_space = 24
	ES_space = 28
	LDT_address = 32

	GDT_lock = 0
	Module_table_lock = 1

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

Error_Code:
	CANNOT_ALLOCATE_MEMORY = -4
	NOT_EXISTED_MODULE = -5
	INVALID_GDT_ENTRY = -6
	NOT_EXISTED_ADDRESS_SPACE = -7
	CANNOT_ACCESS_ADDRESS_SPACE = -8
	NOT_FOUND = -9

use32

IModule = $100008
; Function 1: Register_Module (Module_id : Card128; Module_address : CS_address)
; Function 2: Find_module (Module_id : Card128) : CS_address

; Function 3: Set_module_thread_table (Module_address in ECX; Thread_table_address in EDI)
; Function 4: Get_module_thread_table (Module_address in ECX) return to ECX

; Function 5: Modify_GDT_entry (Entry_index in EDX; Base in EBX; Limit in ECX)
; Function 6: Save_GDT_entry (Entry_index in EDX) return to EBX and ECX
; Function 7: Load_GDT_entry (Entry_index in EDX; Entry_content in EBX and ECX)

; Function 8: Add_address_space (Base : Address; Limit : Card32) : Card32
; Function 9: Set_DS_space (Addr_space : Card32) // parameter pass over SS stack
; Function 10: Set_ES_space (Addr_space : Card32) // parameter pass over SS stack

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Register_Module
	dd Function_Find_module

	dd Function_Set_module_thread_table
	dd Function_Get_module_thread_table
	dd Function_Save_system_state
	dd Function_Load_system_state

	dd Function_Add_address_space
	dd Function_Set_DS_space
	dd Function_Set_ES_space
Header:

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:IModule], eax
	mov [fs:IModule + 4], esi

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 9
		jb .Loop

	mov esi, Module_Table
	xor eax, eax
	.Loop2:
		mov [fs:esi + eax], dword 0
		add eax, 4
		cmp eax, SizeOf_Module_Table
		jb .Loop2

	mov [gs:ebp], dword 3
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], dword ebx
	call Function_Register_Module

	pop esi
	pop edi
	pop ebx
	ret

Lock_module_table:
	lock bts dword [fs:System_data + _Lock], Module_table_lock
	jc .Wait
	ret
	.Wait:
	pause
	jmp Lock_module_table

macro Unlock_Module_table { btr dword [fs:System_data + _Lock], Module_table_lock }

Function_Register_Module: ; Function 1
	.Module_id_1 equ dword [gs:ebp - 20] ; Module_id : Card128
	.Module_id_2 equ dword [gs:ebp - 16]
	.Module_id_3 equ dword [gs:ebp - 12]
	.Module_id_4 equ dword [gs:ebp - 8]
	.Module_address equ dword [gs:ebp - 4] ; Module_address : Address

	push ebp
	add ebp, 20

	push ebx
	push ecx

	mov ecx, .Module_address
	mov eax, ecx
	shr eax, 20

	call Lock_module_table

	mov ebx, [fs:Module_Table + eax]
	test ebx, ebx
	jnz .Add_entry

	; Create level 2 table if it is not existed
	mov eax, [fs:IModule]
	mov [gs:ebp], dword SizeOf_Module_Table
	mov [gs:ebp + 4], eax
	invoke IMemory, IMemory.Allocate

	test eax, eax
	jnz .Error1

	mov ebx, [ss:_Result]
	mov eax, ecx
	shr eax, 20
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
	mov [fs:ebx + Module_address_space_table], eax

	.Return:
	Unlock_Module_table

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

Function_Find_module: ; Function 2
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

Function_Set_module_thread_table: ; Function 3
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

Function_Get_module_thread_table: ; Function 4
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

Function_Save_system_state: ; Function 5
	; ESI - Saving location
	mov eax, [fs:System_data + DS_space]
	mov [fs:esi], eax
	mov eax, [fs:System_data + ES_space]
	mov [fs:esi + 4], eax

	ret

Function_Load_system_state: ; Function 6
	; ESI - Loading location
	mov eax, [fs:esi]
	mov [fs:System_data + DS_space], eax
	mov eax, [fs:esi + 4]
	mov [fs:System_data + ES_space], eax

	mov ebx, [fs:System_data + GDT_address]

	add esi, 8
	mov [fs:System_data + LDT_address], esi

	mov eax, esi
	mov [fs:ebx + 3 * 8 + 2], ax
	shr eax, 16
	mov [fs:ebx + 3 * 8 + 4], al
	mov [fs:ebx + 3 * 8 + 7], ah

	mov ax, 3 * 8
	lldt ax
	mov eax, 4
	mov ds, ax
	add eax, 8
	mov es, ax
	add eax, 16
	mov gs, ax

	sub esi, 8
	ret

Function_Add_address_space: ; Function 8
	.Module_idx equ dword [gs:ebp - 12] ; Module_idx : Card32
	.Base equ dword [gs:ebp - 8] ; Base : Address
	.Limit equ dword [gs:ebp - 4] ; Limit : Card32

	push ebp
	add ebp, 12
	push ebx
	push ecx

	call Lock_module_table

	mov ecx, .Module_idx
	call Get_module_entry

	mov ebx, [fs:ecx + Module_address_space_table]
	test ebx, ebx
	jnz .Find_free_entry

	; Create address space table if it is not existed
	mov [gs:ebp], dword SizeOf_Address_space_table
	mov eax, .Module_idx
	mov [gs:ebp + 4], eax
	invoke IMemory, IMemory.Allocate

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

	.Next:
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
	add ecx, .Module_idx
	mov [ss:_Result], ecx

	.Done:
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

	restore .Module_idx
	restore .Base
	restore .Limit

Get_descriptor:
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

Function_Set_DS_space: ; Function 9
	.Addr_space equ dword [ss:esp + 12 + 4]

	push ebx
	push ecx
	push edx

	mov ecx, .Addr_space
	call Get_descriptor

	test eax, eax
	jnz .Error1

	mov eax, .Addr_space
	mov edx, [fs:System_data + DS_space]
	mov [fs:System_data + DS_space], eax
	mov .Addr_space, edx

	mov eax, [fs:System_data + LDT_address]
	mov [fs:eax], ecx
	mov [fs:eax + 4], ebx

	mov ax, 4
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

Function_Set_ES_space: ; Function 10
	.Addr_space equ dword [ss:esp + 12 + 4]

	push ebx
	push ecx
	push edx

	mov ecx, .Addr_space
	call Get_descriptor

	test eax, eax
	jnz .Error1

	mov eax, .Addr_space
	mov edx, [fs:System_data + ES_space]
	mov [fs:System_data + ES_space], eax
	mov .Addr_space, edx

	mov eax, [fs:System_data + LDT_address]
	mov [fs:eax + 8], ecx
	mov [fs:eax + 8 + 4], ebx

	mov ax, 8 + 4
	mov es, ax

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