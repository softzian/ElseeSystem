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
;               Module_address : Address
;               Thread_table : Address
;       End

Module_Table = $5000	; Address of level 1 module table
; Consts of level 2 table
SizeOf_Module_Entry = 32
NumOf_Module_Entry = 256
SizeOf_Module_Table = SizeOf_Module_Entry * NumOf_Module_Entry	; 8192 bytes
; Module_entry
Module_id = 0
Module_addr = 16
Module_thread_table = 20

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

	mov ecx, .Module_address
	mov eax, ecx
	shr eax, 20

	call Lock_module_table

	mov ebx, [fs:Module_Table + eax]
	test ebx, ebx
	jnz .Add_entry

	; Create level 2 table if it is not existed
	mov .Module_address, eax
	mov [gs:ebp], dword SizeOf_Module_Table
	call Function_Allocate

	test eax, eax
	mov eax, .Module_address
	jnz .Error1

	mov ebx, [ss:_Result]
	mov [fs:Module_Table + eax], ebx

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
	mov [fs:ebx + Module_addr], ecx
	mov [fs:ebx + Module_thread_table], eax

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

Get_module_entry:
	; ECX = Module address
	; Output to EBX - Module entry

	mov eax, ecx
	shr eax, 20

	mov ebx, [fs:Module_Table + eax]
	test ebx, ebx
	jz .Error ; Module not existed

	mov eax, ecx
	and eax, $000FFFFF
	shr eax, 12 - 5 ; div 4096 mul 32
	add ebx, eax

	cmp ecx, [fs:ebx + Module_addr]
	jne .Error

	xor al, al
	ret

	.Error:
	mov al, 1
	ret

Function_Set_module_thread_table:
	; ECX = Module_address
	; ESI = Thread_table_address

	push ebx

	call Get_module_entry
	test al, al
	jnz .Error1

	mov [fs:ebx + Module_thread_table], esi

	.Return:
	pop ebx
	ret

	.Error1:
	mov eax, NOT_EXISTED_MODULE
	ret

Function_Get_module_thread_table:
	; ECX = Module_address
	; Output to EBX = Thread_table_address

	call Get_module_entry
	test al, al
	jnz .Error1

	mov ebx, [fs:ebx + Module_thread_table]

	.Return:
	ret

	.Error1:
	mov eax, NOT_EXISTED_MODULE
	ret

Function_Modify_GDT_entry:
	; EBX = Base
	; ECX = Size
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

	shr ecx, 12
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