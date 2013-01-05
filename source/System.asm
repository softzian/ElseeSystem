; System.asm - ISystem - Memory and Module Manager
; Written in 2012 by Congdm
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
	SizeOf_Region_Header = 4
	SizeOf_Memory_Entry = 12
	SizeOf_Memory_Table = (SizeOf_Data_Region - SizeOf_Region_Header)
	SizeOf_Data_Region = $4000

	SizeOf_Code_Header = 4
	SizeOf_Code_Entry = 12
	SizeOf_Code_Region = $4000
	SizeOf_Code_Table = (SizeOf_Code_Region - SizeOf_Code_Header)

	System_data = $4000	; Address of global system data block
	_Lock = 0
	GDT_address = 4
	GDT_size = 8
	Data_region_lock = 12
	Code_region_lock = 14
	DS_space = 24
	ES_space = 28

	GDT_lock = 0
	Module_table_lock = 1

Error_Code:
	REGION_SIZE_IS_NOT_LARGE_ENOUGH = -1
	NON_POSITIVE_SIZE = INVALID_SIZE
	MESSAGE_QUEUE_FULL = -2
	MESSAGE_QUEUE_EMPTY = -3

	CANNOT_ALLOCATE_MEMORY = -4
	NOT_EXISTED_MODULE = -5
	INVALID_GDT_ENTRY = -6
	NOT_EXISTED_ADDRESS_SPACE = -7
	CANNOT_ACCESS_ADDRESS_SPACE = -8
	NOT_FOUND = -9

use32

ISystem = $100000
; Function 1: Create_Region (Start, Limit : Address; Type : Cardinal)
; Function 2: Allocate (Size : Cardinal) : Address
; Function 3: Deallocate (Ptr : Address)
; Function 4: Mark_Memory (Start, Limit : Address; Module_Idx : Cardinal)

; Function 5: Create_Code_Region (Limit : Address)
; Function 6: Allocate_Code (Size : Cardinal; Type : Cardinal) : Address
; Function 7: Deallocate_Code (Ptr : Address)
; Function 8: Mark_Code (Start, Limit : Address; Type : Cardinal)

; Function 9: Copy_code_to_data (Src, Dst : Address; Count : Cardinal)
; Function 10: Copy_data_to_code (Src, Dst : Address; Count : Cardinal)

; Function 11: Register_Module (Module_id : Card128; Module_address : Address)
; Function 12: Set_module_thread_table (Module_address in ECX; Thread_table_address in EDI)
; Function 13: Get_module_thread_table (Module_address in ECX) return to ECX
; Function 14: Modify_GDT_entry (Entry_index in EDX; Base in EBX; Limit in ECX)
; Function 15: Save_GDT_entry (Entry_index in EDX) return to EBX and ECX
; Function 16: Load_GDT_entry (Entry_index in EDX; Entry_content in EBX and ECX)

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Create_Region
	dd Function_Allocate
	dd Function_Deallocate
	dd Function_Mark_Memory

	dd Function_Create_Code_Region
	dd Function_Allocate_Code
	dd Function_Deallocate_Code
	dd Function_Mark_Code

	dd Function_Copy_code_to_data
	dd Function_Copy_data_to_code

	dd Function_Register_Module
	dd Function_Set_module_thread_table
	dd Function_Get_module_thread_table
	dd Function_Modify_GDT_entry
	dd Function_Save_GDT_entry
	dd Function_Load_GDT_entry

	dd Function_Add_address_space
	dd Function_Get_address_space
	dd Function_Set_DS_space
	dd Function_Set_ES_space

	dd Function_Find_module

	dd Function_Install_ISR
Header:

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:ISystem], eax

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 22
		jb .Loop

	mov esi, Module_Table
	xor eax, eax
	.Loop2:
		mov [fs:esi + eax], dword 0
		add eax, 4
		cmp eax, SizeOf_Module_Table
		jb .Loop2

	mov esi, $7000
	xor eax, eax
	.Loop3:
		mov [fs:esi + eax], dword $80000
		mov [fs:esi + eax + 4], dword $E00
		add eax, 8
		cmp eax, $800
		jb .Loop3

	lidt [fs:ebx + IDTR]

	mov [gs:ebp], dword 2
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], dword ebx
	call Function_Register_Module

	pop esi
	pop edi
	pop ebx
	ret

include 'System_p1.inc' ; Function 1 to 4
include 'System_p2.inc' ; Function 5 to 8

Function_Copy_code_to_data:	; Function 9
	.Src equ dword [gs:ebp - 12]	; Src : Address
	.Dst equ dword [gs:ebp - 8]	; Dst : Address
	.Count equ dword [gs:ebp - 4]	; Count : Cardinal

	push ebp
	add ebp, 12
	push ecx
	push esi
	push edi

	mov edi, .Dst
	mov esi, .Src
	mov ecx, .Count

	.Loop:
		mov al, [fs:esi]
		mov [ds:edi], al
		inc esi
		inc edi
		loop .Loop

	.Return:
	pop edi
	pop esi
	pop ecx
	pop ebp
	ret

	restore .Src
	restore .Dst
	restore .Count

Function_Copy_data_to_code:	; Function 10
	.Src equ dword [gs:ebp - 12]	; Src : Address
	.Dst equ dword [gs:ebp - 8]	; Dst : Address
	.Count equ dword [gs:ebp - 4]	; Count : Cardinal

	push ebp
	add ebp, 12
	push ecx
	push esi
	push edi

	mov edi, .Dst
	mov esi, .Src
	mov ecx, .Count

	.Loop:
		mov al, [ds:esi]
		mov [fs:edi], al
		inc esi
		inc edi
		loop .Loop

	.Return:
	pop edi
	pop esi
	pop ecx
	pop ebp
	ret

	restore .Src
	restore .Dst
	restore .Count

include 'System_p3.inc' ; Function 11 to 21

Function_Install_ISR: ; Function 22
	.INum equ byte [gs:ebp - 5]
	.ISR_Entry equ dword [gs:ebp - 4]

	push ebp
	add ebp, 5
	push ebx

	mov ebx, $7000
	xor eax, eax
	mov al, .INum
	shl eax, 3
	add ebx, eax

	mov eax, .ISR_Entry
	mov [fs:ebx], ax
	mov byte [fs:ebx + 5], 10001110b
	shr eax, 16
	mov [fs:ebx + 6], ax

	xor eax, eax
	pop ebx

	pop ebp
	ret

	restore .INum
	restore .ISR_Entry

IDTR:
	.Limit dw 2047
	.Base dd $7000