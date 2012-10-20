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

use32

ISystem = $100000
; Function 1: Create_Region (Start, Limit : Address; Type : Cardinal)
; Function 2: Allocate (Region : Address; var Ptr : Address; Size : Cardinal)
; Function 3: Deallocate (Region : Address; Ptr : Address)
; Function 4: Mark_Memory (Start, Limit : Address; Module_Idx : Cardinal)

; Function 5: Create_Code_Region (Limit : Address)
; Function 6: Allocate_Code (var Ptr : Address; Size : Cardinal; Type : Cardinal)
; Function 7: Deallocate_Code (Ptr : Address)
; Function 8: Mark_Code (Start, Limit : Address; Type : Cardinal)

; Function 9: Set_Active_Module (ModuleIdx : Cardinal)
; Function 10: Get_Active_Module (var ModuleIdx : Cardinal)

; Function 11: Create_Message_Queue (out Queue : Address; Size : Cardinal)
; Function 12: Send_Message (Queue : Address; in Msg : Message)
; Function 13: Get_Message (Queue : Address; out Msg : Message)

; Function 14: Copy_code_to_data (Src, Dst : Address; Count : Cardinal)

; Type
;       MemoryEntry_Type = Record
;               ModuleIdx : Cardinal
;               Start : Address
;               Size : Cardinal
;       End

Const:
	SizeOf_Region_Header = 4
	SizeOf_Memory_Entry = 12
	SizeOf_Memory_Table = ($4000 - SizeOf_Region_Header)
	Region_Address = $FFFFFFFF - $4000 + 1

	SizeOf_Code_Header = 4
	SizeOf_Code_Entry = 12
	SizeOf_Code_Region = $4000
	SizeOf_Code_Table = (SizeOf_Code_Region - SizeOf_Code_Header)

Error_Code:
	REGION_SIZE_IS_NOT_LARGE_ENOUGH = -1
	NON_POSITIVE_SIZE = INVALID_SIZE
	MESSAGE_QUEUE_FULL = -2
	MESSAGE_QUEUE_EMPTY = -3

jmp Function_Init
Interface:
	.Create_Region dd Function_Create_Region
	.Allocate dd Function_Allocate
	.Deallocate dd Function_Deallocate
	.Mark_Memory dd Function_Mark_Memory

	.Create_Code_Region dd Function_Create_Code_Region
	.Allocate_Code dd Function_Allocate_Code
	.Deallocate_Code dd Function_Deallocate_Code
	.Mark_Code dd Function_Mark_Code

	.Set_Active_Module dd Function_Set_Active_Module
	.Get_Active_Module dd Function_Get_Active_Module

	dd Function_Create_Message_Queue
	dd Function_Send_Message
	dd Function_Get_Message

	dd Function_Copy_code_to_data

Var:
	.Active_Module dd 0

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, ISystem
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, ISystem + 4 * 14
		jna .Loop

	pop esi
	pop edi
	pop ebx
	ret

Function_Create_Region:  ; Function 1
	.Start equ dword [ebp + 16]
	.Limit equ dword [ebp + 12]
	.Type equ dword [ebp + 8]

	push ebp
	mov ebp, esp

	push ebx
	push ecx

	mov eax, .Type
	test eax, eax
	jnz .Error1

	mov ebx, .Start

	cmp ebx, Region_Address
	jae .Error2	 ; Region size must be > $4000

	; Zero fill $4000 byte from $FFFFC000 to $FFFFFFFF
	xor ecx, ecx
	mov eax, Region_Address
	.Fill_zero:
	mov dword [eax + ecx * 8], 0
	mov dword [eax + ecx * 8 + 4], 0
	inc ecx
	cmp ecx, $800
	jb .Fill_zero

	; Create the first entry
	mov ecx, eax
	mov [eax + SizeOf_Region_Header + 4], ebx
	sub ecx, ebx
	mov [eax + SizeOf_Region_Header + 8], ecx

	xor eax, eax

	.Return:
	pop ecx
	pop ebx
	leave
	ret 12
	.Error1:
	mov eax, SUBINTERFACE_UNAVAILABLE
	jmp .Return
	.Error2:
	mov eax, REGION_SIZE_IS_NOT_LARGE_ENOUGH
	jmp .Return
	restore .Start
	restore .Limit
	restore .Type

Function_Allocate:    ; Function 2
	.Region equ dword [ebp + 16] ; Region : Address
	.Ptr equ dword [ebp + 12] ; var Ptr : Address
	.Size equ dword [ebp + 8] ; Size : Cardinal

	push ebp
	mov ebp, esp

	push ebx
	push ecx
	push edx
	push esi
	push edi

	cmp .Region, 0
	jne .Error1	; Region type is not 0

	; Step 1 - Find free entry with enough size (first-fit)
	xor ecx, ecx
	mov eax, .Size
	mov ebx, Region_Address + SizeOf_Region_Header

	.Loop1:
		cmp dword [ebx + ecx + 4], 0
		je .Error2

		cmp dword [ebx + ecx], 0
		jne .Next1

		mov esi, [ebx + ecx + 8]
		cmp eax, esi
		je .Alloc	; Jump to Step 3
		jb .Find_Best_Fit	; Jump to Step 2

		.Next1:
		add ecx, 12
		cmp ecx, SizeOf_Memory_Table
		jae .Error2
		jmp .Loop1

	; Step 2 - Find best fit entry
	.Find_Best_Fit:
	lea edx, [ecx + SizeOf_Memory_Entry]
	cmp edx, SizeOf_Memory_Table
	je .Alloc	; If no more entries to search, jump to Step 3

	.Loop2:
		cmp dword [ebx + edx + 4], 0
		je .Alloc

		cmp dword [ebx + edx], 0
		jne .Next2

		mov edi, [ebx + edx + 8]
		cmp eax, edi
		ja .Next2
		je .End_Loop2

		cmp esi, edi
		jbe .Next2
		mov ecx, edx
		mov esi, edi

		.Next2:
		add edx, SizeOf_Memory_Entry
		cmp edx, SizeOf_Memory_Table
		jae .Alloc
		jmp .Loop2
	.End_Loop2:
	mov ecx, edx

	; Step 3 - Allocate
	.Alloc:
	cmp eax, esi
	je .Done

	push eax
	call Function_Split_Entry

	.Done:
	lea eax, [ebx + ecx]
	push eax
	call Function_Get_Active_Module

	mov edx, .Ptr
	mov eax, [ebx + ecx + 4]
	mov [edx], eax
	xor eax, eax

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 12

	.Error1:
	mov eax, SUBINTERFACE_UNAVAILABLE
	jmp .Return
	.Error2:
	mov eax, NO_FREE_MEMORY
	jmp .Return
	restore .Ptr
	restore .Region
	restore .Size

Function_Split_Entry:
	; Step 1 - Find first blank entry after entry ecx
	mov edx, ecx
	.Loop1:
		cmp dword [ebx + edx + 4], 0
		je .Loop2
		add edx, SizeOf_Memory_Entry
		cmp edx, SizeOf_Memory_Table
		jb .Loop1
		jae .Not_Split

	; Step 2 - Move all the non-blank entries after entry ecx
	;          and copy ecx entry to the entry after it
	.Loop2:
		mov eax, [ebx + edx - SizeOf_Memory_Entry]
		mov [ebx + edx], eax
		mov eax, [ebx + edx - SizeOf_Memory_Entry + 4]
		mov [ebx + edx + 4], eax
		mov eax, [ebx + edx - SizeOf_Memory_Entry + 8]
		mov [ebx + edx + 8], eax

		sub edx, SizeOf_Memory_Entry
		cmp edx, ecx
		ja .Loop2

	; Step 3 - Update entries content
	mov eax, [esp + 4]
	mov [ebx + ecx + 8], eax
	sub [ebx + ecx + SizeOf_Memory_Entry + 8], eax
	add [ebx + ecx + SizeOf_Memory_Entry + 4], eax

	xor eax, eax
	.Return:
	ret 4
	.Not_Split:
	mov eax, 1
	jmp .Return

Function_Deallocate:	; Function 3
	.Region equ dword [ebp+12] ; Region : Address
	.Ptr equ dword [ebp+8] ; Ptr : Address

	push ebp
	mov ebp, esp

	push ebx
	push ecx
	push edx

	cmp .Region, 0
	jne .Error1	; This region isn't Region 0

	; Step 1 - Find Entry
	mov eax, .Ptr
	mov ebx, Region_Address + SizeOf_Region_Header
	xor ecx, ecx

	.Loop1:
		mov edx, [ebx + ecx + 4]
		test edx, edx
		jz .Not_Found
		cmp edx, eax
		je .Found

		add ecx, SizeOf_Memory_Entry
		cmp ecx, SizeOf_Memory_Table
		jb .Loop1
		jmp .Not_Found

	; Step 2 - Mark entry as free and Merge entries
	.Found:
	mov [ebx + ecx], dword 0
	call Function_Merge_Entry

	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx
	leave
	ret 8
	.Error1:
	mov eax, SUBINTERFACE_UNAVAILABLE
	jmp .Return
	.Not_Found:
	mov eax, ENTRY_NOT_FOUND
	jmp .Return
	restore .Ptr
	restore .Region

Function_Merge_Entry:
	; Step 1 - Check the previous entry
	xor edx, edx
	test ecx, ecx
	jz .Next

	mov eax, [ebx + ecx]
	cmp eax, [ebx + ecx - SizeOf_Memory_Entry]
	jne .Next

	sub ecx, SizeOf_Memory_Entry
	mov eax, [ebx + ecx + SizeOf_Memory_Entry + 8]
	add [ebx + ecx + 8], eax

	mov eax, [ebx + ecx + SizeOf_Memory_Entry * 2]
	mov [ebx + ecx + SizeOf_Memory_Entry], eax
	mov eax, [ebx + ecx + SizeOf_Memory_Entry * 2 + 4]
	mov [ebx + ecx + SizeOf_Memory_Entry + 4], eax
	mov eax, [ebx + ecx + SizeOf_Memory_Entry * 2 + 8]
	mov [ebx + ecx + SizeOf_Memory_Entry + 8], eax

	add edx, SizeOf_Memory_Entry

	; Step 2 - Check the subsequent entry
	.Next:
	lea eax, [ecx + SizeOf_Memory_Entry]
	cmp eax, SizeOf_Memory_Table
	je .Move

	mov eax, [ebx + eax]
	cmp eax, [ebx + ecx]
	jne .Move

	mov eax, [ebx + ecx + SizeOf_Memory_Entry + 8]
	add [ebx + ecx + 8], eax

	add edx, SizeOf_Memory_Entry

	; Step 3 - Do some moving if necessary
	.Move:
	test edx, edx
	jz .Return

	lea eax, [ecx + SizeOf_Memory_Entry + edx]
	cmp eax, SizeOf_Memory_Table
	jae .Finish

	mov ecx, eax
	neg edx
	add edx, ebx
	.Loop:
		cmp [ebx + ecx + 4], dword 0
		jne .a
		sub ecx, SizeOf_Memory_Entry
		jmp .End_Loop

		.a:
		mov eax, [ebx + ecx]
		mov [edx + ecx], eax
		mov eax, [ebx + ecx + 4]
		mov [edx + ecx + 4], eax
		mov eax, [ebx + ecx + 8]
		mov [edx + ecx + 8], eax

		add ecx, SizeOf_Memory_Entry
		cmp ecx, SizeOf_Memory_Table
		jb .Loop
	.End_Loop:
	sub edx, ebx
	add ecx, edx

	.Finish:
	xor eax, eax
	mov [ebx + ecx + SizeOf_Memory_Entry], eax
	mov [ebx + ecx + SizeOf_Memory_Entry + 4], eax
	mov [ebx + ecx + SizeOf_Memory_Entry + 8], eax

	cmp edx, -(SizeOf_Memory_Entry * 2)
	jne .Return

	mov [ebx + ecx + SizeOf_Memory_Entry * 2], eax
	mov [ebx + ecx + SizeOf_Memory_Entry * 2 + 4], eax
	mov [ebx + ecx + SizeOf_Memory_Entry * 2 + 8], eax

	.Return:
	ret

Function_Mark_Memory:	; Function 4
	.Start equ dword [ebp + 16]	  ; Offset : Address
	.Limit equ dword [ebp + 12]	  ; Limit : Address
	.Module_Idx equ dword [ebp + 8]   ; Module_Idx : Cardinal

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push edx
	push esi

	mov eax, .Limit
	mov edx, .Start
	cmp eax, edx
	jb .Error2	; Limit is below Start
	inc eax

	; Step 1 - Find entry
	mov ebx, Region_Address + SizeOf_Region_Header
	xor ecx, ecx

	.Loop1:
		mov esi, [ebx + ecx + 4]
		test esi, esi
		jz .Error3	; No more entry to find

		cmp [ebx + ecx], dword 0
		jne .Next1

		cmp esi, edx
		ja .Next1

		add esi, [ebx + ecx + 8]
		sub esi, eax
		jnc .Found

		.Next1:
		add ecx, SizeOf_Memory_Entry
		cmp ecx, SizeOf_Memory_Table
		jb .Loop1
		jmp .Error3	; No more entry to find

	; Step 2 - Split entry
	.Found:
	sub edx, [ebx + ecx + 4]
	jz .Next2

	push edx
	call Function_Split_Entry
	test eax, eax
	jnz .Error4
	add ecx, SizeOf_Memory_Entry

	.Next2:
	test esi, esi
	jz .Finish

	neg esi
	add esi, [ebx + ecx + 8]
	push esi
	call Function_Split_Entry

	.Finish:
	mov eax, .Module_Idx
	mov [ebx + ecx], eax
	xor eax, eax

	.Return:
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 12
	.Error2:
	mov eax, NON_POSITIVE_SIZE
	jmp .Return
	.Error3:
	mov eax, ENTRY_NOT_FOUND
	jmp .Return
	.Error4:
	mov eax, TABLE_FULL
	jmp .Return

	restore .Start
	restore .Limit
	restore .Module_Idx

Function_Create_Code_Region:  ; Function 5
	.Limit equ dword [ebp + 8]

	push ebp
	mov ebp, esp

	push ebx
	push ecx

	mov eax, .Limit
	cmp eax, SizeOf_Code_Region
	jb .Error2	; Region size must be > $4000

	xor ebx, ebx

	; Zero fill $4000 byte
	xor ecx, ecx
	.Fill_zero:
	mov dword [fs:ebx + ecx * 8], 0
	mov dword [fs:ebx + ecx * 8 + 4], 0
	inc ecx
	cmp ecx, SizeOf_Code_Region / 8
	jb .Fill_zero

	; Create the first entry
	lea ecx, [fs:ebx + SizeOf_Code_Region]
	mov [fs:ebx + SizeOf_Code_Header + 4], ecx
	sub eax, (SizeOf_Code_Region - 1)
	mov [fs:ebx + SizeOf_Code_Header + 8], eax

	xor eax, eax

	.Return:
	pop ecx
	pop ebx
	leave
	ret 4
	.Error2:
	mov eax, REGION_SIZE_IS_NOT_LARGE_ENOUGH
	jmp .Return
	restore .Limit

Function_Allocate_Code:    ; Function 6
	.Ptr equ dword [ebp + 16]	; var Ptr : Address
	.Size equ dword [ebp + 12]	; Size : Cardinal
	.Type equ dword [ebp + 8]	; Type : Cardinal

	push ebp
	mov ebp, esp

	push ebx
	push ecx
	push edx
	push esi
	push edi

	; Step 1 - Find free entry with enough size (first-fit)
	xor ecx, ecx
	mov eax, .Size
	mov ebx, SizeOf_Region_Header

	.Loop1:
		cmp dword [fs:ebx + ecx + 4], 0
		je .Error2

		cmp dword [fs:ebx + ecx], 0
		jne .Next1

		mov esi, [fs:ebx + ecx + 8]
		cmp eax, esi
		je .Alloc	; Jump to Step 3
		jb .Find_Best_Fit	; Jump to Step 2

		.Next1:
		add ecx, 12
		cmp ecx, SizeOf_Code_Table
		jae .Error2
		jmp .Loop1

	; Step 2 - Find best fit entry
	.Find_Best_Fit:
	lea edx, [fs:ecx + SizeOf_Code_Entry]
	cmp edx, SizeOf_Code_Table
	je .Alloc	; If no more entries to search, jump to Step 3

	.Loop2:
		cmp dword [fs:ebx + edx + 4], 0
		je .Alloc

		cmp dword [fs:ebx + edx], 0
		jne .Next2

		mov edi, [fs:ebx + edx + 8]
		cmp eax, edi
		ja .Next2
		je .End_Loop2

		cmp esi, edi
		jbe .Next2
		mov ecx, edx
		mov esi, edi

		.Next2:
		add edx, SizeOf_Code_Entry
		cmp edx, SizeOf_Code_Table
		jae .Alloc
		jmp .Loop2
	.End_Loop2:
	mov ecx, edx

	; Step 3 - Allocate
	.Alloc:
	cmp eax, esi
	je .Done

	push eax
	call Function_Split_Code_Entry

	.Done:
	mov eax, .Type
	mov [fs:ebx + ecx], eax
	mov edx, .Ptr
	mov eax, [fs:ebx + ecx + 4]
	mov [edx], eax
	xor eax, eax

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 12

	.Error2:
	mov eax, NO_FREE_MEMORY
	jmp .Return
	restore .Ptr
	restore .Size
	restore .Type

Function_Split_Code_Entry:
	; Step 1 - Find first blank entry after entry ecx
	mov edx, ecx
	.Loop1:
		cmp dword [fs:ebx + edx + 4], 0
		je .Loop2
		add edx, SizeOf_Code_Entry
		cmp edx, SizeOf_Code_Table
		jb .Loop1
		jae .Not_Split

	; Step 2 - Move all the non-blank entries after entry ecx
	;          and copy ecx entry to the entry after it
	.Loop2:
		mov eax, [fs:ebx + edx - SizeOf_Code_Entry]
		mov [fs:ebx + edx], eax
		mov eax, [fs:ebx + edx - SizeOf_Code_Entry + 4]
		mov [fs:ebx + edx + 4], eax
		mov eax, [fs:ebx + edx - SizeOf_Code_Entry + 8]
		mov [fs:ebx + edx + 8], eax

		sub edx, SizeOf_Code_Entry
		cmp edx, ecx
		ja .Loop2

	; Step 3 - Update entries content
	mov eax, [esp + 4]
	mov [fs:ebx + ecx + 8], eax
	sub [fs:ebx + ecx + SizeOf_Code_Entry + 8], eax
	add [fs:ebx + ecx + SizeOf_Code_Entry + 4], eax

	xor eax, eax
	.Return:
	ret 4
	.Not_Split:
	mov eax, 1
	jmp .Return

Function_Deallocate_Code:    ; Function 7
	.Ptr equ dword [ebp+8] ; Ptr : Address

	push ebp
	mov ebp, esp

	push ebx
	push ecx
	push edx

	; Step 1 - Find Entry
	mov eax, .Ptr
	mov ebx, SizeOf_Region_Header
	xor ecx, ecx

	.Loop1:
		mov edx, [fs:ebx + ecx + 4]
		test edx, edx
		jz .Not_Found
		cmp edx, eax
		je .Found

		add ecx, SizeOf_Code_Entry
		cmp ecx, SizeOf_Code_Table
		jb .Loop1
		jmp .Not_Found

	; Step 2 - Mark entry as free and Merge entries
	.Found:
	mov [fs:ebx + ecx], dword 0
	call Function_Merge_Code_Entry

	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx
	leave
	ret 4
	.Error1:
	mov eax, SUBINTERFACE_UNAVAILABLE
	jmp .Return
	.Not_Found:
	mov eax, ENTRY_NOT_FOUND
	jmp .Return
	restore .Ptr
	restore .Region

Function_Merge_Code_Entry:
	; Step 1 - Check the previous entry
	xor edx, edx
	test ecx, ecx
	jz .Next

	mov eax, [fs:ebx + ecx]
	cmp eax, [fs:ebx + ecx - SizeOf_Code_Entry]
	jne .Next

	sub ecx, SizeOf_Code_Entry
	mov eax, [fs:ebx + ecx + SizeOf_Code_Entry + 8]
	add [fs:ebx + ecx + 8], eax

	mov eax, [fs:ebx + ecx + SizeOf_Code_Entry * 2]
	mov [fs:ebx + ecx + SizeOf_Code_Entry], eax
	mov eax, [fs:ebx + ecx + SizeOf_Code_Entry * 2 + 4]
	mov [fs:ebx + ecx + SizeOf_Code_Entry + 4], eax
	mov eax, [fs:ebx + ecx + SizeOf_Code_Entry * 2 + 8]
	mov [fs:ebx + ecx + SizeOf_Code_Entry + 8], eax

	add edx, SizeOf_Code_Entry

	; Step 2 - Check the subsequent entry
	.Next:
	lea eax, [fs:ecx + SizeOf_Code_Entry]
	cmp eax, SizeOf_Code_Table
	je .Move

	mov eax, [fs:ebx + eax]
	cmp eax, [fs:ebx + ecx]
	jne .Move

	mov eax, [fs:ebx + ecx + SizeOf_Code_Entry + 8]
	add [fs:ebx + ecx + 8], eax

	add edx, SizeOf_Code_Entry

	; Step 3 - Do some moving if necessary
	.Move:
	test edx, edx
	jz .Return

	lea eax, [ecx + SizeOf_Code_Entry + edx]
	cmp eax, SizeOf_Code_Table
	jae .Finish

	mov ecx, eax
	neg edx
	add edx, ebx
	.Loop:
		cmp [fs:ebx + ecx + 4], dword 0
		jne .a
		sub ecx, SizeOf_Code_Entry
		jmp .End_Loop

		.a:
		mov eax, [fs:ebx + ecx]
		mov [fs:edx + ecx], eax
		mov eax, [fs:ebx + ecx + 4]
		mov [fs:edx + ecx + 4], eax
		mov eax, [fs:ebx + ecx + 8]
		mov [fs:edx + ecx + 8], eax

		add ecx, SizeOf_Code_Entry
		cmp ecx, SizeOf_Code_Table
		jb .Loop
		sub ecx, SizeOf_Code_Entry
	.End_Loop:
	sub edx, ebx
	add ecx, edx

	.Finish:
	xor eax, eax
	mov [fs:ebx + ecx + SizeOf_Code_Entry], eax
	mov [fs:ebx + ecx + SizeOf_Code_Entry + 4], eax
	mov [fs:ebx + ecx + SizeOf_Code_Entry + 8], eax

	cmp edx, -(SizeOf_Code_Entry * 2)
	jne .Return

	mov [fs:ebx + ecx + SizeOf_Code_Entry * 2], eax
	mov [fs:ebx + ecx + SizeOf_Code_Entry * 2 + 4], eax
	mov [fs:ebx + ecx + SizeOf_Code_Entry * 2 + 8], eax

	.Return:
	ret

Function_Mark_Code:   ; Function 8
	.Start equ dword [ebp + 16]	; Start : Address
	.Limit equ dword [ebp + 12]	; Limit : Address
	.Type equ dword [ebp + 8]	; Type : Cardinal

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push edx
	push esi

	mov eax, .Limit
	mov edx, .Start
	cmp eax, edx
	jb .Error2	; Limit is below Start
	inc eax

	; Step 1 - Find entry
	mov ebx, SizeOf_Code_Header
	xor ecx, ecx

	.Loop1:
		mov esi, [fs:ebx + ecx + 4]
		test esi, esi
		jz .Error3	; No more entry to find

		cmp [fs:ebx + ecx], dword 0
		jne .Next1

		cmp esi, edx
		ja .Next1

		add esi, [fs:ebx + ecx + 8]
		sub esi, eax
		jnc .Found

		.Next1:
		add ecx, SizeOf_Code_Entry
		cmp ecx, SizeOf_Code_Table
		jb .Loop1
		jmp .Error3	; No more entry to find

	; Step 2 - Split entry
	.Found:
	sub edx, [fs:ebx + ecx + 4]
	jz .Next2

	push edx
	call Function_Split_Code_Entry
	test eax, eax
	jnz .Error4
	add ecx, SizeOf_Code_Entry

	.Next2:
	test esi, esi
	jz .Finish

	neg esi
	add esi, [fs:ebx + ecx + 8]
	push esi
	call Function_Split_Code_Entry

	.Finish:
	mov eax, .Type
	mov [fs:ebx + ecx], eax
	xor eax, eax

	.Return:
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 8
	.Error2:
	mov eax, NON_POSITIVE_SIZE
	jmp .Return
	.Error3:
	mov eax, ENTRY_NOT_FOUND
	jmp .Return
	.Error4:
	mov eax, TABLE_FULL
	jmp .Return

	restore .Start
	restore .Limit
	restore .Type

Function_Set_Active_Module:	; Function 9
	.ModuleIdx equ dword [ebp + 8]	; ModuleIdx : Cardinal

	push ebp
	mov ebp, esp
	push ebx

	mov ebx, [fs:ISystem]

	mov eax, .ModuleIdx
	mov [fs:ebx + Var.Active_Module], eax

	xor eax, eax

	.Return:
	pop ebx
	leave
	ret 4
	restore .ModuleIdx

Function_Get_Active_Module:	; Function 10
	.ModuleIdx equ dword [ebp + 8]	; var ModuleIdx : Cardinal

	push ebp
	mov ebp, esp
	push ebx

	mov ebx, [cs:ISystem]
	mov eax, [cs:ebx + Var.Active_Module]
	mov ebx, .ModuleIdx
	mov [ebx], eax

	xor eax, eax
	.Return:
	pop ebx
	leave
	ret 4
	restore .ModuleIdx

Function_Create_Message_Queue:	; Function 11
	.Queue equ dword [ebp + 12]	; out Queue : Address
	.Size equ dword [ebp + 8]	; Size : Cardinal

	push ebp
	mov ebp, esp
	push ebx

	mov ebx, .Queue
	mov eax, .Size

	test eax, eax
	jz .Error1

	add eax, 2
	shl eax, 4	; Size of whole structure
	mov .Size, eax

	push 0
	push ebx
	push eax
	call Function_Allocate

	test eax, eax
	jnz .Error2

	mov ebx, [ebx]
	mov eax, .Size

	add eax, ebx
	mov [ebx], eax	; Limit
	mov [ebx + 4], dword 0	; Lock
	lea eax, [ebx + 16]
	mov [ebx + 8], eax	; Read pointer
	mov [ebx + 12], eax	; Write pointer

	xor eax, eax
	.Return:
	pop ebx
	leave
	ret 8
	.Error1:
	mov eax, INVALID_SIZE
	jmp .Return
	.Error2:
	mov eax, CANNOT_ALLOCATE
	jmp .Return

	restore .Queue
	restore .Size

Function_Send_Message:		; Function 12
	.Queue equ dword [ebp + 12]	; Queue : Address
	.Msg equ dword [ebp + 8]	; in Msg : Message

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push edx
	push esi

	mov ebx, .Queue

	print2 dword [ebx]
	print2 dword [ebx + 4]
	print2 dword [ebx + 8]
	print2 dword [ebx + 12]
	invoke IVideo.New_Line

	; Get lock
	.Spinlock:
		lock bts dword [ebx + 4], 0
		jnc .Next1
		invoke IThread.Yield
		jmp .Spinlock

	.Next1:
	mov eax, [ebx + 12]	; Write pointer

	add eax, 16
	cmp eax, [ebx]
	jb .Next2
	lea eax, [ebx + 16]

	.Next2:
	cmp eax, [ebx + 8]
	je .Error1		; Queue is full

	; Copy message to queue
	mov edx, [ebx + 12]
	mov esi, .Msg

	mov ecx, [esi]
	mov [edx], ecx
	mov ecx, [esi + 4]
	mov [edx + 4], ecx
	mov ecx, [esi + 8]
	mov [edx + 8], ecx
	mov ecx, [esi + 12]
	mov [edx + 12], ecx

	; Update write pointer
	mov [ebx + 12], eax

	xor eax, eax

	.Release_lock:
	lock btr dword [ebx + 4], 0
	.Return:
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 8

	.Error1:
	mov eax, MESSAGE_QUEUE_FULL
	jmp .Release_lock

	restore .Queue
	restore .Msg

Function_Get_Message:	       ; Function 13
	.Queue equ dword [ebp + 12]	; Queue : Address
	.Msg equ dword [ebp + 8]	; in Msg : Message

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push edi

	mov ebx, .Queue

	; Get lock
	.Spinlock:
		lock bts dword [ebx + 4], 0
		jnc .Next1
		invoke IThread.Yield
		jmp .Spinlock

	.Next1:
	mov eax, [ebx + 8]	; Read pointer

	cmp eax, [ebx + 12]
	je .Error1		; Queue is empty

	; Get message from queue
	mov edi, .Msg
	mov ecx, [eax]
	mov [edi], ecx
	mov ecx, [eax + 4]
	mov [edi + 4], ecx
	mov ecx, [eax + 8]
	mov [edi + 8], ecx
	mov ecx, [eax + 12]
	mov [edi + 12], ecx

	; Increase read pointer
	add eax, 16
	cmp eax, [ebx]
	jb .Finish
	lea eax, [ebx + 16]

	.Finish:
	mov [ebx + 8], eax
	xor eax, eax

	.Release_lock:
	lock btr dword [ebx + 4], 0
	.Return:
	pop edi
	pop ecx
	pop ebx
	leave
	ret 8

	.Error1:
	mov eax, MESSAGE_QUEUE_EMPTY
	jmp .Release_lock

	restore .Queue
	restore .Msg

Function_Copy_code_to_data:	; Function 14
	.Src equ dword [ebp + 16]	; Src : Address
	.Dst equ dword [ebp + 12]	; Dst : Address
	.Count equ dword [ebp + 8]	; Count : Cardinal

	push ebp
	mov ebp, esp
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
	leave
	ret 12

	restore .Src
	restore .Dst
	restore .Count
