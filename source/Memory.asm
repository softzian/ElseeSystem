; Memory.asm - IMemory - Memory manager
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
	SizeOf_Region_Header = 4
	SizeOf_Memory_Entry = 12
	SizeOf_Memory_Table = (SizeOf_Data_Region - SizeOf_Region_Header)
	SizeOf_Data_Region = $4000

	SizeOf_Code_Header = 4
	SizeOf_Code_Entry = 12
	SizeOf_Code_Region = $4000
	SizeOf_Code_Table = (SizeOf_Code_Region - SizeOf_Code_Header)

	System_data = $4000
	Data_region_lock = 12
	Code_region_lock = 14

	Page_directory = $13000
	First_page_table = $14000
	Bitmap_16M = $15000

Error_Code:
	REGION_SIZE_IS_NOT_LARGE_ENOUGH = -1
	NON_POSITIVE_SIZE = INVALID_SIZE

use32

IMemory = $100000
; Function 1: Create_Region (Start, Limit : Address; Type : Cardinal)
; Function 2: Allocate (Size : Cardinal) : Address
; Function 3: Deallocate (Ptr : Address)
; Function 4: Mark_Memory (Start, Limit : Address; Module_Idx : Cardinal)

; Function 5: Create_Code_Region (Limit : Address)
; Function 6: Allocate_Code (Size : Cardinal; Type : Cardinal) : Address
; Function 7: Deallocate_Code (Ptr : Address)
; Function 8: Mark_Code (Start, Limit : Address; Type : Cardinal)

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
Header:

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:IMemory], eax
	mov [fs:IMemory + 4], esi

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 8
		jb .Loop

	mov [fs:System_data + Data_region_lock], word 0
	mov [fs:System_data + Code_region_lock], word 0

	mov eax, [fs:$1000]
	mov [fs:$19000], eax

	call Setup_paging

	pop esi
	pop edi
	pop ebx
	ret

Get_lock_0:
	lock bts word [fs:System_data + Data_region_lock], 0
	jc .Wait
	ret
	.Wait:
	pause
	jmp Get_lock_0

Release_lock_0:
	btr word [fs:System_data + Data_region_lock], 0
	ret

Function_Create_Region:  ; Function 1
	.Start equ dword [gs:ebp - 8]
	.Limit equ dword [gs:ebp - 4]

	push ebp
	add ebp, 8
	push ebx
	push ecx

	mov ebx, .Start
	mov eax, .Limit
	cmp eax, ebx
	jbe .Error2

	sub eax, ebx
	cmp eax, SizeOf_Data_Region
	jb .Error2	; Region size must be > $4000

	; Zero fill $4000 bytes
	xor ecx, ecx
	.Fill_zero:
		mov dword [fs:$A000 + ecx], 0
		mov dword [fs:$A000 + ecx + 4], 0
		add ecx, 8
		cmp ecx, SizeOf_Data_Region
		jb .Fill_zero

	; Create the first entry
	inc eax
	mov [fs:$A000 + SizeOf_Region_Header + 4], ebx
	mov [fs:$A000 + SizeOf_Region_Header + 8], eax

	xor eax, eax

	.Return:
	pop ecx
	pop ebx
	pop ebp
	ret

	.Error2:
	mov eax, REGION_SIZE_IS_NOT_LARGE_ENOUGH
	jmp .Return

	restore .Start
	restore .Limit

Function_Allocate:    ; Function 2
	.Size equ dword [gs:ebp - 8] ; Size : Card32
	.Module_idx equ dword [gs:ebp - 4] ; Module_idx : Card32

	push ebp
	add ebp, 8

	push ebx
	push ecx
	push edx
	push esi
	push edi

	call Get_lock_0

	; Step 1 - Find free entry with enough size (first-fit)
	.Step1:
	xor ecx, ecx
	mov eax, .Size
	mov ebx, $A000
	add ebx, SizeOf_Region_Header

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
		cmp ecx, SizeOf_Memory_Table
		jae .Error2
		jmp .Loop1

	; Step 2 - Find best fit entry
	.Find_Best_Fit:
	lea edx, [ecx + SizeOf_Memory_Entry]
	cmp edx, SizeOf_Memory_Table
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

	mov [gs:ebp], eax	; EAX is the size we want to split
	call Function_Split_Entry

	.Done:
	mov eax, .Module_idx ; Active module address
	mov [fs:ebx + ecx], eax

	; Return new pointer
	mov eax, [fs:ebx + ecx + 4]
	mov [ss:_Result], eax
	xor eax, eax

	.Return:
	call Release_lock_0

	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error2:
	mov eax, NO_FREE_MEMORY
	jmp .Return

	restore .Size
	restore .Module_idx

Function_Split_Entry:	; Parameter is the size we want to split
	push ebp
	add ebp, 4

	; Step 1 - Find first blank entry after entry ecx
	mov edx, ecx
	.Loop1:
		cmp dword [fs:ebx + edx + 4], 0
		je .Loop2
		add edx, SizeOf_Memory_Entry
		cmp edx, SizeOf_Memory_Table
		jb .Loop1
		jae .Not_Split

	; Step 2 - Move all the non-blank entries after entry ecx
	;          and copy ecx entry to the entry after it
	.Loop2:
		mov eax, [fs:ebx + edx - SizeOf_Memory_Entry]
		mov [fs:ebx + edx], eax
		mov eax, [fs:ebx + edx - SizeOf_Memory_Entry + 4]
		mov [fs:ebx + edx + 4], eax
		mov eax, [fs:ebx + edx - SizeOf_Memory_Entry + 8]
		mov [fs:ebx + edx + 8], eax

		sub edx, SizeOf_Memory_Entry
		cmp edx, ecx
		ja .Loop2

	; Step 3 - Update entries content
	mov eax, [gs:ebp - 4]
	mov [fs:ebx + ecx + 8], eax
	sub [fs:ebx + ecx + SizeOf_Memory_Entry + 8], eax
	add [fs:ebx + ecx + SizeOf_Memory_Entry + 4], eax

	xor eax, eax
	.Return:
	pop ebp
	ret
	.Not_Split:
	mov eax, 1
	jmp .Return

Function_Deallocate:	; Function 3
	.Ptr equ dword [gs:ebp - 4] ; Ptr : Address

	push ebp
	add ebp, 4
	push ebx
	push ecx
	push edx

	call Get_lock_0

	; Step 1 - Find Entry
	.Step1:
	mov eax, .Ptr
	mov ebx, $A000
	add ebx, SizeOf_Region_Header
	xor ecx, ecx

	.Loop1:
		mov edx, [fs:ebx + ecx + 4]
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
	mov [fs:ebx + ecx], dword 0
	call Function_Merge_Entry	; Merge entries around ECX

	xor eax, eax

	.Return:
	call Release_lock_0

	pop edx
	pop ecx
	pop ebx
	pop ebp
	ret

	.Not_Found:
	mov eax, ENTRY_NOT_FOUND
	jmp .Return

	restore .Ptr

Function_Merge_Entry:	; Merge entries around ECX
	; Step 1 - Check the previous entry
	xor edx, edx
	test ecx, ecx
	jz .Next

	mov eax, [fs:ebx + ecx]
	cmp eax, [fs:ebx + ecx - SizeOf_Memory_Entry]
	jne .Next

	sub ecx, SizeOf_Memory_Entry
	mov eax, [fs:ebx + ecx + SizeOf_Memory_Entry + 8]
	add [fs:ebx + ecx + 8], eax

	mov eax, [fs:ebx + ecx + SizeOf_Memory_Entry * 2]
	mov [fs:ebx + ecx + SizeOf_Memory_Entry], eax
	mov eax, [fs:ebx + ecx + SizeOf_Memory_Entry * 2 + 4]
	mov [fs:ebx + ecx + SizeOf_Memory_Entry + 4], eax
	mov eax, [fs:ebx + ecx + SizeOf_Memory_Entry * 2 + 8]
	mov [fs:ebx + ecx + SizeOf_Memory_Entry + 8], eax

	add edx, SizeOf_Memory_Entry

	; Step 2 - Check the subsequent entry
	.Next:
	lea eax, [ecx + SizeOf_Memory_Entry]
	cmp eax, SizeOf_Memory_Table
	je .Move

	mov eax, [fs:ebx + eax]
	cmp eax, [fs:ebx + ecx]
	jne .Move

	mov eax, [fs:ebx + ecx + SizeOf_Memory_Entry + 8]
	add [fs:ebx + ecx + 8], eax

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
		cmp [fs:ebx + ecx + 4], dword 0
		jne .a
		sub ecx, SizeOf_Memory_Entry
		jmp .End_Loop

		.a:
		mov eax, [fs:ebx + ecx]
		mov [fs:edx + ecx], eax
		mov eax, [fs:ebx + ecx + 4]
		mov [fs:edx + ecx + 4], eax
		mov eax, [fs:ebx + ecx + 8]
		mov [fs:edx + ecx + 8], eax

		add ecx, SizeOf_Memory_Entry
		cmp ecx, SizeOf_Memory_Table
		jb .Loop
	.End_Loop:

	sub edx, ebx
	add ecx, edx

	.Finish:
	xor eax, eax
	mov [fs:ebx + ecx + SizeOf_Memory_Entry], eax
	mov [fs:ebx + ecx + SizeOf_Memory_Entry + 4], eax
	mov [fs:ebx + ecx + SizeOf_Memory_Entry + 8], eax

	cmp edx, -(SizeOf_Memory_Entry * 2)
	jne .Return

	mov [fs:ebx + ecx + SizeOf_Memory_Entry * 2], eax
	mov [fs:ebx + ecx + SizeOf_Memory_Entry * 2 + 4], eax
	mov [fs:ebx + ecx + SizeOf_Memory_Entry * 2 + 8], eax

	.Return:
	ret

Function_Mark_Memory:	; Function 4
	.Start equ dword [gs:ebp - 12]		; Offset : Address
	.Limit equ dword [gs:ebp - 8]		; Limit : Address
	.Module_Idx equ dword [gs:ebp - 4]	; Module_Idx : Cardinal

	push ebp
	add ebp, 12
	push ebx
	push ecx
	push edx
	push esi

	call Get_lock_0

	.Begin:
	mov eax, .Limit
	mov edx, .Start
	cmp eax, edx
	jb .Error2	; Limit is below Start
	inc eax

	; Step 1 - Find entry
	mov ebx, $A000
	add ebx, SizeOf_Region_Header
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
		add ecx, SizeOf_Memory_Entry
		cmp ecx, SizeOf_Memory_Table
		jb .Loop1
		jmp .Error3	; No more entry to find

	; Step 2 - Split entry
	.Found:
	sub edx, [fs:ebx + ecx + 4]
	jz .Next2

	mov [gs:ebp], edx
	call Function_Split_Entry

	test eax, eax
	jnz .Error4
	add ecx, SizeOf_Memory_Entry

	.Next2:
	test esi, esi
	jz .Finish

	neg esi
	add esi, [fs:ebx + ecx + 8]

	mov [gs:ebp], esi
	call Function_Split_Entry

	.Finish:
	mov eax, .Module_Idx
	mov [fs:ebx + ecx], eax
	xor eax, eax

	.Return:
	call Release_lock_0

	pop esi
	pop edx
	pop ecx
	pop ebx
	pop ebp
	ret

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

Get_lock_1:
	lock bts word [fs:System_data + Code_region_lock], 0
	jc .Wait
	ret
	.Wait:
	pause
	jmp Get_lock_1

Release_lock_1:
	btr word [fs:System_data + Code_region_lock], 0
	ret

Function_Create_Code_Region:  ; Function 5
	.Limit equ dword [gs:ebp - 4]

	push ebp
	add ebp, 4

	push ebx
	push ecx

	mov eax, .Limit
	cmp eax, SizeOf_Code_Region
	jb .Error2	; Region size must be > $4000

	; Zero fill $4000 bytes (except byte 0)
	mov dword [fs:4], 0
	mov ecx, 1
	.Fill_zero:
		mov dword [fs:ecx * 8], 0
		mov dword [fs:ecx * 8 + 4], 0
		inc ecx
		cmp ecx, SizeOf_Code_Region / 8
		jb .Fill_zero

	; Create the first entry
	mov ecx, SizeOf_Code_Region
	mov [fs:SizeOf_Code_Header + 4], ecx
	sub eax, (SizeOf_Code_Region - 1)
	mov [fs:SizeOf_Code_Header + 8], eax

	xor eax, eax

	.Return:
	pop ecx
	pop ebx
	pop ebp
	ret

	.Error2:
	mov eax, REGION_SIZE_IS_NOT_LARGE_ENOUGH
	jmp .Return

	restore .Limit

Function_Allocate_Code:    ; Function 6
	.Size equ dword [gs:ebp - 8]	; Size : Cardinal
	.Type equ dword [gs:ebp - 4]	; Type : Cardinal

	push ebp
	add ebp, 8

	push ebx
	push ecx
	push edx
	push esi
	push edi

	call Get_lock_1

	; Step 1 - Find free entry with enough size (first-fit)
	.Step1:
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

	mov [gs:ebp], eax	; EAX is the size we want to split
	call Function_Split_Code_Entry

	.Done:
	mov eax, .Type
	mov [fs:ebx + ecx], eax
	mov eax, [fs:ebx + ecx + 4]
	mov [ss:_Result], eax
	xor eax, eax

	.Return:
	call Release_lock_1

	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error2:
	mov eax, NO_FREE_MEMORY
	jmp .Return

	restore .Size
	restore .Type

Function_Split_Code_Entry:
	push ebp
	add ebp, 4

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
	mov eax, [gs:ebp - 4]
	mov [fs:ebx + ecx + 8], eax
	sub [fs:ebx + ecx + SizeOf_Code_Entry + 8], eax
	add [fs:ebx + ecx + SizeOf_Code_Entry + 4], eax

	xor eax, eax
	.Return:
	pop ebp
	ret
	.Not_Split:
	mov eax, 1
	jmp .Return

Function_Deallocate_Code:    ; Function 7
	.Ptr equ dword [gs:ebp - 4] ; Ptr : Address

	push ebp
	add ebp, 4

	push ebx
	push ecx
	push edx

	call Get_lock_1

	; Step 1 - Find Entry
	.Step1:
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
	call Release_lock_1

	pop edx
	pop ecx
	pop ebx
	pop ebp
	ret

	.Error1:
	mov eax, SUBINTERFACE_UNAVAILABLE
	jmp .Return

	.Not_Found:
	mov eax, ENTRY_NOT_FOUND
	jmp .Return

	restore .Ptr

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
	.Start equ dword [gs:ebp - 12]	   ; Start : Address
	.Limit equ dword [gs:ebp - 8]	   ; Limit : Address
	.Type equ dword [gs:ebp - 4]	   ; Type : Cardinal

	push ebp
	add ebp, 12

	push ebx
	push ecx
	push edx
	push esi

	call Get_lock_1

	.Begin:
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

	mov [gs:ebp], edx
	call Function_Split_Code_Entry

	test eax, eax
	jnz .Error4
	add ecx, SizeOf_Code_Entry

	.Next2:
	test esi, esi
	jz .Finish

	neg esi
	add esi, [fs:ebx + ecx + 8]

	mov [gs:ebp], esi
	call Function_Split_Code_Entry

	.Finish:
	mov eax, .Type
	mov [fs:ebx + ecx], eax
	xor eax, eax

	.Return:
	call Release_lock_1

	pop esi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

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

Setup_paging:
	mov esi, Page_directory
	xor eax, eax
	mov ecx, ($1000 / 4) - 1
	.Loop1:
		mov [fs:esi + ecx * 4], eax
		loop .Loop1

	mov eax, First_page_table + $F
	mov [fs:esi], eax

	mov esi, First_page_table
	xor ecx, ecx
	mov eax, $F
	.Loop2:
		mov [fs:esi + ecx], eax
		add eax, $1000
		add ecx, 4
		cmp ecx, $1000
		jb .Loop2

	mov eax, Page_directory
	mov cr3, eax
	mov eax, cr0
	bts eax, 31
	mov cr0, eax

	ret
