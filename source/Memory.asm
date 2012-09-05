; Memory.asm - IMemory Basic Memory Manager Module
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

SizeOf_Region_Header = 4
SizeOf_Memory_Entry = 12
SizeOf_Master_Table = ($4000 - SizeOf_Region_Header)

use32

IMemory = $100000
; Function 1: Create_Region (Start, Limit : Address; Type : Cardinal)
; Function 2: Allocate (var Ptr : Address; Region : Address; Size : Cardinal)
; Function 3: Deallocate (Ptr : Address; Region : Address)
; Function 4: Mark_Memory (var Region : Memory_Region; Start, Limit : Address; Module_Idx : Cardinal)

Function_Init:
	push ebx
	push edi

	mov ebx, eax
	mov edi, IMemory
	mov [edi], eax
	lea eax, [ebx+Function_Create_Region]
	mov [edi+4], eax
	lea eax, [ebx+Function_Allocate]
	mov [edi+8], eax
	lea eax, [ebx+Function_Deallocate]
	mov [edi+12], eax
	lea eax, [ebx+Function_Mark_Memory]
	mov [edi+16], eax

	xor eax, eax
	pop edi
	pop ebx
	ret

Function_Create_Region:  ; Function 1
	.Start equ dword [ebp+16]
	.Limit equ dword [ebp+12]
	.Type equ dword [ebp+8]

	push ebp
	mov ebp, esp

	push ebx
	push ecx

	mov eax, .Type
	test eax, eax
	jnz .Error1

	mov eax, .Limit
	mov ebx, .Start

	cmp eax, ebx
	jbe .Error2	; Region size is negative

	sub eax, ebx
	cmp eax, $4000
	jb .Error2	; Region size must be > $4000

	; Zero fill $4000 byte
	xor ecx, ecx
	.Fill_zero:
	mov dword [ebx+ecx*8], 0
	mov dword [ebx+ecx*8+4], 0
	inc ecx
	cmp ecx, $500
	jb .Fill_zero

	; Create the first entry
	lea ecx, [ebx+$4000]
	mov [ebx+SizeOf_Region_Header+4], ecx
	sub eax, ($4000-1)
	mov [ebx+SizeOf_Region_Header+8], eax

	xor eax, eax

	.Return:
	pop ecx
	pop ebx
	leave
	ret 12
	.Error1:
	mov eax, UNSUPPORTED_FUNCTION
	jmp .Return
	.Error2:
	mov eax, NOT_LARGE_ENOUGH
	jmp .Return
	restore .Start
	restore .Limit
	restore .Type

Function_Allocate:    ; Function 2
	.Ptr equ dword [ebp+16] ; var Ptr : Address
	.Region equ dword [ebp+12] ; Region : Address
	.Size equ dword [ebp+8] ; Size : Cardinal

	push ebp
	mov ebp, esp

	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, .Region
	cmp [ebx], dword 0
	jne .Error1	; Region type is not 0

	; Step 1 - Find free entry with enough size (first-fit)
	xor ecx, ecx
	mov eax, .Size
	add ebx, SizeOf_Region_Header

	.Loop1:
		cmp dword [ebx+ecx+4], 0
		je .Error2

		cmp dword [ebx+ecx], 0
		jne .Next1

		mov esi, [ebx+ecx+8]
		cmp eax, esi
		je .Alloc	; Jump to Step 3
		jb .Find_Best_Fit	; Jump to Step 2

		.Next1:
		add ecx, 12
		cmp ecx, ($4000 - 4)
		jae .Error2
		jmp .Loop1

	; Step 2 - Find best fit entry
	.Find_Best_Fit:
	lea edx, [ecx+12]
	cmp edx, ($4000 - 4)
	je .Alloc	; If no more entries to search, jump to Step 3

	.Loop2:
		cmp dword [ebx+edx+4], 0
		je .Alloc

		cmp dword [ebx+edx], 0
		jne .Next2

		mov edi, [ebx+edx+8]
		cmp eax, edi
		ja .Next2
		je .End_Loop2

		cmp esi, edi
		jbe .Next2
		mov ecx, edx
		mov esi, edi

		.Next2:
		add edx, 12
		cmp edx, ($4000 - 4)
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
	mov dword [ebx+ecx], 1
	mov edx, .Ptr
	mov eax, [ebx+ecx+4]
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
	mov eax, UNSUPPORTED_FUNCTION
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
		cmp dword [ebx+edx+4], 0
		je .Loop2
		add edx, 12
		cmp edx, ($4000 - 4)
		jb .Loop1
		jae .Not_Split

	; Step 2 - Move all the non-blank entries after entry ecx
	;          and copy ecx entry to the entry after it
	.Loop2:
		mov eax, [ebx+edx-12]
		mov [ebx+edx], eax
		mov eax, [ebx+edx-12+4]
		mov [ebx+edx+4], eax
		mov eax, [ebx+edx-12+8]
		mov [ebx+edx+8], eax

		sub edx, 12
		cmp edx, ecx
		ja .Loop2

	; Step 3 - Update entries content
	mov eax, [esp+4]
	mov [ebx+ecx+8], eax
	sub [ebx+ecx+12+8], eax
	add [ebx+ecx+12+4], eax

	xor eax, eax
	.Return:
	ret 4
	.Not_Split:
	mov eax, 1
	jmp .Return

Function_Deallocate:	; Function 3
	.Ptr equ dword [ebp+12] ; Ptr : Address
	.Region equ dword [ebp+8] ; Region : Address

	push ebp
	mov ebp, esp

	push ebx
	push ecx
	push edx

	mov ebx, .Region
	cmp [ebx], dword 0
	jne .Error1

	; Step 1 - Find Entry
	mov eax, .Ptr
	add ebx, SizeOf_Region_Header
	xor ecx, ecx

	.Loop1:
		mov edx, [ebx+ecx+4]
		test edx, edx
		jz .Not_Found
		cmp edx, eax
		je .Found

		add ecx, 12
		cmp ecx, ($4000 - 4)
		jb .Loop1
		jmp .Not_Found

	; Step 2 - Mark entry as free and Merge entries
	.Found:
	mov [ebx+ecx], dword 0
	call Function_Merge_Entry

	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx
	leave
	ret 8
	.Error1:
	mov eax, UNSUPPORTED_FUNCTION
	jmp .Return
	.Not_Found:
	mov eax, MEMORY_ENTRY_NOT_FOUND
	jmp .Return
	restore .Ptr
	restore .Region

Function_Merge_Entry:
	; Step 1 - Check the previous entry
	xor edx, edx
	test ecx, ecx
	jz .Next

	mov eax, [ebx+ecx]
	cmp eax, [ebx+ecx-12]
	jne .Next

	sub ecx, 12
	mov eax, [ebx+ecx+12+8]
	add [ebx+ecx+8], eax

	mov eax, [ebx+ecx+12*2]
	mov [ebx+ecx+12], eax
	mov eax, [ebx+ecx+12*2+4]
	mov [ebx+ecx+12+4], eax
	mov eax, [ebx+ecx+12*2+8]
	mov [ebx+ecx+12+8], eax

	add edx, 12

	; Step 2 - Check the subsequent entry
	.Next:
	lea eax, [ecx+12]
	cmp eax, ($4000 - 4)
	je .Move

	mov eax, [ebx+eax]
	cmp eax, [ebx+ecx]
	jne .Move

	mov eax, [ebx+ecx+12+8]
	add [ebx+ecx+8], eax

	add edx, 12

	; Step 3 - Do some moving if necessary
	.Move:
	test edx, edx
	jz .Return

	lea eax, [ecx+12+edx]
	cmp eax, ($4000 - 12)
	jae .Finish

	mov ecx, eax
	neg edx
	add edx, ebx
	.Loop:
		cmp [ebx+ecx+4], dword 0
		jne .a
		sub ecx, 12
		jmp .End_Loop

		.a:
		mov eax, [ebx+ecx]
		mov [edx+ecx], eax
		mov eax, [ebx+ecx+4]
		mov [edx+ecx+4], eax
		mov eax, [ebx+ecx+8]
		mov [edx+ecx+8], eax

		add ecx, 12
		cmp ecx, ($4000 - 4)
		jb .Loop
	.End_Loop:
	sub edx, ebx
	add ecx, edx

	.Finish:
	xor eax, eax
	mov [ebx+ecx+12], eax
	mov [ebx+ecx+12+4], eax
	mov [ebx+ecx+12+8], eax

	cmp edx, -24
	jne .Return

	mov [ebx+ecx+24], eax
	mov [ebx+ecx+24+4], eax
	mov [ebx+ecx+24+8], eax

	.Return:
	ret

Function_Mark_Memory:	; Function 4
	.Region equ dword [ebp+20]	; var Region : Memory_Region
	.Start equ dword [ebp+16]	; Offset : Address
	.Limit equ dword [ebp+12]	; Limit : Address
	.Module_Idx equ dword [ebp+8]	; Module_Idx : Cardinal

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push edx
	push esi

	mov ebx, .Region
	cmp dword [ebx], 0
	jne .Error1	; Region type is not 0

	mov eax, .Limit
	mov edx, .Start
	cmp eax, edx
	jb .Error2	; Limit is below Start
	inc eax

	; Step 1 - Find entry
	add ebx, SizeOf_Region_Header
	xor ecx, ecx

	.Loop1:
		mov esi, [ebx+ecx+4]
		test esi, esi
		jz .Error3	; No more entry to find

		cmp [ebx+ecx], dword 0
		jne .Next1

		cmp esi, edx
		ja .Next1

		add esi, [ebx+ecx+8]
		sub esi, eax
		jnc .Found

		.Next1:
		add ecx, SizeOf_Memory_Entry
		cmp ecx, SizeOf_Master_Table
		jb .Loop1
		jmp .Error3	; No more entry to find

	; Step 2 - Split entry
	.Found:
	sub edx, [ebx+ecx+4]
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
	add esi, [ebx+ecx+8]
	push esi
	call Function_Split_Entry

	.Finish:
	mov eax, .Module_Idx
	mov [ebx+ecx], eax
	xor eax, eax

	.Return:
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 16
	.Error1:
	mov eax, UNSUPPORTED_FUNCTION
	jmp .Return
	.Error2:
	mov eax, NON_POSITIVE_SIZE
	jmp .Return
	.Error3:
	mov eax, MEMORY_ENTRY_NOT_FOUND
	jmp .Return
	.Error4:
	mov eax, MASTER_TABLE_IS_FULL
	jmp .Return

	restore .Region
	restore .Start
	restore .Limit
	restore .Module_Idx