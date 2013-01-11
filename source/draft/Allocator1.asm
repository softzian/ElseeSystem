; Allocator1.asm - Simple allocator
; Written in 2012 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

include 'include\Header.inc'
use32

; Function 1: Create_table (Table_address : Address; Entry_size, Num_of_entry : Cardinal)

; Function 2: Add_table_entry (Table : Address) : Cardinal
; Function 3: Delete_table_entry (Table : Address; Index : Cardinal)

; Function 4: Get_table_entry (Table : Address; Index : Cardinal) : Address

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Init_allocator

	dd Function_Allocate
	dd Function_Deallocate
Header:

Error_code:
	NO_FREE_MEMORY = 1
	ENTRY_NOT_FOUND = 2

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	lea esi, [eax + Interface]

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 3
		jb .Loop

	mov [gs:ebp], dword 1
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 1
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], dword ebx
	invoke ISystem, ISystem.Register_Module

	pop esi
	pop edi
	pop ebx
	ret

Get_lock_0:
	lock bts dword [ds:4], 0
	jc .Wait
	ret
	.Wait:
	invoke IThread, IThread.Yield
	jmp Get_lock_0

Release_lock_0:
	btr dword [ds:4], 0
	ret

Function_Init_allocator:  ; Function 1
	.Start equ dword [gs:ebp - 8]
	.Limit equ dword [gs:ebp - 4]

	push ebp
	add ebp, 8
	push ebx
	push ecx

	mov ebx, .Start
	cmp ebx, 8
	jb .Error1

	cmp ebx, $FFFFFF
	jae .Error2

	mov eax, .Limit
	cmp eax, $FFFFFF
	ja .Error2

	cmp eax, ebx
	jbe .Error2

	sub eax, ebx
	cmp eax, $FFF
	jbe .Error2

	mov [ds:0], ebx

	; Zero fill $1000 bytes
	xor ecx, ecx
	.Fill_zero:
		mov dword [ds:ebx + ecx], 0
		add ecx, 4
		cmp ecx, $1000
		jb .Fill_zero

	; Create the first entry
	inc eax
	sub eax, $1000
	add ecx, ebx
	mov [ds:ebx + 2], ecx
	mov [ds:ebx + 5], eax

	xor eax, eax

	.Return:
	pop ecx
	pop ebx
	pop ebp
	ret

	.Error1:
	mov eax, 1
	jmp .Return

	.Error2:
	mov eax, 2
	jmp .Return

	restore .Start
	restore .Limit

Function_Allocate:    ; Function 2
	.Size equ dword [gs:ebp - 4] ; Size : Cardinal

	push ebp
	add ebp, 4

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
	mov ebx, [ds:0]

	.Loop1:
		mov edi, [ds:ebx + ecx + 2]
		and edi, $FFFFFF
		test edi, edi
		jz .Error2

		cmp byte [ds:ebx + ecx], 0
		jne .Next1

		mov esi, [ds:ebx + ecx + 5]
		and esi, $FFFFFF
		cmp eax, esi
		je .Alloc	; Jump to Step 3
		jb .Find_Best_Fit	; Jump to Step 2

		.Next1:
		add ecx, 8
		cmp ecx, $1000
		jae .Error2
		jmp .Loop1

	; Step 2 - Find best fit entry
	.Find_Best_Fit:
	lea edx, [ecx + 8]
	cmp edx, $1000
	je .Alloc	; If no more entries to search, jump to Step 3

	.Loop2:
		mov edi, [ds:ebx + edx + 2]
		and edi, $FFFFFF
		test edi, edi
		jz .Alloc

		cmp byte [ds:ebx + edx], 0
		jne .Next2

		mov edi, [fs:ebx + edx + 5]
		and edi, $FFFFFF
		cmp eax, edi
		ja .Next2
		je .End_Loop2

		cmp esi, edi
		jbe .Next2
		mov ecx, edx
		mov esi, edi

		.Next2:
		add edx, 8
		cmp edx, $1000
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
	mov [ds:ebx + ecx], byte 1

	; Return new pointer
	mov eax, [ds:ebx + ecx + 2]
	and eax, $FFFFFF
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
	jmp .Return

	restore .Size

Function_Split_Entry:	; Parameter is the size we want to split
	push ebp
	add ebp, 4

	; Step 1 - Find first blank entry after entry ecx
	mov edx, ecx
	.Loop1:
		mov eax, [ds:ebx + edx + 2]
		and eax, $FFFFFF
		test eax, eax
		jz .Loop2
		add edx, 8
		cmp edx, $1000
		jb .Loop1
		jae .Not_Split

	; Step 2 - Move all the non-blank entries after entry ecx
	;          and copy ecx entry to the entry after it
	.Loop2:
		mov eax, [ds:ebx + edx - 8]
		mov [ds:ebx + edx], eax
		mov eax, [ds:ebx + edx - 8 + 4]
		mov [ds:ebx + edx + 4], eax

		sub edx, 8
		cmp edx, ecx
		ja .Loop2

	; Step 3 - Update entries content
	mov eax, [gs:ebp - 4]
	sub [ds:ebx + ecx + 8 + 5], eax
	add [ds:ebx + ecx + 8 + 2], eax

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
	mov ebx, [ds:0]
	xor ecx, ecx

	.Loop1:
		mov edx, [ds:ebx + ecx + 2]
		and edx, $FFFFFF
		test edx, edx
		jz .Not_Found
		cmp edx, eax
		je .Found

		add ecx, 8
		cmp ecx, $1000
		jb .Loop1
		jmp .Not_Found

	; Step 2 - Mark entry as free and Merge entries
	.Found:
	mov [ds:ebx + ecx], byte 0
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

	mov al, [ds:ebx + ecx]
	cmp al, [ds:ebx + ecx - 8]
	jne .Next

	sub ecx, 8
	mov eax, [ds:ebx + ecx + 8 + 5]
	and eax, $FFFFFF
	add [ds:ebx + ecx + 5], eax

	mov eax, [ds:ebx + ecx + 8 * 2]
	mov [ds:ebx + ecx + 8], eax
	mov eax, [ds:ebx + ecx + 8 * 2 + 4]
	mov [ds:ebx + ecx + 8 + 4], eax

	add edx, 8

	; Step 2 - Check the subsequent entry
	.Next:
	lea eax, [ecx + 8]
	cmp eax, $1000
	je .Move

	mov al, [ds:ebx + eax]
	cmp al, [ds:ebx + ecx]
	jne .Move

	mov eax, [fs:ebx + ecx + 8 + 5]
	and eax, $FFFFFF
	add [fs:ebx + ecx + 5], eax

	add edx, 8

	; Step 3 - Do some moving if necessary
	.Move:
	test edx, edx
	jz .Return

	lea eax, [ecx + 8 + edx]
	cmp eax, $1000
	jae .Finish

	mov ecx, eax
	neg edx
	add edx, ebx

	.Loop:
		mov eax, [ds:ebx + ecx + 2]
		and eax, $FFFFFF
		test eax, eax
		jnz .a
		sub ecx, 8
		jmp .End_Loop

		.a:
		mov eax, [ds:ebx + ecx]
		mov [ds:edx + ecx], eax
		mov eax, [ds:ebx + ecx + 4]
		mov [ds:edx + ecx + 4], eax

		add ecx, 8
		cmp ecx, $1000
		jb .Loop
	.End_Loop:

	sub edx, ebx
	add ecx, edx

	.Finish:
	xor eax, eax
	mov [ds:ebx + ecx + 8], eax
	mov [ds:ebx + ecx + 8 + 4], eax

	cmp edx, -(8 * 2)
	jne .Return

	mov [ds:ebx + ecx + 8 * 2], eax
	mov [ds:ebx + ecx + 8 * 2 + 4], eax

	.Return:
	ret