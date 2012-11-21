; Data.asm - Basic data structures module
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

IData = $100A00
; Function 1: Create_table (Entry_size, Num_of_entry : Cardinal) : Address

; Function 2: Modify_table (Table : Address)
; Function 3: Finish_modify_table (Table : Address)
; Function 4: Access_table (Table : Address)
; Function 5: Finish_access_table (Table : Address)

; Function 6: Add_table_entry (Table : Address) : Cardinal
; Function 7: Delete_table_entry (Table : Address; Index : Cardinal)
; Function 8: Get_table_entry (Table : Address; Index : Cardinal) : Cardinal

; Function 9: Delete_table (Table : Address)

jmp Function_Init
Interface:
	dd Function_Create_table

	dd Function_Modify_table
	dd Function_Finish_modify_table
	dd Function_Access_table
	dd Function_Finish_access_table

	dd Function_Add_table_entry
	dd Function_Delete_table_entry
	dd Function_Get_table_entry

	dd Function_Delete_table

Const:
	Table_size = -16
	Table_entry_size = -12
	Table_access_count = -8
	Table_modify_lock = -4
	Table_lock = -2

Error_code:
	CANNOT_ALLOC_TABLE = -1
	NO_FREE_ENTRY = -2
	INVALID_INDEX = -3
	INVALID_SIZE = -4
	ENTRY_NOT_EXIST = -5

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, IData
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, IData + 4 * 9
		jna .Loop

	pop esi
	pop edi
	pop ebx
	ret

Function_Create_table:
	.Entry_size equ dword [gs:ebp - 8]
	.Num_of_entry equ dword [gs:ebp - 4]

	push ebp
	add ebp, 8

	push ebx
	push ecx
	push edx

	mov eax, .Entry_size
	test eax, eax
	jz .Error1

	inc eax

	mov edx, .Num_of_entry
	test edx, edx
	jz .Error1

	mul edx
	test edx, edx
	jnz .Error1

	mov ebx, eax
	add eax, 16
	jc .Error1

	mov [gs:ebp], eax
	invoke ISystem.Allocate

	test eax, eax
	jnz .Error2

	mov edx, ebx

	add [ss:_Result], dword 16
	mov ebx, [ss:_Result]

	mov [ds:ebx + Table_size], edx

	mov eax, .Entry_size
	inc eax
	mov [ds:ebx + Table_entry_size], eax

	mov [ds:ebx + Table_lock], word 0
	mov [ds:ebx + Table_modify_lock], word 0
	mov [ds:ebx + Table_access_count], dword 0

	mov ecx, eax
	.Loop:
		mov [ds:ebx + ecx], byte 0
		add ecx, eax
		cmp ecx, edx
		jb .Loop

	.Finish:
	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx
	pop ebp
	ret

	.Error1:
	mov eax, INVALID_SIZE
	jmp .Return
	.Error2:
	mov eax, CANNOT_ALLOC_TABLE
	jmp .Return

Function_Modify_table:
	.Table equ dword [gs:ebp - 4] ; Table : Address

	push ebp
	add ebp, 4
	push ebx

	mov ebx, .Table

	.Get_modify_lock:
		call Get_table_lock
		cmp [ds:ebx + Table_modify_lock], word 0
		jne .Wait

		mov [ds:ebx + Table_modify_lock], word 1
		jmp .Check_access_count

		.Wait:
		lock btr word [ds:ebx + Table_lock], 0
		invoke IThread.Yield
		jmp .Get_modify_lock

	.Check_access_count:
		cmp [ds:ebx + Table_access_count], dword 0
		je .Done_get_lock

		lock btr word [ds:ebx + Table_lock], 0
		invoke IThread.Yield
		call Get_table_lock
		jmp .Check_access_count

	.Done_get_lock:
	lock btr word [ds:ebx + Table_lock], 0

	xor eax, eax

	.Return:
	pop ebx
	pop ebp
	ret

	restore .Table

Function_Finish_modify_table:
	.Table equ dword [gs:ebp - 4] ; Table : Address

	push ebp
	add ebp, 4
	push ebx

	mov ebx, .Table

	call Get_table_lock
	mov [ds:ebx + Table_modify_lock], word 0
	lock btr word [ds:ebx + Table_lock], 0

	xor eax, eax

	.Return:
	pop ebx
	pop ebp
	ret

	restore .Table

Function_Access_table:
	.Table equ dword [gs:ebp - 4] ; Table : Address

	push ebp
	add ebp, 4

	push ebx

	mov ebx, .Table

	.Loop:
		call Get_table_lock
		cmp [ds:ebx + Table_modify_lock], word 0
		je .Finish

		lock btr word [ds:ebx + Table_lock], 0
		invoke IThread.Yield
		jmp .Loop

	.Finish:
	inc dword [ds:ebx + Table_access_count]
	lock btr word [ds:ebx + Table_lock], 0

	xor eax, eax

	.Return:
	pop ebx

	pop ebp
	ret

	restore .Table

Function_Finish_access_table:
	.Table equ dword [gs:ebp - 4] ; Table : Address

	push ebp
	add ebp, 4

	push ebx

	mov ebx, .Table

	call Get_table_lock
	dec dword [ds:ebx + Table_access_count]
	lock btr word [ds:ebx + Table_lock], 0

	xor eax, eax

	.Return:
	pop ebx

	pop ebp
	ret

	restore .Table

Get_table_lock:
	lock bts word [ds:ebx + Table_lock], 0
	jc .Wait
	ret
	.Wait:
	invoke IThread.Yield
	jmp Get_table_lock

Function_Add_table_entry:
	.Table equ dword [gs:ebp - 4]

	push ebp
	add ebp, 4

	push ebx
	push ecx
	push edx

	mov ebx, .Table

	mov ecx, [ds:ebx + Table_entry_size]
	mov edx, [ds:ebx + Table_size]
	xor eax, eax

	.Loop:
		cmp [ds:ebx + eax], byte 0
		je .Finish

		add eax, ecx
		cmp eax, edx
		jb .Loop
		jmp .Error1

	.Finish:
	mov [ds:ebx + eax], byte 1

	mov [ss:_Result + 4], eax
	inc dword [ss:_Result + 4]

	xor edx, edx
	div ecx
	inc eax
	mov [ss:_Result], eax

	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, NO_FREE_ENTRY
	jmp .Return

Function_Delete_table_entry:
	.Table equ dword [gs:ebp - 8] ; Table : Address
	.Index equ dword [gs:ebp - 4] ; Index : Cardinal

	push ebp
	add ebp, 4

	push ebx
	push edx

	mov ebx, .Table
	mov eax, .Index

	test eax, eax
	jz .Error1

	dec eax

	mul dword [ds:ebx + Table_entry_size]

	test edx, edx
	jnz .Error1

	cmp eax, [ds:ebx + Table_size]
	jae .Error1

	mov [ds:ebx + eax], byte 0
	xor eax, eax

	.Return:
	pop edx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_INDEX
	jmp .Return

	restore .Table
	restore .Index

Function_Get_table_entry:
	.Table equ dword [gs:ebp - 8] ; Table : Address
	.Index equ dword [gs:ebp - 4] ; Index : Cardinal

	push ebp
	add ebp, 8

	push ebx
	push edx

	mov ebx, .Table
	mov eax, .Index

	test eax, eax
	jz .Error1

	dec eax
	mul dword [ds:ebx + Table_entry_size]

	test edx, edx
	jnz .Error1

	cmp eax, [ds:ebx + Table_size]
	jae .Error1

	inc eax
	mov [ss:_Result], eax

	cmp [ds:ebx + eax - 1], byte 0
	je .Error2

	xor eax, eax

	.Return:
	pop edx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_INDEX
	jmp .Return

	.Error2:
	mov eax, ENTRY_NOT_EXIST
	jmp .Return

	restore .Table
	restore .Index

Function_Delete_table:
	.Table equ dword [gs:ebp - 4] ; Table : Address

	push ebp
	add ebp, 4

	mov eax, .Table
	sub eax, 16

	mov [gs:ebp], eax
	invoke ISystem.Deallocate

	.Return:
	pop ebp
	ret

	restore .Table
