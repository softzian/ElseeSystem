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

IData = $100014
; Function 1: Create_table (Table_address : Address; Entry_size, Num_of_entry : Cardinal)

; Function 2: Add_table_entry (Table : Address) : Cardinal
; Function 3: Delete_table_entry (Table : Address; Index : Cardinal)

; Function 4: Get_table_entry (Table : Address; Index : Cardinal) : Address

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Create_table

	dd Function_Add_table_entry
	dd Function_Delete_table_entry

	dd Function_Get_table_entry
Header:

Const:
	Table_lock = 0
	Table_entry_size = 4
	Table_size = 8
	Table_data = 12

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
	lea esi, [eax + Interface]
	mov [fs:IData], eax

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 4
		jb .Loop

	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 5
	mov [gs:ebp + 16], dword ebx
	invoke ISystem, ISystem.Register_Module

	pop esi
	pop edi
	pop ebx
	ret

Function_Create_table:
	.Table_address equ dword [gs:ebp - 12]
	.Entry_size equ dword [gs:ebp - 8]
	.Table_size equ dword [gs:ebp - 4]

	push ebp
	add ebp, 12

	push ebx
	push ecx

	mov ebx, .Table_address
	mov eax, .Entry_size
	mov [ds:ebx + Table_entry_size], eax
	mov ecx, .Table_size
	mov [ds:ebx + Table_size], ecx
	xor eax, eax
	mov [ds:ebx + Table_lock], eax

	.Loop:
		mov [ds:ebx + Table_data + ecx - 1], al
		loop .Loop

	.Return:
	pop ecx
	pop ebx
	pop ebp
	ret

Lock_table:
	lock bts dword [ds:ebx + Table_lock], 0
	jc .Wait
	ret
	.Wait:
	invoke IThread, IThread.Yield
	jmp Lock_table

macro Unlock_table
{
	btr dword [ds:ebx + Table_lock], 0
}

Function_Add_table_entry:
	.Table equ dword [gs:ebp - 4]

	push ebp
	add ebp, 4

	push ebx
	push ecx
	push edx

	mov ebx, .Table
	call Lock_table

	mov ecx, [ds:ebx + Table_entry_size]
	mov edx, [ds:ebx + Table_size]
	mov eax, Table_data
	sub edx, ecx

	.Loop:
		cmp eax, edx
		jae .Error1
		cmp [ds:ebx + eax], byte 0
		je .Found
		add eax, ecx
		jmp .Loop

	.Found:
	mov [ds:ebx + eax], byte $FF
	add ebx, eax

	xor edx, edx
	div ecx
	inc eax
	mov [ss:_Result], eax
	mov [ss:_Result + 4], ebx

	Unlock_table
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
	add ebp, 8

	push ebx
	push edx

	mov ebx, .Table
	mov eax, .Index

	call Lock_table

	test eax, eax
	jz .Error1

	mul dword [ds:ebx + Table_entry_size]
	test edx, edx
	jnz .Error1
	add eax, Table_data
	cmp eax, [ds:ebx + Table_size]
	jae .Error1

	sub eax, [ds:ebx + Table_entry_size]
	mov [ds:ebx + eax], byte 0

	xor eax, eax

	.Return:
	Unlock_table
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

	mul dword [ds:ebx + Table_entry_size]
	test edx, edx
	jnz .Error1
	add eax, Table_data
	cmp eax, [ds:ebx + Table_size]
	jae .Error1

	sub eax, [ds:ebx + Table_entry_size]
	add eax, ebx
	mov [ss:_Result], eax

	xor eax, eax

	.Return:
	Unlock_table
	pop edx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_INDEX
	jmp .Return

	restore .Table
	restore .Index

Function_Create_queue:	; Function 9
	.Address equ dword [gs:ebp - 8]
	.Size equ dword [gs:ebp - 4]	   ; Size : Cardinal

	push ebp
	add ebp, 8
	push ebx

	mov eax, .Size

	test eax, eax
	jz .Error1

	add eax, 2
	shl eax, 4	; Size of whole structure
	mov .Size, eax

	mov [gs:ebp], eax
	;call Function_Allocate

	test eax, eax
	jnz .Error2

	mov ebx, [ss:_Result]
	mov eax, .Size

	add eax, ebx
	mov [ds:ebx], eax  ; Limit
	mov [ds:ebx + 4], dword 0  ; Lock
	lea eax, [ebx + 16]
	mov [ds:ebx + 8], eax	   ; Read pointer
	mov [ds:ebx + 12], eax	   ; Write pointer

	xor eax, eax
	.Return:
	pop ebx
	pop ebp
	ret

	.Error1:
	mov eax, 0;INVALID_SIZE
	jmp .Return
	.Error2:
	mov eax, 0;CANNOT_ALLOCATE
	jmp .Return

	restore .Size

Get_Queue_lock:
	lock bts dword [ds:ebx + 4], 0
	jc .Wait
	ret

	.Wait:
	invoke IThread, IThread.Yield
	jmp Get_Queue_lock

Function_Send_Message:		; Function 10
	.Queue equ dword [gs:ebp - 20]	 ; Queue : Address
	.Msg = -16 ; Msg : Message : 16 bytes [gs:ebp - 16]

	push ebp
	add ebp, 20
	push ebx
	push ecx
	push edx

	mov ebx, .Queue

	call Get_Queue_lock

	.Next1:
	mov eax, [ds:ebx + 12]	   ; Write pointer

	add eax, 16
	cmp eax, [ds:ebx]
	jb .Next2
	lea eax, [ds:ebx + 16]

	.Next2:
	cmp eax, [ds:ebx + 8]
	je .Error1		; Queue is full

	; Copy message to queue
	mov edx, [ds:ebx + 12]

	mov ecx, [gs:ebp + .Msg]
	mov [ds:edx], ecx
	mov ecx, [gs:ebp + .Msg + 4]
	mov [ds:edx + 4], ecx
	mov ecx, [gs:ebp + .Msg + 8]
	mov [ds:edx + 8], ecx
	mov ecx, [gs:ebp + .Msg + 12]
	mov [ds:edx + 12], ecx

	; Update write pointer
	mov [ds:ebx + 12], eax

	xor eax, eax

	.Release_lock:
	lock btr dword [ebx + 4], 0

	.Return:
	pop edx
	pop ecx
	pop ebx
	pop ebp
	ret
	.Error1:
	mov eax, 0;MESSAGE_QUEUE_FULL
	jmp .Release_lock

	restore .Queue

Function_Get_Message:	       ; Function 11
	.Queue equ dword [gs:ebp - 4]	  ; Queue : Address

	push ebp
	add ebp, 4
	push ebx
	push ecx

	mov ebx, .Queue

	call Get_Queue_lock

	.Next1:
	mov eax, [ds:ebx + 8]	   ; Read pointer

	cmp eax, [ds:ebx + 12]
	je .Error1		; Queue is empty

	; Get message from queue
	mov ecx, [ds:eax]
	mov [ss:_Result], ecx
	mov ecx, [ds:eax + 4]
	mov [ss:_Result + 4], ecx
	mov ecx, [ds:eax + 8]
	mov [ss:_Result + 8], ecx
	mov ecx, [ds:eax + 12]
	mov [ss:_Result + 12], ecx

	; Increase read pointer
	add eax, 16
	cmp eax, [ds:ebx]
	jb .Finish
	lea eax, [ds:ebx + 16]

	.Finish:
	mov [ds:ebx + 8], eax
	xor eax, eax

	.Release_lock:
	lock btr dword [ds:ebx + 4], 0

	.Return:
	pop ecx
	pop ebx
	pop ebp
	ret

	.Error1:
	mov eax, 0;MESSAGE_QUEUE_EMPTY
	jmp .Release_lock

	restore .Queue