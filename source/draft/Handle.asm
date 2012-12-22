; Handle.asm - Handle table module
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

IHandle = $100A00
; Function 1: Create_handle (Addr : Address; Type : Cardinal; Semaphore : Card16) : Cardinal
; Function 2: Delete_handle (Handle : Cardinal)

; Function 3: Request_handle (Handle : Cardinal)
; Function 4: Resolve_handle (Handle : Cardinal) : Record [Address, Cardinal]
; Function 5: Release_handle (Handle : Cardinal)
; Function 6: Modify_handle (Handle : Cardinal; Addr : Address; Semaphore : Card16)

jmp Function_Init
Interface:
	dd Function_Create_handle
	dd Function_Delete_handle

	dd Function_Request_handle
	dd Function_Resolve_handle
	dd Function_Release_handle

Const:
	SizeOf_Handle_table = NumOf_Handles * 16
	NumOf_Handles = 256

	Handle_available = 0
	Handle_lock = 2
	Handle_semaphore = 4
	Handle_semaphore_limit = 6
	Handle_type = 8
	Handle_address = 12

Error_code:
	CANNOT_CREATE_HANDLE_TABLE = -1
	NO_FREE_ENTRY = -2
	INVALID_HANDLE = -3
	INVALID_TABLE_SIZE = -4

Var:
	.Handle_table dd 0

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, IHandle
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, IHandle + 4 * 5
		jna .Loop

	; Allocate handle table
	mov [gs:ebp], dword SizeOf_Handle_table
	invoke ISystem.Allocate

	test eax, eax
	jnz .Error1

	mov esi, [ss:_Result]
	mov [fs:ebx + Var.Handle_table], esi

	; Zero fill handle table
	xor eax, eax
	mov edi, eax
	.Loop2:
		mov [ds:esi + edi], eax
		add edi, 4
		cmp edi, SizeOf_Handle_table
		jb .Loop2

	.Return:
	pop esi
	pop edi
	pop ebx
	ret

	.Error1:
	mov eax, CANNOT_CREATE_HANDLE_TABLE
	jmp .Return

Function_Create_handle:
	.Addr equ dword [gs:ebp - 10]
	.Type equ dword [gs:ebp - 6]
	.Semaphore equ word [gs:ebp - 2]

	push ebp
	add ebp, 10

	push ebx
	push ecx
	push edx

	mov ebx, [fs:IHandle]

	mov edx, SizeOf_Handle_table
	mov ecx, 16
	mov ebx, [fs:ebx + Var.Handle_table]
	xor eax, eax

	.Loop:
		call Get_handle_lock2
		cmp [ds:ebx + eax + Handle_available], byte 0
		je .Finish

		btr word [ds:ebx + eax+ Handle_lock], 0
		add eax, ecx
		cmp eax, edx
		jb .Loop
		jmp .Error1

	.Finish:
	mov ecx, eax
	add ebx, eax

	mov [ds:ebx + Handle_available], byte 1
	mov dx, .Semaphore
	mov [ds:ebx + Handle_semaphore], dx
	mov [ds:ebx + Handle_semaphore_limit], dx
	mov edx, .Type
	mov [ds:ebx + Handle_type], edx
	mov edx, .Addr
	mov [ds:ebx + Handle_address], edx

	btr word [ds:ebx + Handle_lock], 0

	shr ecx, 4
	inc ecx
	mov [ss:_Result], ecx

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

	restore .Addr
	restore .Semaphore
	restore .Type

Get_handle_lock2:
	lock bts word [ds:ebx + eax + Handle_lock], 0
	jc .Wait
	ret
	.Wait: invoke IThread.Yield
	jmp Get_handle_lock2

Function_Request_handle:
	.Handle equ dword [gs:ebp - 4] ; Handle : Cardinal

	push ebp
	add ebp, 4

	push ebx

	mov eax, .Handle

	call Calculate_handle_index
	test al, al
	jnz .Error1

	.Loop:
		call Get_handle_lock

		cmp [ds:ebx + Handle_available], byte 0
		je .Error2

		bt word [ds:ebx + Handle_lock], 1
		je .Error2	; This handle is prepared to be deleted

		cmp [ds:ebx + Handle_semaphore], word 0
		je .Wait

		dec word [ds:ebx + Handle_semaphore]
		btr word [ds:ebx + Handle_lock], 0
		jmp .Finish

		.Wait:
		btr word [ds:ebx + Handle_lock], 0
		invoke IThread.Yield
		jmp .Loop

	.Finish:
	xor eax, eax

	.Return:
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_HANDLE
	jmp .Return

	.Error2:
	btr word [ds:ebx + Handle_lock], 0
	jmp .Error1

	restore .Handle

Calculate_handle_index:
	test eax, eax
	jz .Error

	cmp eax, NumOf_Handles
	ja .Error

	dec eax
	shl eax, 4

	mov ebx, [fs:IHandle]
	mov ebx, [fs:ebx + Var.Handle_table]
	add ebx, eax

	xor al, al
	ret
	.Error:
	mov al, 1
	ret

Get_handle_lock:
	lock bts word [ds:ebx + Handle_lock], 0
	jc .Wait
	ret
	.Wait: invoke IThread.Yield
	jmp Get_handle_lock

Function_Resolve_handle:
	.Handle equ dword [gs:ebp - 4] ; Handle : Cardinal

	push ebp
	add ebp, 4

	push ebx

	mov eax, .Handle

	call Calculate_handle_index
	test al, al
	jnz .Error1

	call Get_handle_lock

	cmp [ds:ebx + Handle_available], byte 0
	je .Error2

	btr word [ds:ebx + Handle_lock], 0

	.Finish:
	mov eax, [ds:ebx + Handle_address]
	mov [ss:_Result], eax
	mov eax, [ds:ebx + Handle_type]
	mov [ss:_Result + 4], eax
	xor eax, eax

	.Return:
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_HANDLE
	jmp .Return

	.Error2:
	btr word [ds:ebx + Handle_lock], 0
	jmp .Error1

	restore .Handle

Function_Release_handle:
	.Handle equ dword [gs:ebp - 4] ; Handle : Cardinal

	push ebp
	add ebp, 4

	push ebx

	mov eax, .Handle

	call Calculate_handle_index
	test al, al
	jnz .Error1

	call Get_handle_lock

	cmp [ds:ebx + Handle_available], byte 0
	je .Error2

	inc word [ds:ebx + Handle_semaphore]
	btr word [ds:ebx + Handle_lock], 0

	xor eax, eax

	.Return:
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_HANDLE
	jmp .Return

	.Error2:
	btr word [ds:ebx + Handle_lock], 0
	jmp .Error1

	restore .Handle

Function_Delete_handle:
	.Handle equ dword [gs:ebp - 4] ; Handle : Cardinal

	push ebp
	add ebp, 4

	push ebx

	mov eax, .Handle

	call Calculate_handle_index
	test al, al
	jnz .Error1

	call Get_handle_lock

	cmp [ds:ebx + Handle_available], byte 0
	je .Error2

	bt word [ds:ebx + Handle_lock], 1
	je .Error2	; This handle is prepared to be deleted by other thread

	mov ax, [ds:ebx + Handle_semaphore_limit]
	.Loop:
		cmp [ds:ebx + Handle_semaphore], ax
		jb .Wait

		; Deleting
		mov [ds:ebx + Handle_available], byte 0
		btr word [ds:ebx + Handle_lock], 0
		jmp .Finish

		.Wait:
		btr word [ds:ebx + Handle_lock], 0
		invoke IThread.Yield
		call Get_handle_lock
		jmp .Loop

	.Finish:
	xor eax, eax

	.Return:
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_HANDLE
	jmp .Return

	.Error2:
	btr word [ds:ebx + Handle_lock], 0
	jmp .Error1

	restore .Handle