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
include 'System_p0.inc'

use32

ISystem = $100000
; Function 1: Create_Region (Start, Limit : Address; Type : Cardinal)
; Function 2: Allocate (Region : Address; Size : Cardinal) : Address
; Function 3: Deallocate (Region : Address; Ptr : Address)
; Function 4: Mark_Memory (Start, Limit : Address; Module_Idx : Cardinal)

; Function 5: Create_Code_Region (Limit : Address)
; Function 6: Allocate_Code (Size : Cardinal; Type : Cardinal) : Address
; Function 7: Deallocate_Code (Ptr : Address)
; Function 8: Mark_Code (Start, Limit : Address; Type : Cardinal)

; Function 9: Create_Message_Queue (Size : Cardinal) : Address (of queue)
; Function 10: Send_Message (Queue : Address; Msg : Message)
; Function 11: Get_Message (Queue : Address) : Message

; Function 12: Copy_code_to_data (Src, Dst : Address; Count : Cardinal)

jmp Function_Init
Interface:
	dd Function_Create_Region
	dd Function_Allocate
	dd Function_Deallocate
	dd Function_Mark_Memory

	dd Function_Create_Code_Region
	dd Function_Allocate_Code
	dd Function_Deallocate_Code
	dd Function_Mark_Code

	dd Function_Create_Message_Queue
	dd Function_Send_Message
	dd Function_Get_Message

	dd Function_Copy_code_to_data

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

		cmp edi, ISystem + 4 * 12
		jna .Loop

	pop esi
	pop edi
	pop ebx
	ret

include 'System_p1.inc' ; Function 1 to 4
include 'System_p2.inc' ; Function 5 to 8


Function_Create_Message_Queue:	; Function 9
	.Size equ dword [gs:ebp - 4]	   ; Size : Cardinal

	push ebp
	mov ebp, [gs:0]
	push ebx

	mov eax, .Size

	test eax, eax
	jz .Error1

	add eax, 2
	shl eax, 4	; Size of whole structure
	mov .Size, eax

	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], eax
	add [gs:0], dword 8
	call Function_Allocate

	test eax, eax
	jnz .Error2

	mov ebx, [ss:_Result]
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
	pop ebp
	sub [gs:0], dword 4
	ret

	.Error1:
	mov eax, INVALID_SIZE
	jmp .Return
	.Error2:
	mov eax, CANNOT_ALLOCATE
	jmp .Return

	restore .Size

Function_Send_Message:		; Function 10
	.Queue equ dword [gs:ebp - 20]	 ; Queue : Address
	.Msg = -16 ; Msg : Message : 16 bytes [gs:ebp - 16]

	push ebp
	mov ebp, [gs:0]
	push ebx
	push ecx
	push edx

	mov ebx, .Queue

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

	mov ecx, [gs:ebp + .Msg]
	mov [edx], ecx
	mov ecx, [gs:ebp + .Msg + 4]
	mov [edx + 4], ecx
	mov ecx, [gs:ebp + .Msg + 8]
	mov [edx + 8], ecx
	mov ecx, [gs:ebp + .Msg + 12]
	mov [edx + 12], ecx

	; Update write pointer
	mov [ebx + 12], eax

	xor eax, eax

	.Release_lock:
	lock btr dword [ebx + 4], 0

	.Return:
	pop edx
	pop ecx
	pop ebx
	pop ebp
	sub [gs:0], dword 20
	ret
	.Error1:
	mov eax, MESSAGE_QUEUE_FULL
	jmp .Release_lock

	restore .Queue

Function_Get_Message:	       ; Function 11
	.Queue equ dword [gs:ebp - 4]	  ; Queue : Address

	push ebp
	mov ebp, [gs:0]
	push ebx
	push ecx

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
	mov ecx, [eax]
	mov [ss:_Result], ecx
	mov ecx, [eax + 4]
	mov [ss:_Result + 4], ecx
	mov ecx, [eax + 8]
	mov [ss:_Result + 8], ecx
	mov ecx, [eax + 12]
	mov [ss:_Result + 12], ecx

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
	pop ecx
	pop ebx
	pop ebp
	sub [gs:0], dword 4
	ret

	.Error1:
	mov eax, MESSAGE_QUEUE_EMPTY
	jmp .Release_lock

	restore .Queue

Function_Copy_code_to_data:	; Function 12
	.Src equ dword [gs:ebp - 12]	; Src : Address
	.Dst equ dword [gs:ebp - 8]	; Dst : Address
	.Count equ dword [gs:ebp - 4]	; Count : Cardinal

	push ebp
	mov ebp, [gs:0]
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
	sub [gs:0], dword 12
	ret

	restore .Src
	restore .Dst
	restore .Count

Function_Create_table:	; Function 13
	.Max_size equ word [gs:ebp - 2] ; Max_size : Word

	push ebp
	mov ebp, [gs:0]

	push ecx

	xor eax, eax
	mov cx, .Max_size
	mov ax, cx
	shl eax, 4
	add eax, 8

	mov [gs:ebp], eax
	mov [gs:ebp + 4], dword 2
	add [gs:0], dword 8
	call Function_Allocate_Code

	mov eax, [ss:_Result]
	mov [fs:eax], cx
	mov [fs:eax + 2], word 0
	mov [fs:eax + 4], word 0
	mov [fs:eax + 6], word 0

	xor eax, eax

	.Return:
	pop ecx

	pop ebp
	sub [gs:0], dword 2
	ret

	restore .Max_size

Function_Add_table_entry:
	.Table equ dword [gs:ebp - 20]	 ; Table : fs_Address
	.Entry = -16 ; Entry : 16 bytes [gs:ebp - 16]

	push ebp
	mov ebp, [gs:0]

	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, .Table
	; Get lock
	.Spinlock:
		bts word [fs:ebx + 6], 0
		jnc .Begin
		invoke IThread.Yield

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	sub [gs:0], dword 20
	ret

	restore .Table
	restore .index
