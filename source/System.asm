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

; Function 9: Create_Message_Queue (Size : Cardinal) : Address (of queue)
; Function 10: Send_Message (Queue : Address; Msg : Message)
; Function 11: Get_Message (Queue : Address) : Message

; Function 12: Copy_code_to_data (Src, Dst : Address; Count : Cardinal)
; Function 13: Copy_data_to_code (Src, Dst : Address; Count : Cardinal)

; Function 14: Copy_data_to_code (Src, Dst : Address; Count : Cardinal)
; Function 15: Copy_data_to_code (Src, Dst : Address; Count : Cardinal)
; Function 16: Copy_data_to_code (Src, Dst : Address; Count : Cardinal)
; Function 17: Copy_data_to_code (Src, Dst : Address; Count : Cardinal)
; Function 18: Copy_data_to_code (Src, Dst : Address; Count : Cardinal)
; Function 19: Copy_data_to_code (Src, Dst : Address; Count : Cardinal)

dd Function_Init
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

	dd Function_Create_Message_Queue
	dd Function_Send_Message
	dd Function_Get_Message

	dd Function_Copy_code_to_data
	dd Function_Copy_data_to_code

	dd Function_Register_Module
	dd Function_Set_module_thread_table
	dd Function_Get_module_thread_table
	dd Function_Modify_GDT_entry
	dd Function_Save_GDT_entry
	dd Function_Load_GDT_entry
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
		cmp eax, 4 * 19
		jb .Loop

	mov esi, Module_Table
	xor eax, eax
	.Loop2:
		mov [fs:esi + eax], dword 0
		add eax, 4
		cmp eax, SizeOf_Module_Table
		jb .Loop2

	pop esi
	pop edi
	pop ebx
	ret

include 'System_p1.inc' ; Function 1 to 4
include 'System_p2.inc' ; Function 5 to 8

Function_Create_Message_Queue:	; Function 9
	.Size equ dword [gs:ebp - 4]	   ; Size : Cardinal

	push ebp
	add ebp, 4
	push ebx

	mov eax, .Size

	test eax, eax
	jz .Error1

	add eax, 2
	shl eax, 4	; Size of whole structure
	mov .Size, eax

	mov [gs:ebp], eax
	call Function_Allocate

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
	mov eax, INVALID_SIZE
	jmp .Return
	.Error2:
	mov eax, CANNOT_ALLOCATE
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
	mov eax, MESSAGE_QUEUE_FULL
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
	mov eax, MESSAGE_QUEUE_EMPTY
	jmp .Release_lock

	restore .Queue

Function_Copy_code_to_data:	; Function 12
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

Function_Copy_data_to_code:	; Function 13
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

include 'System_p3.inc'
