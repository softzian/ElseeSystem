; System2.asm - Multithread serialized interface to system functions
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

use32

ISystem2 = $100020
; Function 1: Allocate (Size : Cardinal) : Address
; Function 2: Deallocate (Ptr : Address)

; Function 3: Allocate_Code (Size : Cardinal; Type : Cardinal) : Address
; Function 4: Deallocate_Code (Ptr : Address)

; Function 5: Register_Module (Module_id : Card128; Module_address : Address)

; Function 6: New_Thread (Entry_point : Address) : Card32

; Function 7: Add_address_space (Base : Address; Limit : Card32) : Card32

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Allocate
	dd Function_Deallocate

	dd Function_Allocate_Code
	dd Function_Deallocate_Code

	dd Function_Register_Module

	dd Function_New_Thread

	dd Function_Add_address_space
Header:

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:ISystem2], eax
	mov [fs:ISystem2 + 4], esi

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 7
		jb .Loop

	mov [gs:ebp], dword 8
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], dword ebx
	invoke ISystem, ISystem.Register_Module

	; Create main thread
	lea eax, [ebx + Main_thread]
	mov [gs:ebp], eax
	mov [gs:ebp + 4], ebx
	invoke IThread, IThread.New_Thread

	mov eax, [ss:_Result]
	mov [gs:ebp], eax
	invoke IThread, IThread.Start

	pop esi
	pop edi
	pop ebx
	ret

Main_thread:
	mov ebx, [fs:ISystem2]

	.Begin:
	xor esi, esi
	invoke IThread, IThread.Disable_context_switch

	.Loop:
	mov edx, [fs:ebx + esi + Slot_1.Flag]
	test edx, edx
	jz .Next1

	lea ebx, [ebx + esi + Slot_1.Parameter]
	xor ecx, ecx
	.Loop1:
		mov al, [fs:ebx + ecx]
		mov [gs:ebp + ecx], al
		inc ecx
		cmp ecx, edx
		jb .Loop1
	sub ebx, Slot_1.Parameter
	sub ebx, esi

	call dword [fs:ebx + esi + Slot_1.Function]
	mov [fs:ebx + esi + Slot_1.Error_code], eax
	mov eax, [ss:_Result]
	mov [fs:ebx + esi + Slot_1.Result], eax
	mov eax, [ss:_Result + 4]
	mov [fs:ebx + esi + Slot_1.Result + 4], eax
	mov eax, [ss:_Result + 8]
	mov [fs:ebx + esi + Slot_1.Result + 8], eax
	mov eax, [ss:_Result + 12]
	mov [fs:ebx + esi + Slot_1.Result + 12], eax

	mov [fs:ebx + esi + Slot_1.Flag], dword 0

	.Next1:
	add esi, Slot_size
	cmp esi, Slot_size * 4
	jb .Loop

	invoke IThread, IThread.Yield
	jmp .Begin

Slot_size = Slot_2 - Slot_1
Slot_1:
	.Lock dd 0
	.Flag dd 0
	.Function dd 0
	.Parameter dd 16 dup 0
	.Result dd 4 dup 0
	.Error_code dd 0
Slot_2:
	.Lock dd 0
	.Flag dd 0
	.Function dd 0
	.Parameter dd 16 dup 0
	.Result dd 4 dup 0
	.Error_code dd 0
Slot_3:
	.Lock dd 0
	.Flag dd 0
	.Function dd 0
	.Parameter dd 16 dup 0
	.Result dd 4 dup 0
	.Error_code dd 0
Slot_4:
	.Lock dd 0
	.Flag dd 0
	.Function dd 0
	.Parameter dd 16 dup 0
	.Result dd 4 dup 0
	.Error_code dd 0

Lock_slot_1:
	lock bts dword [fs:ebx + Slot_1.Lock], 0
	jc .Wait
	ret
	.Wait:
	invoke IThread, IThread.Yield
	jmp Lock_slot_1

Function_Allocate:    ; Function 1
	.Size equ dword [gs:ebp] ; Size : Cardinal

	push ebx

	mov ebx, [fs:ISystem2]
	call Lock_slot_1

	mov eax, [fs:ISystem + 4]
	mov eax, [fs:eax + ISystem.Allocate]
	mov [fs:ebx + Slot_1.Function], eax

	mov eax, .Size
	mov [fs:ebx + Slot_1.Parameter], eax
	mov eax, [ss:_ModuleIdx]
	mov [fs:ebx + Slot_1.Parameter + 4], eax

	mov [fs:ebx + Slot_1.Flag], dword 8

	.Wait:
		invoke IThread, IThread.Yield
		cmp [fs:ebx + Slot_1.Flag], dword 0
		jne .Wait

	mov eax, [fs:ebx + Slot_1.Result]
	mov [ss:_Result], eax
	mov eax, [fs:ebx + Slot_1.Error_code]

	btr dword [fs:ebx + Slot_1.Lock], 0

	.Return:
	pop ebx

	ret

	restore .Size

Function_Deallocate:	; Function 2
	.Ptr equ dword [gs:ebp] ; Ptr : Address

	push ebx

	mov ebx, [fs:ISystem2]
	call Lock_slot_1

	mov eax, [fs:ISystem + 4]
	mov eax, [fs:eax + ISystem.Deallocate]
	mov [fs:ebx + Slot_1.Function], eax

	mov eax, .Ptr
	mov [fs:ebx + Slot_1.Parameter], eax

	mov [fs:ebx + Slot_1.Flag], dword 4

	.Wait:
		invoke IThread, IThread.Yield
		cmp [fs:ebx + Slot_1.Flag], dword 0
		jne .Wait

	mov eax, [fs:ebx + Slot_1.Error_code]

	btr dword [fs:ebx + Slot_1.Lock], 0

	.Return:
	pop ebx

	ret

	restore .Ptr

Lock_slot_2:
	lock bts dword [fs:ebx + Slot_2.Lock], 0
	jc .Wait
	ret
	.Wait:
	invoke IThread, IThread.Yield
	jmp Lock_slot_2

Function_Allocate_Code:    ; Function 3
	.Size equ dword [gs:ebp] ; Size : Card32
	.Type equ dword [gs:ebp + 4] ; Type : Card32

	push ebx

	mov ebx, [fs:ISystem2]
	call Lock_slot_2

	mov eax, [fs:ISystem + 4]
	mov eax, [fs:eax + ISystem.Allocate_Code]
	mov [fs:ebx + Slot_2.Function], eax

	mov eax, .Size
	mov [fs:ebx + Slot_2.Parameter], eax
	mov eax, .Type
	mov [fs:ebx + Slot_2.Parameter + 4], eax

	mov [fs:ebx + Slot_2.Flag], dword 8

	.Wait:
		invoke IThread, IThread.Yield
		cmp [fs:ebx + Slot_2.Flag], dword 0
		jne .Wait

	mov eax, [fs:ebx + Slot_2.Result]
	mov [ss:_Result], eax
	mov eax, [fs:ebx + Slot_2.Error_code]

	btr dword [fs:ebx + Slot_2.Lock], 0

	.Return:
	pop ebx

	ret

	restore .Size
	restore .Type

Function_Deallocate_Code:    ; Function 4
	.Ptr equ dword [gs:ebp] ; Ptr : Address

	push ebx

	mov ebx, [fs:ISystem2]
	call Lock_slot_2

	mov eax, [fs:ISystem + 4]
	mov eax, [fs:eax + ISystem.Deallocate_Code]
	mov [fs:ebx + Slot_2.Function], eax

	mov eax, .Ptr
	mov [fs:ebx + Slot_2.Parameter], eax

	mov [fs:ebx + Slot_2.Flag], dword 4

	.Wait:
		invoke IThread, IThread.Yield
		cmp [fs:ebx + Slot_2.Flag], dword 0
		jne .Wait

	mov eax, [fs:ebx + Slot_2.Error_code]

	btr dword [fs:ebx + Slot_2.Lock], 0

	.Return:
	pop ebx

	ret

	restore .Ptr

Lock_slot_3:
	lock bts dword [fs:ebx + Slot_3.Lock], 0
	jc .Wait
	ret
	.Wait:
	invoke IThread, IThread.Yield
	jmp Lock_slot_3

Function_Register_Module:    ; Function 5
	.Module_id_1 equ dword [gs:ebp] ; Module_id : Card128
	.Module_id_2 equ dword [gs:ebp + 4]
	.Module_id_3 equ dword [gs:ebp + 8]
	.Module_id_4 equ dword [gs:ebp + 12]
	.Module_address equ dword [gs:ebp + 16] ; Module_address : Address

	push ebx

	mov ebx, [fs:ISystem2]
	call Lock_slot_3

	mov eax, [fs:ISystem + 4]
	mov eax, [fs:eax + ISystem.Register_Module]
	mov [fs:ebx + Slot_3.Function], eax

	mov eax, .Module_id_1
	mov [fs:ebx + Slot_3.Parameter], eax
	mov eax, .Module_id_2
	mov [fs:ebx + Slot_3.Parameter + 4], eax
	mov eax, .Module_id_3
	mov [fs:ebx + Slot_3.Parameter + 8], eax
	mov eax, .Module_id_4
	mov [fs:ebx + Slot_3.Parameter + 12], eax
	mov eax, .Module_address
	mov [fs:ebx + Slot_3.Parameter + 16], eax

	mov [fs:ebx + Slot_3.Flag], dword 20

	.Wait:
		invoke IThread, IThread.Yield
		cmp [fs:ebx + Slot_3.Flag], dword 0
		jne .Wait

	mov eax, [fs:ebx + Slot_3.Error_code]

	btr dword [fs:ebx + Slot_3.Lock], 0

	.Return:
	pop ebx

	ret

	restore .Module_address
	restore .Module_id_1
	restore .Module_id_2
	restore .Module_id_3
	restore .Module_id_4

Lock_slot_4:
	lock bts dword [fs:ebx + Slot_4.Lock], 0
	jc .Wait
	ret
	.Wait:
	invoke IThread, IThread.Yield
	jmp Lock_slot_4

Function_New_Thread: ; Function 6
	.Entry_point equ dword [gs:ebp] ; Entry_point : Address

	push ebx

	mov ebx, [fs:ISystem2]
	call Lock_slot_4

	mov eax, [fs:IThread + 4]
	mov eax, [fs:eax + IThread.New_Thread]
	mov [fs:ebx + Slot_4.Function], eax

	mov eax, .Entry_point
	mov [fs:ebx + Slot_4.Parameter], eax
	mov eax, [ss:_ModuleIdx]
	mov [fs:ebx + Slot_4.Parameter + 4], eax

	mov [fs:ebx + Slot_4.Flag], dword 8

	.Wait:
		invoke IThread, IThread.Yield
		cmp [fs:ebx + Slot_4.Flag], dword 0
		jne .Wait

	mov eax, [fs:ebx + Slot_4.Result]
	mov [ss:_Result], eax
	mov eax, [fs:ebx + Slot_4.Error_code]

	btr dword [fs:ebx + Slot_4.Lock], 0

	.Return:
	pop ebx

	ret

	restore .Entry_point

Function_Add_address_space:    ; Function 7
	.Base equ dword [gs:ebp] ; Base : Address
	.Limit equ dword [gs:ebp + 4] ; Limit : Card32

	push ebx

	mov ebx, [fs:ISystem2]
	call Lock_slot_3

	mov eax, [fs:ISystem + 4]
	mov eax, [fs:eax + ISystem.Add_address_space]
	mov [fs:ebx + Slot_3.Function], eax

	mov eax, [ss:_ModuleIdx]
	mov [fs:ebx + Slot_3.Parameter], eax
	mov eax, .Base
	mov [fs:ebx + Slot_3.Parameter + 4], eax
	mov eax, .Limit
	mov [fs:ebx + Slot_3.Parameter + 8], eax

	mov [fs:ebx + Slot_3.Flag], dword 12

	.Wait:
		invoke IThread, IThread.Yield
		cmp [fs:ebx + Slot_3.Flag], dword 0
		jne .Wait

	mov eax, [fs:ebx + Slot_3.Result]
	mov [ss:_Result], eax
	mov eax, [fs:ebx + Slot_3.Error_code]

	btr dword [fs:ebx + Slot_3.Lock], 0

	.Return:
	pop ebx

	ret

	restore .Base
	restore .Limit