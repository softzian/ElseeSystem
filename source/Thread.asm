; Thread.asm - Thread Manager
; Written in 2012 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

include 'include\errcode.inc'
include 'include\Header.inc'

use32

; Type declaration
;       ThreadEntry_Type = Record
;               DS_entry : GDT_entry
;               SS_entry : GDT_entry
;               GS_entry : GDT_entry
;               ESP : Address
;               State : Cardinal
;               Next_thread : Thread_idx
;               Previous_thread : Thread_idx
;       End

IThread = $100008
; Function 1: New_Thread (StartPoint : Address) : Thread_handle_type
; Function 2: Start (ThreadIdx : Thread_handle_type)
; Function 3: Yield
; Function 4: Block_self
; Function 5: Block (ThreadIdx : Thread_handle_type)
; Function 6: Get_Active_Thread : Thread_handle_type

Const:
	SizeOf_Thread_entry = 64
	NumOf_Thread_entries = 64
	SizeOf_Thread_table = SizeOf_Thread_entry * NumOf_Thread_entries

	DS_entry = 0
	SS_entry = 8
	GS_entry = 16
	_ESP = 24
	State = 28
	Next_thread = 32
	Previous_thread = 36

	Free = 0
	Running = 1
	Stop = 2
	Kill = 3

Error_Code:
	ZERO_MODULE_INDEX = -1
	CAN_NOT_CREATE_STACK = -2

dd Function_Init
dd Header
Interface:
	dd Function_New_Thread
	dd Function_Start
	dd Function_Yield
;        dd Function_Block_self
Header:

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:IThread], eax

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 3
		jb .Loop

	cli

	; Install ISR for IRQ 0
	lea eax, [ebx + Procedure_IRQ0]
	mov [gs:ebp], byte $20
	mov [gs:ebp + 1], eax
	invoke IInterrupt, IInterrupt.Install_ISR

	sti

	pop esi
	pop edi
	pop ebx
	ret

Function_New_Thread:	 ; Function 1
	.StartPoint equ dword [gs:ebp - 4]	  ; StartPoint : Address

	push ebp
	add ebp, 4

	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ecx, [ss:_ModuleIdx]
	invoke ISystem, ISystem.Get_module_thread_table

	; test eax, eax
	; jnz WTF_ModuleIdx_is_invalid !? Who touch the SS critical data?

	mov edi, ebx
	test edi, edi
	jnz .Find_free_entry

	; This module doesn't have thread table, create one for it
	mov [gs:ebp], dword SizeOf_Thread_table
	invoke ISystem, ISystem.Allocate

	test eax, eax
	jnz .Error1	; Damn, run out of memory

	mov edi, [ss:_Result]
	mov esi, edi
	mov ecx, [ss:_ModuleIdx]
	invoke ISystem, ISystem.Set_module_thread_table

	xor ecx, ecx
	.Zero_fill:
		mov [fs:edi + ecx], byte 0
		inc ecx
		cmp ecx, SizeOf_Thread_table
		jb .Zero_fill


	.Find_free_entry:
	xor ecx, ecx
	.Loop:
		cmp [fs:edi + ecx + State], dword Free
		je .Found
		add ecx, 32
		cmp ecx, SizeOf_Thread_table
		jb .Loop
		jmp .Error2	; Thread Table is full

	; Step 2 - New Thread
	.Found:
	add edi, ecx

	mov [gs:ebp], dword $1000	; Size
	invoke ISystem, ISystem.Allocate    ; Allocate the first stack

	test eax, eax
	jnz .Error3	; Can not allocate stack

	; Create GDT entry for the first stack
	mov ebx, [ss:_Result]
	add ebx, $1000
	mov eax, ebx
	mov [fs:edi + SS_entry + 2], ax
	shr eax, 16
	mov [fs:edi + SS_entry + 4], al
	mov [fs:edi + SS_entry + 7], ah
	mov [fs:edi + SS_entry], word 1
	mov [fs:edi + SS_entry + 5], word 1100000010010110b

	; Init data on the first stack
	mov eax, [ss:_ModuleIdx]
	mov [fs:ebx - $14], eax
	mov [fs:ebx - $18], ecx ; ThreadIdx
	mov [fs:ebx - $1C], ebx ; Stack base

	mov eax, .StartPoint
	mov [fs:ebx - $24], eax
	pushf
	pop dword [fs:ebx - $28]
	bts dword [fs:ebx - $28], 9
	; eax ebx ecx edx ebp esi edi
	xor eax, eax
	mov [fs:ebx - $2C], eax
	mov [fs:ebx - $30], eax
	mov [fs:ebx - $34], eax
	mov [fs:ebx - $38], eax
	mov [fs:ebx - $3C], dword 16
	mov [fs:ebx - $40], eax
	mov [fs:ebx - $44], eax

	mov [gs:ebp], dword $1000	; Size
	invoke ISystem, ISystem.Allocate    ; Allocate the second stack

	test eax, eax
	jnz .Error4	; Can not allocate stack

	mov esi, [ss:_Result]

	; Create LDT entry for the second stack
	mov eax, esi
	mov [fs:edi + GS_entry + 2], ax
	shr eax, 16
	mov [fs:edi + GS_entry + 4], al
	mov [fs:edi + GS_entry + 7], ah
	mov [fs:edi + GS_entry], word 1
	mov [fs:edi + GS_entry + 5], word 1100000010010010b

	mov [fs:edi + _ESP], dword ($FFFFFFFF - $44 + 1)
	mov [fs:edi + State], dword Stop

	; Step 3 - Finish
	mov ebx, [fs:IThread]
	; Get lock 0
	.Spinlock:
		lock bts dword [fs:ebx + Var.Lock], 0
		jnc .Continue
		jmp .Spinlock

	.Continue:	; Insert thread to the round robin list
	mov edx, ecx
	shr edx, 6
	add edx, [ss:_ModuleIdx]
	mov [ss:_Result], edx	       ; Return ThreadIdx

	mov ecx, [fs:ebx + Var.First_thread]
	test ecx, ecx
	jz .First_thread

	mov [fs:edi + Next_thread], edx

	mov esi, ecx
	and esi, $00000FFF
	and ecx, $FFFFF000
	invoke ISystem, ISystem.Get_module_thread_table

	shl esi, 6
	add ebx, esi
	shr esi, 6

	mov esi, [fs:ebx + Previous_thread]
	mov [fs:edi + Previous_thread], esi
	mov [fs:ebx + Previous_thread], edx

	mov ecx, esi
	and esi, $00000FFF
	and ecx, $FFFFF000
	invoke ISystem, ISystem.Get_module_thread_table

	shl esi, 6
	add ebx, esi
	shr esi, 6

	mov [fs:ebx + Next_thread], edx
	jmp .Release_Lock

	.First_thread:
	mov [fs:ebx + Var.First_thread], edx
	mov [fs:edi + Next_thread], edx
	mov [fs:edi + Previous_thread], edx

	.Release_Lock:
	mov ebx, [fs:IThread]
	btr dword [fs:ebx + Var.Lock], 0

	xor eax, eax

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, ZERO_MODULE_INDEX
	jmp .Return
	.Error2:
	mov eax, TABLE_FULL
	jmp .Release_Lock
	.Error3:
	mov eax, CAN_NOT_CREATE_STACK
	jmp .Release_Lock
	.Error4:

	restore .StartPoint
	restore .Temp_var

Function_Start: 	; Function 2
	.ThreadIdx equ dword [gs:ebp - 4]  ; ThreadIdx : Cardinal

	push ebp
	add ebp, 4

	push ebx
	push ecx
	push edx

	mov edx, .ThreadIdx
	test edx, edx
	jz .Error1

	mov ecx, edx
	and ecx, $FFFFF000
	invoke ISystem, ISystem.Get_module_thread_table

	mov ecx, ebx
	and edx, $FFF
	shl edx, 6

	mov ebx, [fs:IThread]
	; Get lock 0
	.Spinlock:
		lock bts dword [fs:ebx + Var.Lock], 0
		jnc .Continue
		jmp .Spinlock

	.Continue:
	mov [fs:ecx + edx + State], dword Running

	xor eax, eax

	.Release_Lock:
	btr dword [fs:ebx + Var.Lock], 0

	.Return:
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INDEX_OUT_OF_RANGE
	jmp .Return

	restore .ThreadIdx

Function_Yield:        ; Function 3
	push eax
	push ebx
	mov ebx, [fs:IThread]

	; Get lock 0
	.Spinlock:
		lock bts dword [fs:ebx + Var.Lock], 0
		jnc .Continue
		jmp .Spinlock

	.Continue:
	mov eax, [ss:esp + 8]
	mov [fs:ebx + Var.EIP], eax

	lea eax, [ebx + Procedure_Switch_Context]
	mov [ss:esp + 8], eax

	.Return:
	pop ebx
	pop eax
	ret

;Function_Block_self:    ; Function 4
;        push ebx
;        mov ebx, [fs:IThread]

	; Get lock 0
;        .Spinlock:
;                lock bts dword [fs:ebx + Var.Lock], 0
;                jnc .Continue
;                jmp .Spinlock

;        .Continue:
;        mov ebx, [fs:ebx + Var.ThreadTable]
;        mov eax, [ss:_ThreadIdx]
;        mov [ds:ebx + eax * 8 + State], dword Stop

;        .Release_Lock:
;        mov ebx, [fs:IThread]
;        lock btr dword [fs:ebx + Var.Lock], 0

;        xor eax, eax

;        .Return:
;        pop ebx
;        ret

Procedure_Switch_Context:
	push 0
	pushf
	push eax
	push ebx
	push ecx
	push edx
	push ebp
	push esi
	push edi

	mov ebx, [fs:IThread]
	mov eax, [fs:ebx + Var.EIP]
	mov [ss:esp + 4 * 8], eax

	mov edx, [fs:ebx + Var.Active_thread]
	test edx, edx
	jz .First_thread

	mov ecx, edx
	and ecx, $FFFFF000
	invoke ISystem, ISystem.Get_module_thread_table

	mov esi, ebx
	and edx, $FFF
	shl edx, 6
	add esi, edx

	mov [fs:esi + _ESP], esp

	mov edx, 3
	invoke ISystem, ISystem.Save_GDT_entry
	mov [fs:esi + DS_entry + 4], ebx
	mov [fs:esi + DS_entry], ecx

	mov edx, 4
	invoke ISystem, ISystem.Save_GDT_entry
	mov [fs:esi + SS_entry + 4], ebx
	mov [fs:esi + SS_entry], ecx

	mov edx, 5
	invoke ISystem, ISystem.Save_GDT_entry
	mov [fs:esi + GS_entry + 4], ebx
	mov [fs:esi + GS_entry], ecx

	mov edx, [fs:esi + Next_thread]

	.Loop:
		mov ecx, edx
		and ecx, $FFFFF000
		invoke ISystem, ISystem.Get_module_thread_table

		mov esi, ebx
		mov ebx, edx
		and ebx, $FFF
		shl ebx, 6
		add esi, ebx

		cmp [fs:esi + State], dword Running
		je .Found
		mov edx, [fs:esi + Next_thread]
		jmp .Loop

	.Found:
	mov ebx, [fs:IThread]
	mov [fs:ebx + Var.Active_thread], edx

	.Return:
	mov edx, 3
	mov ebx, [fs:esi + DS_entry + 4]
	mov ecx, [fs:esi + DS_entry]
	test ecx, ecx
	jnz .Load_DS
	test ebx, ebx
	jz .Next
	.Load_DS:
	invoke ISystem, ISystem.Load_GDT_entry

	.Next:
	mov edx, 4
	mov ebx, [fs:esi + SS_entry + 4]
	mov ecx, [fs:esi + SS_entry]
	invoke ISystem, ISystem.Load_GDT_entry

	mov edx, 5
	mov ebx, [fs:esi + GS_entry + 4]
	mov ecx, [fs:esi + GS_entry]
	invoke ISystem, ISystem.Load_GDT_entry

	mov esp, [fs:esi + _ESP]
	mov ebx, [fs:IThread]

	mov cx, $8 * 3
	mov ds, cx
	add cl, $8 * 2
	mov gs, cx
	sub cl, $8
	mov ss, cx

	; Release lock
	btr dword [fs:ebx + Var.Lock], 0

	pop edi
	pop esi
	pop ebp
	pop edx
	pop ecx
	pop ebx
	pop eax
	popf
	ret

	.First_thread:
	mov edx, [fs:ebx + Var.First_thread]
	jmp .Loop

Procedure_IRQ0:
	push eax
	push ebx
	mov ebx, [fs:IThread]

	;inc dword [fs:ebx + Var.TimeCount]
	;cmp dword [fs:ebx + Var.TimeCount], 18
	;jb .Return

	;mov dword [fs:ebx + Var.TimeCount], 0

	; Get lock 0
	lock bts dword [fs:ebx + Var.Lock], 0
	jc .Return

	mov eax, [ss:esp + 4 * 2]
	mov [fs:ebx + Var.EIP], eax

	lea eax, [ebx + Procedure_Switch_Context]
	mov [ss:esp + 4 * 2], eax

	.Return:
	invoke IInterrupt, IInterrupt.Send_EOI

	pop ebx
	pop eax
	iret

Halt:
	hlt
	jmp Halt

Var:
	.First_thread dd 0
	.Active_thread dd 0
	.Lock dd 0
	.EIP dd 0
	.TimeCount dd 0
