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
;               ModuleIdx : Cardinal;
;               Stack1Ptr : Address;
;               Stack2Ptr : Address;
;               State : Cardinal;
;       End

IThread = $100600
; Function 1: New_Thread (StartPoint : Address) : Thread_handle_type
; Function 2: Start (ThreadIdx : Thread_handle_type)
; Function 3: Yield
; Function 4: Block_self
; Function 5: Block (ThreadIdx : Thread_handle_type)
; Function 6: Get_Active_Thread : Thread_handle_type

Const:
	SizeOf_ThreadEntry = 16
	NumOf_ThreadEntries = 511
	SizeOf_Thread_Table = SizeOf_ThreadEntry * (NumOf_ThreadEntries + 1)

	ModuleIdx = 0
	Stack1Ptr = 4
	Stack2Ptr = 8
	State = 12

	Free = 0
	Running = 1
	Stop = 2
	Kill = 3

Error_Code:
	ZERO_MODULE_INDEX = -1
	CAN_NOT_CREATE_STACK = -2

jmp Function_Init
Interface:
	dd Function_New_Thread
	dd Function_Start
	dd Function_Yield
	dd Function_Block_self

Function_Init:
	push ebp
	mov ebp, [gs:0]

	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, IThread
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, IThread + 4 * 3
		jna .Loop

	; Allocate Thread Table
	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], dword SizeOf_Thread_Table
	add [gs:0], dword 8
	invoke ISystem.Allocate

	test eax, eax
	jnz Halt

	mov eax, [ss:_Result]
	mov [fs:ebx + Var.ThreadTable], eax

	; Fill zero Thread table
	mov esi, eax
	xor eax, eax
	xor edi, edi

	.Loop2:
		mov [ds:esi + edi], al
		inc edi
		cmp edi, SizeOf_Thread_Table
		jb .Loop2

	cli

	; Install ISR for IRQ 0
	lea eax, [ebx + Procedure_IRQ0]
	mov [gs:ebp], byte $20
	mov [gs:ebp + 1], eax
	add [gs:0], dword 5
	invoke IInterrupt.Install_ISR

	sti

	pop esi
	pop edi
	pop ebx

	pop ebp
	ret

Function_New_Thread:	 ; Function 1
	.StartPoint equ dword [gs:ebp - 4]	  ; StartPoint : Address

	push ebp
	mov ebp, [gs:0]

	push ebx
	push ecx
	push edi

	mov ebx, [fs:IThread]

	; Get lock 0
	.Spinlock:
		lock bts dword [fs:ebx + Var.Lock], 0
		jnc .Step1
		jmp .Spinlock

	; Step 1 - Found empty entry
	.Step1:
	mov edi, [fs:ebx + Var.ThreadTable]
	mov ecx, 2

	.Loop:
		cmp [ds:edi + ecx * 8 + State], dword Free
		je .Found
		add ecx, 2
		cmp ecx, NumOf_ThreadEntries * 2
		jbe .Loop
		jmp .Error2	; Thread Table is full

	; Step 2 - New Thread
	.Found:
	mov eax, [ss:_ModuleIdx]
	mov [ds:edi + ecx * 8 + ModuleIdx], eax

	; Allocate the stacks
	mov [gs:ebp], dword $6000	; Size
	mov [gs:ebp + 4], dword 2	; Type is stack
	add [gs:0], dword 8
	invoke ISystem.Allocate_Code

	test eax, eax
	jnz .Error3	; Can not allocate stack

	; Create LDT entry for the first stack
	mov ebx, [ss:_Result]
	add ebx, $2000
	mov eax, ebx
	shl eax, 16
	add eax, ($FFFF - 2)
	mov [fs:_LDT + ecx * 8], eax
	mov eax, ebx
	shr eax, 16
	mov [fs:_LDT + ecx * 8 + 4], al
	mov [fs:_LDT + ecx * 8 + 7], ah
	mov [fs:_LDT + ecx * 8 + 5], word 1100111110010110b

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
	mov [fs:ebx - $3C], eax
	mov [fs:ebx - $40], eax
	mov [fs:ebx - $44], eax

	; Create LDT entry for the second stack
	mov eax, ebx
	shl eax, 16
	add eax, 3
	mov [fs:_LDT + ecx * 8 + 8], eax
	mov eax, ebx
	shr eax, 16
	mov [fs:_LDT + ecx * 8 + 8 + 4], al
	mov [fs:_LDT + ecx * 8 + 8 + 7], ah
	mov [fs:_LDT + ecx * 8 + 8 + 5], word 1100000010010010b

	; Init data on the second stack
	lea eax, [ebx + 16]
	mov [fs:ebx], eax

	mov [ds:edi + ecx * 8 + Stack1Ptr], dword ($FFFFFFFF - $44 + 1)
	mov [ds:edi + ecx * 8 + State], dword Stop

	; Step 3 - Finish
	mov eax, $30
	lldt ax

	mov [ss:_Result], ecx	       ; Return ThreadIdx
	xor eax, eax

	.Release_Lock:
	mov ebx, [fs:IThread]
	lock btr dword [fs:ebx + Var.Lock], 0

	.Return:
	pop edi
	pop ecx
	pop ebx

	pop ebp
	sub [gs:0], dword 4
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

	restore .StartPoint

Function_Start: 	; Function 2
	.ThreadIdx equ dword [gs:ebp - 4]  ; ThreadIdx : Cardinal

	push ebp
	mov ebp, [gs:0]
	push ebx

	mov eax, .ThreadIdx
	cmp eax, NumOf_ThreadEntries * 2
	ja .Error1	; Index out of range
	test eax, eax
	jz .Error1

	mov ebx, [fs:IThread]
	; Get lock 0
	.Spinlock:
		lock bts dword [fs:ebx + Var.Lock], 0
		jnc .Continue
		jmp .Spinlock

	.Continue:
	mov ebx, [fs:ebx + Var.ThreadTable]
	mov [ds:ebx + eax * 8 + State], dword Running

	xor eax, eax

	.Release_Lock:
	mov ebx, [fs:IThread]
	lock btr dword [fs:ebx + Var.Lock], 0

	.Return:
	pop ebx

	pop ebp
	sub [gs:0], dword 4
	ret

	.Error1:
	mov eax, INDEX_OUT_OF_RANGE
	jmp .Return

	restore .ThreadIdx

Function_Yield:        ; Function 3
	push ebx
	mov ebx, [fs:IThread]

	; Get lock 0
	.Spinlock:
		lock bts dword [fs:ebx + Var.Lock], 0
		jnc .Continue
		jmp .Spinlock

	.Continue:
	mov eax, [ss:esp + 4]
	mov [fs:ebx + Var.EIP], eax

	lea eax, [ebx + Procedure_Switch_Context]
	mov [ss:esp + 4], eax

	.Return:
	pop ebx
	ret

Function_Block_self:	; Function 4
	push ebx
	mov ebx, [fs:IThread]

	; Get lock 0
	.Spinlock:
		lock bts dword [fs:ebx + Var.Lock], 0
		jnc .Continue
		jmp .Spinlock

	.Continue:
	mov ebx, [fs:ebx + Var.ThreadTable]
	mov eax, [ss:_ThreadIdx]
	mov [ds:ebx + eax * 8 + State], dword Stop

	.Release_Lock:
	mov ebx, [fs:IThread]
	lock btr dword [fs:ebx + Var.Lock], 0

	xor eax, eax

	.Return:
	pop ebx
	ret

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

	mov eax, [fs:ebx + Var.ActiveThread]
	mov esi, [fs:ebx + Var.ThreadTable]
	mov edi, eax

	mov [ds:esi + eax * 8 + Stack1Ptr], esp

	.Loop:
		add eax, 2
		cmp eax, NumOf_ThreadEntries * 2
		jae .Wrap

		cmp [ds:esi + eax * 8 + State], dword Running
		je .End_Loop
		jmp .Loop

		.Wrap:
		mov eax, 0
		jmp .Loop

		.End_Loop:

	mov [fs:ebx + Var.ActiveThread], eax

	.Return:
	mov ecx, eax
	shl ecx, 3
	add ecx, 4
	mov ss, cx
	mov esp, [ds:esi + eax * 8 + Stack1Ptr]

	add ecx, 8
	mov gs, cx

	; Release lock
	lock btr dword [fs:ebx + Var.Lock], 0

	pop edi
	pop esi
	pop ebp
	pop edx
	pop ecx
	pop ebx
	pop eax
	popf
	ret

Procedure_IRQ0:
	push eax
	push ebx
	mov ebx, [fs:IThread]

	inc dword [fs:ebx + Var.TimeCount]
	cmp dword [fs:ebx + Var.TimeCount], 18
	jb .Return

	mov dword [fs:ebx + Var.TimeCount], 0

	; Get lock 0
	lock bts dword [fs:ebx + Var.Lock], 0
	jc .Return

	mov eax, [ss:esp + 4 * 2]
	mov [fs:ebx + Var.EIP], eax

	lea eax, [ebx + Procedure_Switch_Context]
	mov [ss:esp + 4 * 2], eax

	.Return:
	invoke IInterrupt.Send_EOI

	pop ebx
	pop eax
	iret

Halt:
	hlt
	jmp Halt

Var:
	.ThreadTable dd 0
	.ActiveThread dd 0
	.Lock dd 0
	.EIP dd 0
	.TimeCount dd 0
