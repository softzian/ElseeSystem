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
use32

; Type declaration
;       ThreadEntry_Type = Record
;               ModuleIdx : Cardinal;
;               StackBase : Address;
;               StackSize : Cardinal;
;               StackPtr : Address;
;               State : Byte;
;       End

IThread = $100400
; Function 1: New_Thread (ModuleIdx : Cardinal; StartPoint : Address; StackSize : Cardinal; var ThreadIdx : Cardinal)
; Function 2: Start (ThreadIdx : Cardinal)
; Function 3: Yield
; Function 4: Pause1
; Function 5: Pause2 (ThreadIdx : Cardinal)
; Function 6: Get_Active_Thread (var ThreadIdx : Cardinal)

Const:
	SizeOf_ThreadEntry = 8
	NumOf_ThreadEntries = 1023

	StackBase = 4
	StackSize = 8
	StackPtr = 12
	State = 16

	Running = 0
	Stop = 1

Error_Code:
	ZERO_MODULE_INDEX = -1


jmp Function_Init
Interface:
	.New_Thread dd Function_New_Thread
	.Start dd Function_Start
	.Switch dd Function_Switch

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, IThread
	lea esi, [eax + Interface]
	mov [edi], eax
	add edi, 4

	.Loop:
		lodsd
		add eax, ebx
		stosd
		cmp edi, IThread + 4 * 3
		jna .Loop

	pop esi
	pop edi
	pop ebx
	ret

Function_NewThread:	; Function 1
	.ModuleIdx equ dword [ebp + 20] 	; ModuleIdx : Cardinal
	.StartPoint equ dword [ebp + 16]	; StartPoint : Address
	.StackSize equ dword [ebp + 12] 	; StackSize : Cardinal
	.ThreadIdx equ dword [ebp + 8]		; var ThreadIdx : Cardinal

	push ebp
	mov ebp, esp
	push ebx
	push edx
	push edi

	; Check ModuleIdx
	cmp .ModuleIdx, 0
	je .Error1	; Invalid ModuleIdx

	mov ebx, IThread

	; Get lock 0
	.Spinlock:
		lock bts dword [ebx + Var.Lock], 0
		jnc .Step1
		jmp .Spinlock

	; Step 1 - Found empty entry
	.Step1:
	mov eax, [ebx + Var.ThreadTable]
	mov edi, [eax]
	mov eax, SizeOf_ThreadEntry

	.Loop:
		cmp [edi + eax], dword 0
		je .Found
		add eax, SizeOf_ThreadEntry
		cmp eax, NumOf_ThreadEntries * SizeOf_ThreadEntry
		jbe .Loop
		jmp .Error2	; Thread Table is full

	; Step 2 - New Thread
	.Found:
	add edi, eax
	mov [edi], .ModuleIdx

	push 0
	lea eax, [edi + StackBase]
	push eax
	mov eax, .StackSize
	mov [edi + StackSize], eax
	push eax
	call dword [IMemory.Allocate]

	test eax, eax
	jnz .Error3	; Can not allocate stack

	; Init data on stack
	mov ebx, [edi + StackBase]
	add ebx, [edi + StackSize]
	pushf
	pop dword [ebx-4]
	mov ax, cs
	mov [ebx-8], eax
	mov eax, .StartPoint
	mov [ebx-12], eax
	; eax ebx ecx edx ebp esi edi
	xor eax, eax
	mov [ebx-16], eax
	mov [ebx-20], eax
	mov [ebx-24], eax
	mov [ebx-28], eax
	mov [ebx-32], eax
	mov [ebx-36], eax
	mov [ebx-40], eax

	lea eax, [ebx-40]
	mov [edi + StackPtr], eax

	mov [edi + State], byte Stop

	; Step 3 - Finish
	mov ebx, [IThread]
	mov eax, edi
	xor edx, edx
	sub eax, [ebx + Var.ThreadTable]

	mov ebx, SizeOf_ThreadEntry
	mov edi, .ThreadIdx
	div ebx
	mov [edi], eax		; Return ThreadIdx

	xor eax, eax

	.Release_Lock:
	mov ebx, [IThread]
	lock btr dword [ebx + Var.Lock], 0

	.Return:
	pop edi
	pop edx
	pop ebx
	leave
	ret 16
	.Error1:
	mov eax, ZERO_MODULE_INDEX
	jmp .Return
	.Error2:
	mov eax, TABLE_FULL
	jmp .Release_Lock

	restore .ModuleIdx
	restore .StartPoint
	restore .StackSize
	restore .ThreadIdx

Function_Start: 	; Function 2
	.ThreadIdx equ dword [ebp + 8]	; ThreadIdx : Cardinal

	push ebp
	mov ebp, esp
	push ebx

	mov eax, .ThreadIdx
	cmp eax, NumOf_ThreadEntry
	ja .Error1	; Index out of range
	mov ebx, SizeOf_ThreadEntry
	mul ebx

	; Get lock 0
	.Spinlock:
		lock bts dword [ebx + Var.Lock], 0
		jnc .Continue
		jmp .Spinlock

	.Continue:
	mov ebx, [IThread]
	mov ebx, [ebx + Var.ThreadTable]
	mov [ebx + eax + State], byte Running

	xor eax, eax

	.Release_Lock:
	mov ebx, [IThread]
	lock btr dword [ebx + Var.Lock], 0

	.Return:
	pop ebx
	leave
	ret 4
	.Error1:
	mov eax, INDEX_OUT_OF_RANGE
	jmp .Return

	restore .ThreadIdx


Function_Yield:        ; Function 3
	push ebx
	mov ebx, [IThread]

	; Get lock 0
	lock bts dword [ebx + Var.Lock], 0
	jc .Return

	xchg dword [ebx + Var.EIP], [esp + 4]
	pop ebx
	add esp, 4
	jmp Procedure_Switch_Context

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

	mov ebx, [IThread]
	mov eax, [ebx + Var.EIP]
	mov [esp + 8 * 4], eax

	mov eax, [ebx + Var.ActiveThread]
	mov esi, [ebx + Var.ThreadTable]
	mov edi, eax

	mov ecx, SizeOf_ThreadEntry
	mul ecx

	mov [esi + eax + StackPtr], esp

	add eax, ecx
	cmp eax, NumOf_ThreadEntry * SizeOf_ThreadEntry
	jae .Wrap
	cmp [esi + eax], dword 0
	je .Wrap

	inc dword [ebx + Var.ActiveThread]
	jmp .Return

	.Wrap:
	mov [ebx + Var.ActiveThread], dword 1
	mov eax, SizeOf_ThreadEntry

	.Return:
	mov esp, [esi + eax + StackPtr]
	lock btr dword [ebx + Var.Lock], 0

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
	push ebx
	mov ebx, [IThread]

	; Get lock 0
	lock bts dword [ebx + Var.Lock], 0
	jnc .Continue
	pop ebx
	call dword [IInterrupt.Send_EOI]
	iret


Var:
	.ThreadTable dd 0
	.ActiveThread dd 0
	.Lock dd 0
	.EIP dd 0
	.TimeCount dd 0
