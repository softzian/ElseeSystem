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
;               DS_space : Address_space_index
;               ES_space : Address_space_index
;               SS_entry : GDT_entry
;               GS_entry : GDT_entry
;               ESP : Address
;               State : Cardinal
;               Next_thread : Thread_idx
;               Previous_thread : Thread_idx
;               Flag : Card32
;       End

IThread = $100010
; Function 1: New_Thread (StartPoint : Address) : Thread_idx
; Function 2: Start (ThreadIdx : Card32)
; Function 3: Yield
; Function 4: Block_self
; Function 5: Block (ThreadIdx : Card32)

; Function 6: Disable_context_switch
; Function 7: Enable_context_switch

Const:
	SizeOf_Thread_entry = 64
	NumOf_Thread_entries = 64
	SizeOf_Thread_table = SizeOf_Thread_entry * NumOf_Thread_entries

	DS_space = 0
	ES_space = 4
	SS_entry = 8
	GS_entry = 16
	_ESP = 24
	State = 28
	Next_thread = 32
	Previous_thread = 36
	Flag = 40

	Free = 0
	Running = 1
	Stop = 2
	Blocked = 3
	Kill = 4

Error_Code:
	ZERO_MODULE_INDEX = -1
	CAN_NOT_CREATE_STACK = -2
	NOT_EXISTED_ADDRESS_SPACE = -3

jmp near dword Function_Init
dd Header
Interface:
	dd Function_New_Thread
	dd Function_Start
	dd Function_Yield
	dd Function_Block_self
	dd 0

	dd Function_Disable_context_switch
	dd Function_Enable_context_switch
Header:

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:IThread], eax
	mov [fs:IThread + 4], esi

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 7
		jb .Loop

	cli

	; Install ISR for IRQ 0
	lea eax, [ebx + Procedure_IRQ0]
	mov [gs:ebp], byte 0
	mov [gs:ebp + 1], eax
	invoke IInterrupt, IInterrupt.Install_IRQ_direct_handler

	; Enable IRQ0, start threads
	mov [gs:ebp], byte 0
	invoke IInterrupt, IInterrupt.Enable_IRQ

	sti

	mov [gs:ebp], dword 5
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], dword ebx
	invoke ISystem, ISystem.Register_Module

	pop esi
	pop edi
	pop ebx
	ret

Lock_Thread_module:
	mov ebx, [fs:IThread]
	; Get lock 0
	.Spinlock:
		pause
		lock bts dword [fs:ebx + Var.Lock], 0
		jc .Spinlock
	ret

Function_New_Thread:	 ; Function 1
	.StartPoint equ dword [gs:ebp - 8] ; StartPoint : Address
	.Module_idx equ dword [gs:ebp - 4] ; Module_idx : Card32

	push ebp
	add ebp, 8

	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov esi, .Module_idx ; For now, esi is Module_idx
	mov ecx, esi
	invoke ISystem, ISystem.Get_module_thread_table

	;test eax, eax ; Error handling here

	mov edi, ecx
	test ecx, ecx
	jnz .Find_free_entry

	; This module doesn't have thread table, create one for it
	mov ecx, esi
	mov [gs:ebp], dword SizeOf_Thread_table
	mov [gs:ebp + 4], ecx
	invoke ISystem, ISystem.Allocate

	test eax, eax
	jnz .Error1	; Damn, run out of memory

	mov edi, [ss:_Result]
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
		add ecx, 64
		cmp ecx, SizeOf_Thread_table
		jb .Loop
		jmp .Error2	; Thread Table is full

	; Step 2 - New Thread
	.Found:
	add edi, ecx

	mov [gs:ebp], dword $1000	; Size
	mov [gs:ebp + 4], esi
	invoke ISystem, ISystem.Allocate    ; Allocate the GS stack
	test eax, eax
	jnz .Error3	; Can not allocate stack
	push dword [ss:_Result]

	mov [gs:ebp], dword $1000
	mov [gs:ebp + 4], esi
	invoke ISystem, ISystem.Allocate    ; Allocate the SS stack
	test eax, eax
	jnz .Error4	; Can not allocate stack

	; Create GDT entry for the SS stack
	mov ebx, [ss:_Result]
	add ebx, $1000
	mov eax, ebx
	mov [fs:edi + SS_entry + 2], ax
	shr eax, 16
	mov [fs:edi + SS_entry + 4], al
	mov [fs:edi + SS_entry + 7], ah
	mov [fs:edi + SS_entry], word 0
	mov [fs:edi + SS_entry + 5], word 1100000010010110b

	; Init data on the first stack
	mov eax, esi
	mov [fs:ebx - $14], eax
	shr ecx, 6
	add eax, ecx
	mov [fs:ebx - $18], eax ; ThreadIdx
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

	; Create GDT entry for the GS stack
	pop eax
	mov [fs:edi + GS_entry + 2], ax
	shr eax, 16
	mov [fs:edi + GS_entry + 4], al
	mov [fs:edi + GS_entry + 7], ah
	mov [fs:edi + GS_entry], word 0
	mov [fs:edi + GS_entry + 5], word 1100000010010010b

	mov [fs:edi + _ESP], dword ($FFFFFFFF - $44 + 1)
	mov [fs:edi + State], dword Blocked

	; Set the address space
	xor eax, eax
	mov [fs:edi + DS_space], eax
	mov [fs:edi + ES_space], eax

	; Step 3 - Finish
	call Lock_Thread_module

	; Insert thread to the round robin list
	mov edx, ecx
	add edx, esi
	mov [ss:_Result], edx	       ; Return ThreadIdx

	mov ecx, [fs:ebx + Var.First_thread]
	test ecx, ecx
	jz .First_thread

	mov [fs:edi + Next_thread], ecx

	mov esi, ecx
	and esi, $00000FFF
	and ecx, $FFFFF000
	invoke ISystem, ISystem.Get_module_thread_table

	shl esi, 6
	add ecx, esi

	mov esi, [fs:ecx + Previous_thread]
	mov [fs:edi + Previous_thread], esi
	mov [fs:ecx + Previous_thread], edx

	mov ecx, esi
	and esi, $00000FFF
	and ecx, $FFFFF000
	invoke ISystem, ISystem.Get_module_thread_table

	shl esi, 6
	add ecx, esi

	mov [fs:ecx + Next_thread], edx
	jmp .Release_Lock

	.First_thread:
	mov [fs:ebx + Var.First_thread], edx
	mov [fs:edi + Next_thread], edx
	mov [fs:edi + Previous_thread], edx

	.Release_Lock:
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
	restore .Module_idx

Set_thread_state:
	.ThreadIdx equ dword [gs:ebp - 8]  ; ThreadIdx : Card32
	.State equ dword [gs:ebp - 4] ; State : Card32

	push ebp
	add ebp, 8

	push ebx
	push ecx
	push edx

	mov edx, .ThreadIdx
	test edx, edx
	jz .Error1

	mov ecx, edx
	and ecx, $FFFFF000
	invoke ISystem, ISystem.Get_module_thread_table

	and edx, $FFF
	shl edx, 6
	add edx, ecx

	call Lock_Thread_module

	.Continue:
	mov eax, .State
	mov [fs:edx + State], eax

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
	restore .State

Function_Start: 	; Function 2
	.ThreadIdx equ dword [gs:ebp - 4]  ; ThreadIdx : Cardinal

	mov [gs:ebp + 4], dword Stop
	jmp Set_thread_state

	restore .ThreadIdx

Function_Yield:        ; Function 3
	push eax
	push ebx

	call Lock_thread_switch

	mov eax, [ss:esp + 8]
	mov [fs:ebx + Var.EIP], eax

	lea eax, [ebx + Procedure_Switch_Context]
	mov [ss:esp + 8], eax

	.Return:
	pop ebx
	pop eax
	ret

Function_Block_self:	; Function 4
	mov eax, [ss:_ThreadIdx]
	mov [gs:ebp], eax
	mov [gs:ebp + 4], dword Blocked
	jmp Set_thread_state

Lock_thread_switch:
	mov ebx, [fs:IThread]
	bts dword [fs:ebx + Var.Switch_lock], 0
	ret

Function_Disable_context_switch: ; Function 6
	push ebx
	call Lock_thread_switch
	pop ebx
	ret

Function_Enable_context_switch: ; Function 7
	mov eax, [fs:IThread]
	btr dword [fs:eax + Var.Switch_lock], 0
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

	mov edx, [fs:ebx + Var.Active_thread]
	test edx, edx
	jz .First_thread

	mov ecx, edx
	and ecx, $FFFFF000
	invoke ISystem, ISystem.Get_module_thread_table

	mov esi, ecx
	and edx, $FFF
	shl edx, 6
	add esi, edx

	mov [fs:esi + _ESP], esp
	mov eax, [fs:$4000 + 24]
	mov [fs:esi + DS_space], eax
	mov eax, [fs:$4000 + 28]
	mov [fs:esi + ES_space], eax

	.Save_SS_and_GS:
	mov edx, 5
	invoke ISystem, ISystem.Save_GDT_entry
	mov [fs:esi + SS_entry + 4], ebx
	mov [fs:esi + SS_entry], ecx

	mov edx, 6
	invoke ISystem, ISystem.Save_GDT_entry
	mov [fs:esi + GS_entry + 4], ebx
	mov [fs:esi + GS_entry], ecx

	cmp [fs:esi + State], dword Blocked
	je .j1
	mov [fs:esi + State], dword Stop
	.j1: mov edx, [fs:esi + Next_thread]

	.Loop:
		mov ecx, edx
		and ecx, $FFFFF000
		invoke ISystem, ISystem.Get_module_thread_table

		mov esi, ecx
		mov ecx, edx
		and ecx, $FFF
		shl ecx, 6
		add esi, ecx

		cmp [fs:esi + State], dword Stop
		je .Found
		mov edx, [fs:esi + Next_thread]
		jmp .Loop

	.Found:
	mov ebx, [fs:IThread]
	mov [fs:ebx + Var.Active_thread], edx

	push dword [fs:esi + DS_space]
	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	push dword [fs:esi + ES_space]
	invoke ISystem, ISystem.Set_ES_space
	add esp, 4

	.Load_SS_and_GS:
	mov edx, 5
	mov ebx, [fs:esi + SS_entry + 4]
	mov ecx, [fs:esi + SS_entry]
	invoke ISystem, ISystem.Load_GDT_entry

	mov edx, 6
	mov ebx, [fs:esi + GS_entry + 4]
	mov ecx, [fs:esi + GS_entry]
	invoke ISystem, ISystem.Load_GDT_entry

	mov dx, $8 * 6
	mov gs, dx
	mov dx, $8 * 5
	mov ss, dx
	mov esp, [fs:esi + _ESP]

	mov [fs:esi + State], dword Running

	; Release lock
	mov ebx, [fs:IThread]
	btr dword [fs:ebx + Var.Switch_lock], 0

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

	; Get switch lock
	bts dword [fs:ebx + Var.Switch_lock], 0
	jc .Return

	mov eax, [ss:esp + 4 * 2]
	mov [fs:ebx + Var.EIP], eax

	lea eax, [ebx + Procedure_Switch_Context]
	mov [ss:esp + 4 * 2], eax

	.Return:
	dec esp
	mov [ss:esp], byte 0
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
	.Switch_lock dd 1
	.EIP dd 0
	.TimeCount dd 0
