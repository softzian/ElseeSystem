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
;               DS_space : Address_space
;               ES_space : Address_space
;               DS_entry : GDT_entry
;               ES_entry : GDT_entry
;               SS_entry : GDT_entry
;               GS_entry : GDT_entry
;               ESP : Address
;               State : Cardinal
;               Next_thread : Thread_idx
;               Next_thread_entry : Address
;               Previous_thread : Thread_idx
;               Previous_thread_entry : Address
;               Flag : Card32
;       End

IThread = $100018
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
	DS_entry = 8
	ES_entry = 16
	SS_entry = 24
	GS_entry = 32
	_ESP = 40
	State = 44
	Next_thread = 48
	Next_thread_entry = 52
	Previous_thread = 56
	Previous_thread_entry = 60

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

	mov [gs:ebp], dword 6
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], dword ebx
	invoke IModule, IModule.Register_Module

	pop esi
	pop edi
	pop ebx
	ret

Function_New_Thread:	 ; Function 1
	.StartPoint equ dword [gs:ebp - 16] ; StartPoint : Address
	.Module_idx equ dword [gs:ebp - 12] ; Module_idx : Card32
	.Stack1_size equ dword [gs:ebp - 8] ; Stack1_size : Card32
	.Stack2_size equ dword [gs:ebp - 4] ; Stack2_size : Card32

	push ebp
	add ebp, 16

	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov esi, .Module_idx ; For now, esi is Module_idx
	mov ecx, esi
	invoke IModule, IModule.Get_module_thread_table

	;test eax, eax ; Error handling here

	mov edi, ecx
	test ecx, ecx
	jnz .Find_free_entry

	; This module doesn't have thread table, create one for it
	mov ecx, esi
	mov [gs:ebp], dword SizeOf_Thread_table
	mov [gs:ebp + 4], ecx
	invoke IMemory, IMemory.Allocate

	test eax, eax
	jnz .Error1	; Damn, run out of memory

	mov edi, [ss:_Result]
	invoke IModule, IModule.Set_module_thread_table

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

	mov eax, .Stack2_size
	mov [gs:ebp], eax	; Size
	mov [gs:ebp + 4], esi
	invoke IMemory, IMemory.Allocate    ; Allocate the GS stack
	test eax, eax
	jnz .Error3	; Can not allocate stack
	push dword [ss:_Result]

	mov eax, .Stack1_size
	mov [gs:ebp], eax
	mov [gs:ebp + 4], esi
	invoke IMemory, IMemory.Allocate    ; Allocate the SS stack
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
	mov [fs:edi + DS_entry], dword $F0000000
	mov [fs:edi + DS_entry + 4], dword $FFC090FF
	mov [fs:edi + ES_entry], dword $F0000000
	mov [fs:edi + ES_entry + 4], dword $FFC090FF

	; Step 3 - Finish
	mov ebx, [fs:IThread]

	; Insert thread to the round robin list
	mov edx, ecx
	add edx, esi
	mov [ss:_Result], edx	       ; Return ThreadIdx

	mov ecx, [fs:ebx + Var.First_thread]
	test ecx, ecx
	jz .First_thread

	mov esi, [fs:ebx + Var.First_thread_entry]
	mov [fs:edi + Next_thread], ecx
	mov [fs:edi + Next_thread_entry], esi

	mov eax, [fs:esi + Previous_thread]
	mov [fs:edi + Previous_thread], eax
	mov eax, [fs:esi + Previous_thread_entry]
	mov [fs:edi + Previous_thread_entry], eax

	mov [fs:esi + Previous_thread], edx
	mov [fs:esi + Previous_thread_entry], edi

	mov [fs:eax + Next_thread], edx
	mov [fs:esi + Next_thread_entry], edi

	jmp .Done

	.First_thread:
	mov [fs:ebx + Var.First_thread], edx
	mov [fs:ebx + Var.First_thread_entry], edi

	mov [fs:edi + Next_thread], edx
	mov [fs:edi + Next_thread_entry], edi

	mov [fs:edi + Previous_thread], edx
	mov [fs:edi + Previous_thread_entry], edi

	.Done:
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
	jmp .Return

	.Error3:
	mov eax, CAN_NOT_CREATE_STACK
	jmp .Return

	.Error4:
	pop eax
	mov [gs:ebp], eax
	invoke IMemory, IMemory.Deallocate
	mov eax, CAN_NOT_CREATE_STACK
	jmp .Return

	restore .StartPoint
	restore .Module_idx
	restore .Stack1_size
	restore .Stack2_size

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
	invoke IModule, IModule.Get_module_thread_table

	and edx, $FFF
	shl edx, 6
	add edx, ecx

	mov eax, .State
	mov [fs:edx + State], eax

	xor eax, eax

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
	mov eax, [fs:IThread]
	bts dword [fs:eax + Var.Switch_lock], 0
	ret

Function_Enable_context_switch: ; Function 7
	mov eax, [fs:IThread]
	btr dword [fs:eax + Var.Switch_lock], 0
	ret

Procedure_Switch_Context:
	cli
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

	mov esi, [fs:ebx + Var.Active_thread_entry]

	mov [fs:esi + _ESP], esp

	invoke IModule, IModule.Save_system_state

	cmp [fs:esi + State], dword Blocked
	je .j1
	mov [fs:esi + State], dword Stop

	.j1:
	mov edx, [fs:esi + Next_thread]
	mov esi, [fs:esi + Next_thread_entry]

	.Loop:
		cmp [fs:esi + State], dword Stop
		je .Found
		mov edx, [fs:esi + Next_thread]
		mov esi, [fs:esi + Next_thread_entry]
		jmp .Loop

	.Found:
	mov [fs:ebx + Var.Active_thread], edx
	mov [fs:ebx + Var.Active_thread_entry], esi

	invoke IModule, IModule.Load_system_state

	mov ax, 2 * 8 + 4
	mov ss, ax
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
	sti
	ret

	.First_thread:
	mov edx, [fs:ebx + Var.First_thread]
	mov esi, [fs:ebx + Var.First_thread_entry]
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
	.First_thread_entry dd 0
	.Active_thread dd 0
	.Active_thread_entry dd 0
	.Switch_lock dd 1
	.EIP dd 0
	.TimeCount dd 0
