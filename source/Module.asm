; Module.asm - Module Manager
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
;       ModuleEntry_Type = Record
;               ModuleId : Cardinal64;
;       End

IModule = $100000
; Function 1: Add_Module (var Entry : ModuleEntry_Type; var ModuleIdx : Cardinal)
; Function 2: Set_Active_Module (ModuleIdx : Cardinal)
; Function 3: Get_Active_Module (var ModuleIdx : Cardinal)
; Function 4: Set_Module_Table (var Table : Array)

Const:
	SizeOf_ModuleEntry = 8
	NumOf_ModuleEntries = 1023

Error_Code:


jmp Function_Init
Interface:
	.Add_Module dd Function_Add_Module
	.Set_Active_Module dd Function_Set_Active_Module
	.Get_Active_Module dd Function_Get_Active_Module
	.Set_Module_Table dd Function_Set_Module_Table

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, IModule
	lea esi, [eax + Interface]
	mov [edi], eax
	add edi, 4

	.Loop:
		lodsd
		add eax, ebx
		stosd
		cmp edi, IModule + 4 * 4
		jna .Loop

	pop esi
	pop edi
	pop ebx
	ret

Function_Add_Module:	; Function 1
	.Entry equ dword [ebp + 12]	; var Entry : ModuleEntry_Type
	.ModuleIdx equ dword [ebp + 8]	; var ModuleIdx : Cardinal

	push ebp
	mov ebp, esp

	push ebx
	push ecx
	push edx
	push esi

	mov ebx, [IModule]
	mov ebx, [ebx + Var.ModuleTable]
	mov esi, .Entry

	mov ecx, SizeOf_ModuleEntry
	.Loop:
		mov eax, [ebx + ecx]
		mov edx, [ebx + ecx + 4]
		cmp eax, [esi]
		jne .Next
		cmp edx, [esi + 4]
		je .Error1

		test eax, eax
		jnz .Next
		test edx, edx
		jz .Found

		.Next:
		add ecx, SizeOf_ModuleEntry
		cmp ecx, SizeOf_ModuleEntry * NumOf_ModuleEntries
		jbe .Loop
		jmp .Error2

	.Found:
	mov eax, [esi]
	mov edx, [esi + 4]
	mov [ebx + ecx], eax
	mov [ebx + ecx + 4], edx

	mov esi, .ModuleIdx
	mov eax, ecx
	xor edx, edx
	mov ebx, SizeOf_ModuleEntry
	div ebx
	mov [esi], eax

	xor eax, eax

	.Return:
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 4

	.Error1:
	mov eax, DUPLICATE_ENTRY
	jmp .Return
	.Error2:
	mov eax, TABLE_FULL
	jmp .Return

	restore .Entry
	restore .ModuleIdx

Function_Set_Active_Module:	; Function 2
	.ModuleIdx equ dword [ebp + 8]	; ModuleIdx : Cardinal

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push edx

	mov ebx, [IModule]
	mov ebx, [ebx + Var.ModuleTable]

	mov eax, .ModuleIdx
	test eax, eax
	jz .Error1
	cmp eax, NumOf_ModuleEntries
	ja .Error1	; Index out of range

	mov ecx, SizeOf_ModuleEntry
	mul ecx

	mov ecx, eax
	mov eax, [ebx + ecx]
	mov edx, [ebx + ecx + 4]

	test eax, eax
	jnz .Next
	test edx, edx
	jz .Error2	; ModuleId is 0

	.Next:
	mov ebx, [IModule]
	mov [ebx + Var.ActiveModule], ecx

	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx
	leave
	ret 4
	.Error1:
	mov eax, INDEX_OUT_OF_RANGE
	jmp .Return
	.Error2:
	mov eax, EMPTY_ENTRY

	restore .ModuleIdx

Function_Get_Active_Module:	; Function 3
	.ModuleIdx equ dword [ebp + 8]	; var ModuleIdx : Cardinal

	push ebp
	mov ebp, esp
	push ebx

	mov ebx, [IModule]
	mov eax, [ebx + Var.ActiveModule]
	mov ebx, .ModuleIdx
	mov [ebx], eax

	xor eax, eax
	.Return:
	pop ebx
	leave
	ret 4

	restore .ModuleIdx

Function_Set_Module_Table:
	.Table equ dword [esp + 4]

	push ebx

	mov ebx, [IModule]
	mov eax, .Table
	mov [ebx + Var.ModuleTable], eax

	.Return:
	pop ebx
	xor eax, eax
	ret 4

	restore .Table

Var:
	.ModuleTable dd 0
	.ActiveModule dd 0
