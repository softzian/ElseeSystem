; Network.asm - Network Interface Module
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

INetwork = $100C00
; Function 1: Add_adapter (Driver : Address; AdapterId : Cardinal)

; Function 4: Transmit (Adapter : Cardinal; Dest : MAC_address; Payload : Address; Size : Card16; Protocol : Card16)
; Function 5: Start_Receiver (Adapter : Handle)
; Function 6: Stop_Receiver (Adapter : Handle)

; Function 7: Add_recipient (AdapterIdx : Byte)
; Function 8: Receive_packet (AdapterIdx : Byte; var Packet : Array of Byte)

Const:
	NumOf_Adapters = 32

	Adapter_driver = 1
	Adapter_id = 5

	Func_Transmit = 0
	Func_Start_Receiver = 4
	Func_Stop_Receiver = 8
	Func_Set_Receive_Buffer = 12
	Func_Request_transmit = 0

Error_Code:
	ADAPTER_TABLE_PROBLEM = -1
	ADAPTER_NOT_EXISTED = -5

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Add_adapter
	dd Function_Transmit
	;dd Function_Start_Receiver
	;dd Function_Stop_Receiver
Header:

Function_Init:
	push ebx
	push edi
	push esi
	push edx

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:INetwork], eax

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 2
		jb .Loop

	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 6
	mov [gs:ebp + 16], dword ebx
	invoke ISystem, ISystem.Register_Module

	xor ecx, ecx
	invoke ISystem, ISystem.Get_address_space
	push ecx

	mov [gs:ebp], dword $1000
	invoke ISystem, ISystem.Allocate

	mov esi, [ss:_Result]

	mov [gs:ebp], esi
	mov [gs:ebp + 4], dword 0
	invoke ISystem, ISystem.Add_address_space

	mov edi, [ss:_Result]
	mov ecx, edi
	xor edx, edx
	invoke ISystem, ISystem.Set_address_space

	; Create adapter table
	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], dword 32
	mov [gs:ebp + 8], dword 32 * NumOf_Adapters
	invoke IData, IData.Create_table

	pop ecx
	xor edx, edx
	invoke ISystem, ISystem.Set_address_space

	test eax, eax
	; Error handling here

	pop edx
	pop esi
	pop edi
	pop ebx
	ret

Function_Add_adapter:	; Function 1
	.Driver equ dword [gs:ebp - 8] ; Driver : Address
	.AdapterId equ dword [gs:ebp - 4] ; AdapterId : Cardinal

	push ebp
	add ebp, 8

	push ebx
	push ecx
	push edx

	xor ecx, ecx
	invoke ISystem, ISystem.Get_address_space
	push ecx

	mov ecx, [fs:INetwork]
	xor edx, edx
	invoke ISystem, ISystem.Set_address_space

	mov [gs:ebp], dword 0
	invoke IData, IData.Add_table_entry

	test eax, eax
	jnz .Error1

	mov ebx, [ss:_Result + 4]

	mov eax, .Driver
	mov [ds:ebx + 1], eax
	mov eax, .AdapterId
	mov [ds:ebx + 5], eax

	pop ecx
	xor edx, edx
	invoke ISystem, ISystem.Set_address_space

	.Return:
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, ADAPTER_TABLE_PROBLEM
	jmp .Return

	restore .AdapterId
	restore .Driver

Function_Transmit:	; Function 3
	.Adapter equ dword [gs:ebp - 18] ; Adapter : Cardinal
	.Dest_lo4 equ dword [gs:ebp - 14] ; Dest : MAC_address
	.Dest_hi2 equ word [gs:ebp - 10] ;
	.Payload equ dword [gs:ebp - 8] ; Payload : Address
	.Size equ word [gs:ebp - 4] ; Size : Card16
	.Protocol equ word [gs:ebp - 2] ; Protocol : Card16

	push ebp
	add ebp, 18

	push ebx
	push ecx
	push edx

	xor ecx, ecx
	invoke ISystem, ISystem.Get_address_space
	push ecx

	mov ecx, [fs:INetwork]
	xor edx, edx
	invoke ISystem, ISystem.Set_address_space

	mov ecx, .Adapter
	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], ecx
	invoke IData, IData.Get_table_entry

	test eax, eax
	jnz .Error1

	mov ebx, [ss:_Result]
	mov eax, [ds:ebx + Adapter_id]
	mov .Adapter, eax

	pop ecx
	xor edx, edx
	invoke ISystem, ISystem.Set_address_space

	mov ecx, [ds:ebx + Adapter_driver]
	invoke2 ecx, Func_Transmit

	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, ADAPTER_NOT_EXISTED
	jmp .Return

	restore .Adapter
	restore .Dest_lo4
	restore .Dest_hi2
	restore .Payload
	restore .Size
	restore .Protocol

Var:
	.nAdap dd 0