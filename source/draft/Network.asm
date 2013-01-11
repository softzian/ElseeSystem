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

INetwork = $100030
; Function 1: Add_adapter (Driver : Address; AdapterId : Cardinal) : Card32

; Function 2: Transmit (Adapter : Card32; Dest : MAC_address; Payload : Address; Size : Card16; Protocol : Card16)

; Function 3: Add_receiver (Adapter : Card32; Protocol : Card16; Buffer : Address)
; Function 4: Receive_packet (Adapter : Card32; Protocol : Card16; Packet : Address; Size : Card16)

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
	RECEIVER_TABLE_FULL = -6

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Add_adapter
	dd Function_Transmit

	dd Function_Add_receiver
	dd Function_Receive_packet
Header:

Function_Init:
	push ebx
	push edi
	push esi
	push edx

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:INetwork], eax
	mov [fs:INetwork + 4], esi

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 4
		jb .Loop

	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 6
	mov [gs:ebp + 16], dword ebx
	invoke ISystem2, ISystem2.Register_Module

	mov [gs:ebp], dword $2000
	invoke ISystem2, ISystem2.Allocate

	mov esi, [ss:_Result]

	mov [gs:ebp], esi
	mov [gs:ebp + 4], dword 1
	invoke ISystem2, ISystem2.Add_address_space

	mov edi, [ss:_Result]
	push edi
	invoke ISystem, ISystem.Set_DS_space

	; Create adapter table
	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], dword 32
	mov [gs:ebp + 8], dword 32 * NumOf_Adapters
	invoke IData, IData.Create_table

	; Create receiver list
	xor eax, eax
	.Loop2:
		mov [ds:$1000 + eax], byte 0
		inc eax
		cmp eax, $A00
		jb .Loop2

	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

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

	push dword [fs:INetwork]
	invoke ISystem, ISystem.Set_DS_space

	mov [gs:ebp], dword 0
	invoke IData, IData.Add_table_entry

	test eax, eax
	jnz .Error1

	mov ebx, [ss:_Result + 4]

	mov eax, .Driver
	mov [ds:ebx + 1], eax
	mov eax, .AdapterId
	mov [ds:ebx + 5], eax

	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

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

	push dword [fs:INetwork]
	invoke ISystem, ISystem.Set_DS_space

	mov ecx, .Adapter
	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], ecx
	invoke IData, IData.Get_table_entry

	test eax, eax
	jnz .Error1

	mov ebx, [ss:_Result]
	mov eax, [ds:ebx + Adapter_id]
	mov .Adapter, eax

	mov ebx, [ds:ebx + Adapter_driver]

	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	invoke2 ebx, Func_Transmit

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

Function_Add_receiver:
	.Adapter equ dword [gs:ebp - 10] ; Adapter : Card32
	.Protocol equ word [gs:ebp - 6] ; Protocol : Card16
	.Buffer equ dword [gs:ebp - 4] ; Buffer : Address

	push ebp
	add ebp, 10

	push ecx
	push esi
	push edi

	push dword [fs:INetwork]
	invoke ISystem, ISystem.Set_DS_space

	xor ecx, ecx
	.Loop1:
		cmp [ds:$1000 + ecx], dword 0
		je .Found
		add ecx, 10
		cmp ecx, $A00
		jb .Loop1
		jmp .Error1

	.Found:
	mov eax, .Adapter
	mov [ds:$1000 + ecx], eax
	mov ax, .Protocol
	mov [ds:$1000 + ecx + 4], ax
	mov eax, .Buffer
	mov [ds:$1000 + ecx + 6], eax

	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	.Return:
	pop edi
	pop esi
	pop ecx

	pop ebp
	ret

	.Error1:
	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	mov eax, RECEIVER_TABLE_FULL
	jmp .Return

	restore .Adapter
	restore .Protocol
	restore .Buffer

Function_Receive_packet:
	; Parameters
	.Adapter equ dword [gs:ebp - 12] ; Adapter : Card32
	.Protocol equ word [gs:ebp - 8] ; Protocol : Card16
	.Packet equ dword [gs:ebp - 6] ; Packet : Address
	.Size equ word [gs:ebp - 2] ; Size : Card16

	push ebp
	add ebp, 12

	push ebx
	push ecx
	push edx
	push esi
	push edi

	push dword [fs:INetwork]
	invoke ISystem, ISystem.Set_DS_space

	xor ecx, ecx
	mov esi, .Adapter
	mov di, .Protocol
	.Loop1:
		cmp esi, [ds:$1000 + ecx]
		jne .Next1
		cmp di, [ds:$1000 + ecx + 4]
		jne .Next1

		mov edi, [ds:$1000 + ecx + 6]
		mov esi, .Packet
		movzx edx, .Size
		xor ebx, ebx
		.Loop2:
			mov al, [fs:esi + ebx]
			mov [fs:edi + 4 + ebx], al
			inc ebx
			cmp ebx, edx
			jb .Loop2
		mov esi, .Adapter
		mov di, .Protocol

		.Next1:
		add ecx, 10
		cmp ecx, $A00
		jb .Loop1

	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	restore .Adapter
	restore .Protocol
	restore .Packet
	restore .Size

Var:
	.nAdap dd 0