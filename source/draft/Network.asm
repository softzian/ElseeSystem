; Ethernet.asm - Ethernet Interface Module
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
	SizeOf_DriverId = 4
	NumOf_Drivers = 16
	NumOf_Adapters = 255

	Adapter_interface = 0
	Adapter_lock = 4

	Func_Transmit = 0
	Func_Start_Receiver = 4
	Func_Stop_Receiver = 8
	Func_Set_Receive_Buffer = 12
	Func_Request_transmit = 0

Error_Code:
	ADAPTER_TABLE_PROBLEM = -1
	ADAPTER_NOT_EXISTED = -5

jmp Function_Init
Interface:
	dd Function_Add_adapter
	dd Function_Transmit
	;dd Function_Start_Receiver
	;dd Function_Stop_Receiver

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, INetwork
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, INetwork + 4 * 2
		jna .Loop

	; Create adapter table
	mov [gs:ebp], dword 32
	mov [gs:ebp + 4], dword NumOf_Adapters
	invoke IData.Create_table

	test eax, eax
	; Error handling here

	mov eax, [ss:_Result]
	mov [fs:ebx + Var.Adapter_table], eax

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
	push esi

	mov ebx, [fs:INetwork]
	mov esi, [fs:ebx + Var.Adapter_table]

	mov [gs:ebp], esi
	invoke IData.Add_table_entry

	test eax, eax
	jnz .Error1

	mov ecx, [ss:_Result]
	mov [gs:ebp], esi
	mov [gs:ebp + 4], ecx
	invoke IData.Modify_table_entry

	mov ebx, [ss:_Result]
	mov eax, .Driver
	mov [ds:ebx], eax
	mov eax, .AdapterId
	mov [ds:ebx + 4], eax

	mov [gs:ebp], esi
	mov [gs:ebp + 4], ecx
	invoke IData.Finish_modify_table_entry

	xor eax, eax

	.Return:
	pop esi
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
	push esi

	mov ebx, [fs:INetwork]
	mov esi, [fs:ebx + Var.Adapter_table]

	mov ecx, .Adapter
	mov [gs:ebp], esi
	mov [gs:ebp + 4], ecx
	invoke IData.Read_table_entry

	test eax, eax
	jnz .Error1

	mov ebx, [ss:_Result]
	mov eax, [ds:ebx]
	mov [gs:ebp], eax
	mov eax, [ds:ebx + 4]
	mov [gs:ebp + 4], eax
	sub ebp, 14
	mov eax, [ds:ebx]
	mov eax, [fs:eax + 4]
	call dword [fs:eax]
	add ebp, 14

	mov [gs:ebp], esi
	mov [gs:ebp + 4], ecx
	invoke IData.Finish_read_table_entry

	.Return:
	pop esi
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
	.Adapter_table dd 0
	.Recipient_list dd 255 dup 0
	.nAdap dd 0