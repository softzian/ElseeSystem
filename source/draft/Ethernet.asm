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
; Function 2: Add_Adapter (Driver_interface : Address) : Byte

; Function 4: Transmit (AdapterIdx : Byte; Segment : Byte; Payload : Address; Size : Word)
; Function 5: Start_Receiver (AdapterIdx : Byte)
; Function 6: Stop_Receiver (AdapterIdx : Byte)

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
	ADAPTER_TABLE_IS_FULL = -1
	INVALID_DRIVER_ID = -2
	DUPLICATE_DRIVER = -3
	DRIVER_UNAVAILABLE = -4
	NOT_EXISTED_ADAPTER = -5

jmp Function_Init
Interface:
	dd Function_Install_Driver
	dd Function_Add_Adapter
	dd Function_Transmit
	dd Function_Start_Receiver
	dd Function_Stop_Receiver

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, IEthernet
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, IEthernet + 4 * 5
		jna .Loop



	pop esi
	pop edi
	pop ebx
	ret

Function_Install_Driver: ; Function 1

	push ebx
	push ecx
	push edx
	push esi

	mov ebx, [fs:IEthernet]
	add ebx, Var.DriverList

	xor edx, edx
	xor ecx, ecx

	mov eax, [ss:_ModuleIdx]
	mov eax, [fs:eax + 5]	; DriverId

	test eax, eax
	jz .Error2

	.Loop:
		mov esi, [fs:ebx + ecx]
		test esi, esi
		jnz .j1

		lea edx, [ds:ecx + 4]
		jmp .Next

		.j1:
		cmp eax, [fs:esi + 5]
		je .Error1	; Duplicate DriverId

		.Next:
		add ecx, 4
		cmp ecx, 4 * NumOf_Drivers
		jb .Loop

	mov eax, [ss:_ModuleIdx]
	mov [fs:ebx + edx - 4], eax
	shr dl, 2
	mov [ss:_Result], dl
	xor eax, eax

	.Return:
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

	.Error1:
	mov eax, DUPLICATE_DRIVER
	jmp .Return
	.Error2:
	mov eax, INVALID_DRIVER_ID

Function_Add_Adapter:	; Function 2
	.DriverIdx equ byte [gs:ebp - 1] ; DriverIdx : Byte

	push ebp
	inc ebp

	push ebx
	push ecx

	mov ebx, [fs:IEthernet]
	add ebx, Var.Adapter_table

	mov [gs:ebp], ebx
	invoke IData.Add_table_entry
	test eax, eax
	jnz .Error1

	mov [gs:ebp], ebx
	invoke IData.Access_table

	mov eax, [ss:_Result]
	mov [gs:ebp], ebx
	mov [gs:ebp + 4], eax
	invoke IData.Get_table_entry

	mov eax, [ss:_Result]
	mov cl, .DriverIdx
	mov [ds:ebx + eax], cl
	mov [ds:ebx + eax + 1], word 0

	mov [gs:ebp], ebx
	invoke IData.Finish_access_table

	.Return:
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, ADAPTER_TABLE_IS_FULL
	jmp .Return

	restore .DriverIdx

Function_Transmit:	; Function 3
	.AdapterIdx equ byte [gs:ebp - 8] ; AdapterIdx : Byte
	.Segment equ byte [gs:ebp - 7]	  ; Segment : Byte
	.Payload equ dword [gs:ebp - 6]   ; var Payload : Array of Byte
	.Size equ word [gs:ebp - 2]	  ; Size : Word

	push ebp
	add ebp, 8

	push ebx
	push ecx
	push esi

	mov ebx, [fs:IEthernet]
	mov esi, [fs:ebx + Var.Adapter_table]

	mov [gs:ebp], esi
	invoke IData.Access_table

	xor eax, eax
	mov al, .AdapterIdx

	mov [gs:ebp], esi
	mov [gs:ebp + 4], eax
	invoke IData.Get_table_entry

	test eax, eax
	jnz .Error1

	mov ecx, [ss:_Result]

	cmp [ds:esi + ecx + Adapter_driverIdx], byte 0
	je .Error1

	.Get_adapter_lock:
		lock bts word [ds:esi + ecx + Adapter_lock], 0
		jnc .Continue
		invoke IThread.Yield
		jmp .Get_adapter_lock

	.Continue:
	xor eax, eax
	mov al, [ds:esi + ecx +

	.Return:
	pop esi
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, ADAPTER_NOT_EXIST
	jmp .Return

	restore .AdapterIdx
	restore .Segment
	restore .Payload
	restore .Size

Function_Start_Receiver:	; Function 4
	.AdapterIdx equ byte [gs:ebp - 1]     ; AdapterIdx : Byte

	push ebp
	inc ebp

	push ebx

	mov ebx, [fs:IEthernet]
	xor eax, eax
	mov al, .AdapterIdx
	mov al, [fs:ebx + Var.AdapterList + eax - 1]

	test al, al
	jz .Error1

	shl eax, 2

	mov eax, dword [fs:ebx + Var.DriverList + eax - 4]
	pop ebx

	jmp dword [fs:eax + 5 + SizeOf_DriverId + Sub_Start_Receiver]

	.Error1:
	pop ebx
	mov eax, DRIVER_UNAVAILABLE
	pop ebp
	ret

	restore .AdapterIdx

Function_Stop_Receiver: 	; Function 5
	.AdapterIdx equ byte [gs:ebp - 1]     ; AdapterIdx : Byte

	push ebp
	inc ebp

	push ebx

	mov ebx, [fs:IEthernet]
	xor eax, eax
	mov al, .AdapterIdx
	mov al, [fs:ebx + Var.AdapterList + eax - 1]

	test al, al
	jz .Error1

	shl eax, 2

	mov eax, dword [fs:ebx + Var.DriverList + eax - 4]
	pop ebx

	jmp dword [fs:eax + 5 + SizeOf_DriverId + Sub_Stop_Receiver]

	.Error1:
	pop ebx
	mov eax, DRIVER_UNAVAILABLE
	pop ebp
	ret

	restore .AdapterIdx

Var:
	.DriverList db (4 * NumOf_Drivers) dup 0
	.Adapter_table dd 0
	.Recipient_list dd 255 dup 0