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
use32

IEthernet = $100C00
; Function 1: Install_Driver (ModuleAddr : Address; var DriverIdx : Byte)
; Function 2: Add_Adapter (DriverIdx : Byte; var AdapterIdx : Byte)
; Function 3: Transmit (AdapterIdx : Byte; var Payload : Array of Byte; Size : Word)
; Function 4: Start_Receiver (AdapterIdx : Byte)
; Function 5: Stop_Receiver (AdapterIdx : Byte)

Const:
	SizeOf_DriverId = 4
	NumOf_Drivers = 16
	NumOf_Adapters = 255

	Sub_Transmit = 0
	Sub_Start_Receiver = 4
	Sub_Stop_Receiver = 8
	Sub_Set_Receive_Buffer = 12
Error_Code:
	ADAPTER_TABLE_IS_FULL = TABLE_FULL
	INVALID_DRIVER_ID = -1
	DUPLICATE_DRIVER = DUPLICATE_ENTRY
	DRIVER_UNAVAILABLE = SUBINTERFACE_UNAVAILABLE

jmp Function_Init
Interface:
	.Install_Driver dd Function_Install_Driver
	.Add_Adapter dd Function_Add_Adapter
	.Transmit dd Function_Transmit
	.Start_Receiver dd Function_Start_Receiver
	.Stop_Receiver dd Function_Stop_Receiver

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
	.ModuleAddr equ dword [ebp + 12]	; ModuleAddr : Address
	.DriverIdx equ dword [ebp + 8]		; var DriverIdx : Byte

	push ebp
	mov ebp, esp

	push ebx
	push ecx
	push edx
	push esi

	mov ebx, [fs:IEthernet]
	add ebx, Var.DriverList

	xor edx, edx
	xor ecx, ecx

	mov eax, .ModuleAddr	; TO DO: Check ModuleAddr?
	mov eax, [fs:eax + 5]

	test eax, eax
	jz .Error2	; DriverId is 0

	.Loop:
		mov esi, [fs:ebx + ecx]
		test esi, esi
		jnz .j1

		lea edx, [ecx + 4]
		jmp .Next

		.j1:
		cmp eax, [fs:esi + 5]
		je .Error1	; Duplicate DriverId

		.Next:
		add ecx, 4
		cmp ecx, 4 * NumOf_Drivers
		jb .Loop

	mov eax, .ModuleAddr
	mov [fs:ebx + edx - 4], eax
	shr dl, 2
	mov ebx, .DriverIdx
	mov [ebx], dl
	xor eax, eax

	.Return:
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret (4 + 4)
	.Error1:
	mov eax, DUPLICATE_DRIVER
	jmp .Return
	.Error2:
	mov eax, INVALID_DRIVER_ID

	restore .ModuleAddr
	restore .DriverIdx

Function_Add_Adapter:	; Function 2
	.DriverIdx equ byte [ebp + 12] ; DriverIdx : Byte
	.AdapterIdx equ dword [ebp + 8]    ; var AdapterIdx : Byte

	push ebp
	mov ebp, esp

	push ebx

	mov ebx, [fs:IEthernet]
	add ebx, Var.AdapterList
	xor eax, eax

	.Loop:
		cmp [fs:ebx + eax], byte 0
		je .Found
		inc al
		cmp al, NumOf_Adapters
		jae .Error1
		jmp .Loop

	.Found:
	add ebx, eax
	mov ah, .DriverIdx
	mov [fs:ebx], ah

	mov ebx, .AdapterIdx
	inc al
	mov [fs:ebx], al

	xor eax, eax

	.Return:
	pop ebx
	leave
	ret (1 + 4)
	.Error1:
	mov eax, ADAPTER_TABLE_IS_FULL
	jmp .Return

	restore .DriverIdx
	restore .AdapterIdx

Function_Transmit:	; Function 3
	.AdapterIdx equ byte [ebp + 14] ; AdapterIdx : Byte
	.Payload equ dword [ebp + 10]	; var Payload : Array of Byte
	.Size equ word [ebp + 8]	; Size : Word

	push ebp
	mov ebp, esp

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

	jmp dword [fs:eax + 5 + SizeOf_DriverId + Sub_Transmit]

	.Error1:
	pop ebx
	mov eax, DRIVER_UNAVAILABLE
	leave
	ret (1 + 4 + 2)

	restore .AdapterIdx
	restore .Payload
	restore .Size

Function_Start_Receiver:	; Function 4
	.AdapterIdx equ byte [ebp + 8]	   ; AdapterIdx : Byte

	push ebp
	mov ebp, esp

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
	leave
	ret 1

	restore .AdapterIdx

Function_Stop_Receiver: 	; Function 5
	.AdapterIdx equ byte [ebp + 8]	   ; AdapterIdx : Byte

	push ebp
	mov ebp, esp

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
	leave
	ret 1

	restore .AdapterIdx

Var:
	.DriverList db (4 * NumOf_Drivers) dup 0
	.AdapterList db 255 dup 0