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
; Function 1: Allocate_SubInterface (InterfaceId : Word; var Interface : SubInterface_Type; var InterfaceIdx : Byte)
; Function 2: Add_Adapter (InterfaceIdx : Byte; var AdapterId : Byte)
; Function 3: Transmit (AdapterId : Byte; var Payload : Array of Byte; Size : Word)
; Function 4: Start_Receiver (AdapterId : Byte)
; Function 5: Stop_Receiver (AdapterId : Byte)
; Function 6: Set_Receive_Buffer (AdapterId : Byte; var Buffer : Array of Byte)

Const:
	SizeOf_SubInterface = SizeOf_InterfaceId + 4 * 4
	SizeOf_InterfaceId = 2
	NumOf_SubInterfaces = 16
	NumOf_Adapters = 255

	Sub_Transmit = 0
	Sub_Start_Receiver = 4
	Sub_Stop_Receiver = 8
	Sub_Set_Receive_Buffer = 12
Error_Code:
	ADAPTER_TABLE_IS_FULL = 17

Function_Init:
	push ebx
	push edi

	mov ebx, eax
	mov edi, IEthernet
	mov [edi], eax
	lea eax, [ebx+Function_Allocate_SubInterface]
	mov [edi+4], eax
	lea eax, [ebx+Function_Add_Adapter]
	mov [edi+8], eax
	lea eax, [ebx+Function_Transmit]
	mov [edi+12], eax
	lea eax, [ebx+Function_Start_Receiver]
	mov [edi+16], eax
	lea eax, [ebx+Function_Stop_Receiver]
	mov [edi+20], eax
	lea eax, [ebx+Function_Set_Receive_Buffer]
	mov [edi+24], eax

	xor eax, eax
	pop edi
	pop ebx
	ret

Function_Allocate_SubInterface: ; Function 1
	.InterfaceId equ word [ebp+16]	; InterfaceId : Word
	.Interface equ dword [ebp+12]	; var Interface : SubInterface_Type
	.InterfaceIdx equ dword [ebp+8] ; var InterfaceIdx : Byte

	push ebp
	mov ebp, esp

	push ebx
	push ecx
	push edx

	mov ebx, [IEthernet]
	add ebx, Var.SubInterfaceTable
	xor edx, edx

	xor ecx, ecx
	mov ax, .InterfaceId
	.Loop:
		cmp ax, [ebx+ecx]
		je .Error1	; Duplicate InterfaceId

		test ax, ax
		jnz .Next

		test edx, edx
		jnz .Next

		lea edx, [ecx+SizeOf_SubInterface]

		.Next:
		add ecx, SizeOf_SubInterface
		cmp ecx, SizeOf_SubInterface*NumOf_SubInterfaces
		jb .Loop

	lea ebx, [ebx+edx-SizeOf_SubInterface]
	mov [ebx], ax
	mov eax, .Interface
	mov [eax], ebx
	mov eax, .InterfaceIdx
	mov [eax], dl
	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx
	leave
	ret (2 + 4 + 4)
	.Error1:
	mov eax, DUPLICATE_INTERFACE
	jmp .Return

	restore .InterfaceId
	restore .Interface
	restore .InterfaceIdx

Function_Add_Adapter:	; Function 2
	.InterfaceIdx equ byte [ebp+12] ; InterfaceIdx : Byte
	.AdapterId equ dword [ebp+8]	; var AdapterId : Byte

	push ebp
	mov ebp, esp

	push ebx

	mov ebx, [IEthernet]
	add ebx, Var.AdapterList
	xor eax, eax

	.Loop:
		cmp [ebx+eax], byte 0
		je .Found
		inc al
		cmp al, NumOf_Adapters
		je .Error1
		jmp .Loop

	.Found:
	add ebx, eax
	mov ah, .InterfaceIdx
	mov [ebx], ah
	mov ebx, .AdapterId
	inc al
	mov [ebx], al
	xor eax, eax

	.Return:
	pop ebx
	leave
	ret (1 + 4)
	.Error1:
	mov eax, ADAPTER_TABLE_IS_FULL
	jmp .Return

	restore .InterfaceIdx
	restore .AdapterId

Function_Transmit:	; Function 3
	.AdapterId equ byte [ebp+14]	; AdapterId : Byte
	.Payload equ dword [ebp+10]	; var Payload : Array of Byte
	.Size equ word [ebp+8]		; Size : Word

	push ebp
	mov ebp, esp

	push ebx
	mov ebx, [IEthernet]
	xor eax, eax
	mov al, .AdapterId
	mov al, [ebx+Var.AdapterList+eax-1]

	test al, al
	je .Error1

	mov bl, SizeOf_SubInterface
	mul bl
	mov ebx, [IEthernet]
	mov eax, dword [ebx + Var.SubInterfaceTable + eax - SizeOf_SubInterface + SizeOf_InterfaceId + Sub_Transmit]
	pop ebx

	jmp dword [eax]

	.Error1:
	pop ebx
	mov eax, INTERFACE_UNAVAILABLE
	leave
	ret (1 + 4 + 2)

	restore .AdapterId
	restore .Payload
	restore .Size

Function_Start_Receiver:	; Function 4
	.AdapterId equ byte [ebp+8]	; AdapterId : Byte

	push ebp
	mov ebp, esp

	push ebx
	mov ebx, [IEthernet]
	xor eax, eax
	mov al, .AdapterId
	mov al, [ebx+Var.AdapterList+eax-1]

	test al, al
	je .Error1

	mov bl, SizeOf_SubInterface
	mul bl
	mov ebx, [IEthernet]
	mov eax, dword [ebx + Var.SubInterfaceTable + eax - SizeOf_SubInterface + SizeOf_InterfaceId + Sub_Start_Receiver]
	pop ebx

	jmp dword [eax]

	.Error1:
	pop ebx
	mov eax, INTERFACE_UNAVAILABLE
	leave
	ret 1

	restore .AdapterId

Function_Stop_Receiver: ; Function 5
	.AdapterId equ byte [ebp+8]	; AdapterId : Byte

	push ebp
	mov ebp, esp

	push ebx
	mov ebx, [IEthernet]
	xor eax, eax
	mov al, .AdapterId
	mov al, [ebx+Var.AdapterList+eax-1]

	test al, al
	je .Error1

	mov bl, SizeOf_SubInterface
	mul bl
	mov ebx, [IEthernet]
	mov eax, dword [ebx + Var.SubInterfaceTable + eax - SizeOf_SubInterface + SizeOf_InterfaceId + Sub_Stop_Receiver]
	pop ebx

	jmp dword [eax]

	.Error1:
	pop ebx
	mov eax, INTERFACE_UNAVAILABLE
	leave
	ret 1

	restore .AdapterId

Function_Set_Receive_Buffer:
	.AdapterId equ byte [ebp+12]	; AdapterId : Byte
	.Buffer equ dword [ebp+8]	; var Buffer : Array of Byte

	push ebp
	mov ebp, esp

	push ebx
	mov ebx, [IEthernet]
	xor eax, eax
	mov al, .AdapterId
	mov al, [ebx+Var.AdapterList+eax-1]

	test al, al
	je .Error1

	mov bl, SizeOf_SubInterface
	mul bl
	mov ebx, [IEthernet]
	mov eax, dword [ebx + Var.SubInterfaceTable + eax - SizeOf_SubInterface + SizeOf_InterfaceId + Sub_Set_Receive_Buffer]
	pop ebx

	jmp dword [eax]

	.Error1:
	pop ebx
	mov eax, INTERFACE_UNAVAILABLE
	leave
	ret (1 + 4)

	restore .AdapterId
	restore .Buffer

Var:
	.SubInterfaceTable db (SizeOf_SubInterface * NumOf_SubInterfaces) dup 0
	.AdapterList db 255 dup 0