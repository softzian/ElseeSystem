; RTL8139.asm - RTL8139 Driver Module
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

; Function 1: Transmit (AdapterId : Byte; var Payload : Array of Byte; Size : Word)
; Function 2: Start_Receiver (AdapterId : Byte)
; Function 3: Stop_Receiver (AdapterId : Byte)
; Function 4: Set_Receive_Buffer (AdapterId : Byte; var Buffer : Array of Byte)

jmp Function_Init
repeat 5-($-$$)
	db 0
end repeat

DriverId dd $10EC8139
Interface:
	dd Function_Transmit
	dd Function_Start_Receiver
	dd Function_Stop_Receiver


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

RTL_Port = $4000

Var32:
	.Text db 20,0,'Packet transmitted! '
	.Text3 db 17,0,'Packet received! '
	.Text2 db 5,0,'Done!'
	.Count db 36
	.Port dw RTL_Port + $20

macro Choose_PCI_Reg
{
	mov dx, $CF8
	out dx, eax
}
macro Read_PCI_Reg
{
	mov dx, $CFC
	in eax, dx
}
macro Write_PCI_Reg
{
	mov dx, $CFC
	out dx, eax
}

Search_RTL8139:
	mov ecx, $80000000
	.Loop:
	mov eax, ecx
	Choose_PCI_Reg
	Read_PCI_Reg
	cmp eax, $813910EC
	je .Found

	add ecx, $800
	cmp ecx, $80FFF800+$800
	je .Return
	jmp .Loop

	.Found:

	.Return:
	ret

PCI_Config_RTL8139:
	; IOAR
	lea eax, [ecx+4*4]
	Choose_PCI_Reg
	Read_PCI_Reg
	mov eax, $4001
	Write_PCI_Reg

	; BMAR
	lea eax, [ecx+$C*4]
	Choose_PCI_Reg
	Read_PCI_Reg
	xor eax, eax
	Write_PCI_Reg

	; PCI Command
	lea eax, [ecx+1*4]
	Choose_PCI_Reg
	mov dx, $CFC
	in ax, dx
	or al, 101b
	out dx, ax

	; IRL
	lea eax, [ecx+$F*4]
	Choose_PCI_Reg
	Read_PCI_Reg

	ret

Start_RTL8139:
	; Command
	mov dx, RTL_Port + $37
	xor eax, eax
	in al, dx
	bts eax, 4
	out dx, al

	.Loop:
	in al, dx
	mov ebx, eax
	bt ebx, 4
	jnz .Loop

	; 93C46
	mov dx, RTL_Port + $50
	xor eax, eax
	mov al, 11000000b
	out dx, al

	; CONFIG1
	mov dx, RTL_Port + $52
	in al, dx
	btr eax, 4
	btr eax, 0
	bts eax, 5
	out dx, al

	; 93C46
	mov dx, RTL_Port + $50
	xor eax, eax
	out dx, al

	; IMR
	mov dx, RTL_Port + $3C
	mov ax, $FFFF
	out dx, ax

	mov [esp-1], byte $25
	dec esp
	push Function_RTL8139_ISR
	call dword [IInterrupt.Install_ISR]

	mov [esp-1], byte 5
	dec esp
	call dword [IInterrupt.Enable_IRQ]

	; Command
	mov dx, RTL_Port + $37
	mov al, 1100b
	out dx, al
	in al, dx

	; Allocate
	sub esp, 4
	lea eax, [esp]
	push eax
	push $0
	push (8192 + 16)
	call dword [IMemory.Allocate]
	pop eax

	; RBSTART
	mov dx, RTL_Port + $30
	out dx, eax

	; RCR
	mov dx, RTL_Port + $44
	mov eax, $F
	out dx, eax

	ret

Function_RTL8139_ISR:
	pusha

	xor eax, eax
	mov dx, RTL_Port + $3E
	in ax, dx
	call Write_EAX
	out dx, ax

	call dword [IVideo.New_Line]

	call dword [IInterrupt.Send_EOI]
	popa
	iret

Procedure_Timer_ISR:
	pusha

	dec byte [Var32.Count]
	cmp byte [Var32.Count], 0
	jne .Return

	mov [Var32.Count], byte 36

	;cmp [Var32.Port], word (RTL_Port + $30)
	;jb .j1
	;mov [Var32.Port], word (RTL_Port + $20)
	;.j1:

	; Transmit descriptor
	;mov dx, [Var32.Port]
	;mov eax, Ethernet_Frame
	;out dx, eax

	; TDS
	;sub dl, $10
	;mov eax, $0038203C
	;out dx, eax

	;add [Var32.Port], word 4

	.Return:
	call dword [IInterrupt.Send_EOI]
	popa
	iret

Write_EAX:
	push eax
	push eax
	call dword [IUtility.Write_Cardinal_Hex]
	mov [esp-1], byte ' '
	dec esp
	call dword [IUtility.Write_Char]
	pop eax
	ret

Calculate_IP_Checksum:
	xor eax, eax
	xor ecx, ecx
	.Loop1:
	mov bx, [IP_Datagram+ecx*2]
	xchg bl, bh
	add ax, bx
	adc ax, 0
	inc ecx
	cmp ecx, 10
	jb .Loop1

	xchg al, ah
	not ax
	mov [IP_Datagram.Header_Checksum], ax

	xor eax, eax
	mov ecx, 7
	.Loop2:
	mov bx, [ICMP_Load+ecx*2]
	xchg bl, bh
	add ax, bx
	adc ax, 0
	loop .Loop2
	mov bx, [ICMP_Load]
	xchg bl, bh
	add ax, bx
	adc ax, 0
	xchg al, ah
	not ax
	mov [ICMP_Load.Checksum], ax

	ret

Ethernet_Frame:
	.MAC_Destination db $00,$A1,$B0,$00,$02,$D4
	.MAC_Source db $FF,$FF,$FF,$FF,$FF,$FF
	.Ethertype db $8, $00
IP_Datagram:
	.Version__IHL db $45
	.Type_of_Service db 0
	.Total_Length db $0,20+16
	.Identification db $19,$93
	.Flags__Fragment_Offset db $0,$0
	.Time_to_Live db 32
	.Protocol db 1
	.Header_Checksum dw 0
	.Source_Address db 192,168,100,100
	.Destination_Address db 192,168,100,40
ICMP_Load:
	.Type db 8
	.Code db 0
	.Checksum dw 0
	.Identifier dw 0
	.Sequence_Number dw 0
	.Data db 'PingPong'
Pad:
	db 10 dup 0