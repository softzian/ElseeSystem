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

include 'include\Header.inc'
use32

; Function 1: Transmit (AdapterId : Cardinal; var Payload : Array of Byte; Size : Word)
; Function 2: Start_Receiver (AdapterId : Byte)
; Function 3: Stop_Receiver (AdapterId : Byte)
; Function 4: Set_Receive_Buffer (AdapterId : Byte; var Buffer : Array of Byte)

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Transmit
Header:
	.Driver_id dd $10EC8139
	dd $1
	dq $0

Var:
	.Data_space dd 0
	.Adapter_desc dd 0
	.Adapter_port dd 0
	.Adapter_rbuff dd 0
	.Adapter_tbuff dd 0

Function_Init:
	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, eax
	lea esi, [eax + Interface]

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 1
		jb .Loop

	mov [fs:ebx + Get_module_address.Imm], ebx

	; Register module
	mov [gs:ebp], dword $10EC8139
	mov [gs:ebp + 4], dword 1
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], ebx
	invoke ISystem, ISystem.Register_Module

	; Allocate address space
	mov [gs:ebp], dword $4000
	invoke ISystem, ISystem.Allocate

	mov esi, [ss:_Result]

	mov [gs:ebp], esi
	mov [gs:ebp + 4], dword 3
	invoke ISystem, ISystem.Add_address_space

	test eax, eax
	; Error handling here

	mov [fs:ebx + Var.Data_space], esi

	push ebx
	call Search_RTL8139
	call PCI_Config_RTL8139
	call Start_RTL8139
	pop ebx

	mov [fs:ebx + Var.Adapter_port], dword RTL_Port

	mov [fs:ebx + Var.Adapter_rbuff], esi
	mov [fs:ebx + Var.Adapter_desc], dword 0
	add esi, $3000
	mov [fs:ebx + Var.Adapter_tbuff], esi

	mov [gs:ebp], ebx
	mov [gs:ebp + 4], dword 1
	invoke INetwork, INetwork.Add_adapter

	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

Get_module_address:
	db $B8
	.Imm dd 0
	ret

Function_Transmit:	; Function 3
	.AdapterId equ dword [gs:ebp - 18] ; AdapterId : Card32
	.Dest_lo4 equ dword [gs:ebp - 14] ; Dest : MAC_address
	.Dest_hi2 equ word [gs:ebp - 10]
	.Payload equ dword [gs:ebp - 8] ; var Payload : Array of Byte
	.Size equ word [gs:ebp - 4] ; Size : Word
	.Protocol equ word [gs:ebp - 2] ; Protocol : Word

	push ebx
	push ecx
	push edx
	push esi
	push edi

	call Get_module_address
	mov ebx, eax

	push ebx
	invoke ISystem, ISystem.Set_ES_space

	; TSAD
	mov edx, [fs:ebx + Var.Adapter_port]
	mov eax, [fs:ebx + Var.Adapter_desc]
	shl al, 2
	add al, $20
	add edx, eax
	mov eax, [fs:ebx + Var.Adapter_tbuff]
	out dx, eax

	; Dest MAC addr
	mov eax, .Dest_lo4
	mov [es:$3000], eax
	mov ax, .Dest_hi2
	mov [es:$3004], ax

	; Src MAC addr
	push edx
	mov edx, [fs:ebx + Var.Adapter_port]
	in eax, dx
	mov [es:$3006], eax
	add edx, 2
	in eax, dx
	mov [es:$3008], eax
	pop edx

	; Ethertype
	mov ax, .Protocol
	mov [es:$300C], ax

	movzx ecx, .Size
	mov esi, .Payload
	mov edi, $300E
	rep movsb

	; TDS
	movzx eax, .Size
	sub edx, $10
	add eax, 14
	cmp eax, 60
	jae .j1
	mov eax, 60
	.j1:
	btr eax, 13
	bts eax, 16
	out dx, eax

	mov eax, [fs:ebx + Var.Adapter_desc]
	inc al
	cmp al, 4
	jb .j2
	xor al, al
	.j2: mov [fs:ebx + Var.Adapter_desc], eax

	invoke ISystem, ISystem.Set_ES_space
	add esp, 4

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

	restore .Dest_lo4
	restore .Dest_hi2
	restore .Payload
	restore .Size
	restore .Protocol
	restore .AdapterId
	restore .Driver

RTL_Port = $4000

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
	cmp ecx, $80FFF800 + $800
	je .Not_found
	jmp .Loop

	.Found:

	.Return:
	ret

	.Not_found:
	cli
	hlt

PCI_Config_RTL8139:
	; IOAR
	lea eax, [ecx + 4 * 4]
	Choose_PCI_Reg
	Read_PCI_Reg
	mov eax, RTL_Port + 1
	Write_PCI_Reg

	; BMAR
	lea eax, [ecx + $C * 4]
	Choose_PCI_Reg
	Read_PCI_Reg
	xor eax, eax
	Write_PCI_Reg

	; PCI Command
	lea eax, [ecx + 1 * 4]
	Choose_PCI_Reg
	mov dx, $CFC
	in ax, dx
	or al, 101b
	out dx, ax

	; ILR
	lea eax, [ecx + $F * 4]
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

	mov [gs:ebp], byte $2B
	mov eax, [ss:_ModuleIdx]
	add eax, Function_RTL8139_ISR
	mov [gs:ebp + 1], eax
	invoke IInterrupt, IInterrupt.Install_ISR

	mov [gs:ebp], byte $B
	invoke IInterrupt, IInterrupt.Enable_IRQ

	; Command
	mov dx, RTL_Port + $37
	mov al, 100b
	out dx, al
	in al, dx

	; RBSTART
	;mov dx, RTL_Port + $30
	;out dx, eax

	; RCR
	;mov dx, RTL_Port + $44
	;mov eax, $F
	;out dx, eax

	ret

Function_RTL8139_ISR:
	pusha
	push gs

	mov ax, 8 * 8
	mov gs, ax
	mov ebp, 16

	xor eax, eax
	mov dx, RTL_Port + $3E
	in ax, dx
	out dx, ax

	invoke IInterrupt, IInterrupt.Send_EOI

	pop gs
	popa
	iret

Calculate_IP_Checksum:
	push eax
	push ecx
	push edx

	xor eax, eax
	xor ecx, ecx
	.Loop1:
	mov dx, [fs:ebx + ecx * 2]
	xchg dl, dh
	add ax, dx
	adc ax, 0
	inc ecx
	cmp ecx, 10
	jb .Loop1

	xchg al, ah
	not ax
	mov [fs:ebx + 12], ax

	xor eax, eax
	mov ecx, 7
	.Loop2:
	mov dx, [fs:ebx + 20 + ecx * 2]
	xchg dl, dh
	add ax, dx
	adc ax, 0
	loop .Loop2
	mov dx, [fs:ebx + 20]
	xchg dl, dh
	add ax, dx
	adc ax, 0
	xchg al, ah
	not ax
	mov [fs:ebx + 20 + 2], ax

	pop edx
	pop ecx
	pop eax

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