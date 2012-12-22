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

dd Function_Init
dd Interface

;DriverId dd $10EC8139
Interface:
	dd Function_Transmit

Adapter_desc = 8
Adapter_port = 0
Adapter_rbuff = 4
Adapter_tbuff = 12

Var:
	.Adapter_table dd 0

Function_Init:
	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, eax
	add [fs:eax + 4], eax
	lea esi, [eax + Interface]
	xor eax, eax

	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 4
		jb .Loop

	; Create adapter table
	mov [gs:ebp], dword 32
	mov [gs:ebp + 4], dword 4
	invoke IData.Create_table

	test eax, eax
	; Error handling here

	mov esi, [ss:_Result]
	mov [fs:ebx + Var.Adapter_table], esi

	mov [gs:ebp], esi
	invoke IData.Add_table_entry

	mov edi, [ss:_Result]

	push ebx
	call Search_RTL8139
	call PCI_Config_RTL8139
	call Start_RTL8139
	pop ebx

	mov [gs:ebp], esi
	mov [gs:ebp + 4], edi
	invoke IData.Modify_table_entry

	mov edx, [ss:_Result]
	mov [ds:edx + Adapter_port], dword RTL_Port

	; Allocate receive buffer
	mov [gs:ebp], dword $3000
	mov [gs:ebp + 4], dword 3
	invoke ISystem.Allocate_Code
	mov eax, [ss:_Result]

	mov [ds:edx + Adapter_rbuff], eax
	mov [ds:edx + Adapter_desc], dword 0

	; Allocate transmit buffer
	mov [gs:ebp], dword $1000
	mov [gs:ebp + 4], dword 3
	invoke ISystem.Allocate_Code
	mov eax, [ss:_Result]

	mov [ds:edx + Adapter_tbuff], eax

	mov [gs:ebp], esi
	mov [gs:ebp + 4], edi
	invoke IData.Finish_modify_table_entry

	mov [gs:ebp], ebx
	mov [gs:ebp + 4], edi
	invoke INetwork.Add_adapter

	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

Function_Transmit:	; Function 3
	.Dest_lo4 equ dword [gs:ebp - 22] ; Dest : MAC_address
	.Dest_hi2 equ word [gs:ebp - 18]
	.Payload equ dword [gs:ebp - 16] ; var Payload : Array of Byte
	.Size equ word [gs:ebp - 12] ; Size : Word
	.Protocol equ word [gs:ebp - 10] ; Protocol : Word
	.Driver equ dword [gs:ebp - 8] ; Driver : Address
	.AdapterId equ dword [gs:ebp - 4] ; AdapterId : Cardinal

	push ebp
	add ebp, 22
	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, .Driver
	mov esi, [fs:ebx + Var.Adapter_table]

	mov [gs:ebp], esi
	mov eax, .AdapterId
	mov [gs:ebp + 4], eax
	invoke IData.Modify_table_entry

	mov edi, [ss:_Result]

	; TSAD
	mov edx, [ds:edi + Adapter_port]
	mov eax, [ds:edi + Adapter_desc]
	shl al, 2
	add al, $20
	add edx, eax
	Write_register edx
	mov eax, [ds:edi + Adapter_tbuff]
	out dx, eax

	; Dest MAC addr
	mov ebx, eax
	Write_register ebx
	mov eax, .Dest_lo4
	mov [fs:ebx], eax
	mov ax, .Dest_hi2
	mov [fs:ebx + 4], ax

	; Src MAC addr
	push edx
	mov edx, [ds:edi + Adapter_port]
	in eax, dx
	mov [fs:ebx + 6], eax
	add edx, 2
	in eax, dx
	mov [fs:ebx + 8], eax
	pop edx

	; Ethertype
	mov ax, .Protocol
	mov [fs:ebx + 12], ax

	xor ecx, ecx
	.Loop:
		mov al, [fs:ebx + ecx]
		Write_reg_byte al
		inc ecx
		cmp ecx, 14
		jb .Loop

	mov eax, .Payload
	mov [gs:ebp], eax
	add ebx, 14
	mov [gs:ebp + 4], ebx
	movzx ecx, .Size
	mov [gs:ebp + 8], ecx
	invoke ISystem.Copy_data_to_code

	; TDS
	mov eax, ecx
	sub edx, $10
	add eax, 14
	cmp eax, 60
	jae .j1
	mov eax, 60
	.j1:
	btr eax, 13
	bts eax, 16
	out dx, eax

	mov eax, [ds:edi + Adapter_desc]
	inc al
	cmp al, 4
	jb .j2
	xor al, al
	.j2: mov [ds:edi + Adapter_desc], eax

	mov [gs:ebp], esi
	mov eax, .AdapterId
	mov [gs:ebp + 4], eax
	invoke IData.Finish_modify_table_entry

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop ebp
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
	invoke IInterrupt.Install_ISR

	mov [gs:ebp], byte $B
	invoke IInterrupt.Enable_IRQ

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

	xor eax, eax
	mov dx, RTL_Port + $3E
	in ax, dx
	Write_register eax
	out dx, ax

	invoke IInterrupt.Send_EOI
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

Function_Cardinal_to_HexStr_32:
	.Num equ dword [gs:ebp - 8]
	.HexStr equ dword [gs:ebp - 4]

	push ebp
	add ebp, 8
	push ebx
	push ecx
	push edx
	push edi

	mov edx, .Num
	xor ebx, ebx
	mov edi, .HexStr

	mov cl, 7
	.Loop:
	mov eax, edx
	shl cl, 2
	shr eax, cl
	shr cl, 2
	and al, $F

	cmp al, $A
	jae .j1
	add al, '0' - 0
	jmp .j2
	.j1: add al, 'A' - $A
	.j2: inc ebx

	mov [ds:edi + ebx - 1], al

	.Continue_loop:
	dec cl
	jns .Loop

	.Return:
	xor eax, eax
	pop edi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	restore .Num
	restore .HexStr