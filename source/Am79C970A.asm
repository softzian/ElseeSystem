; Am79C970A.asm - Am79C970A Driver Module
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
	.Driver_id dd $10222000
	dd $1
	dq $0

Var:
	.Data_space dd 0
	.Adapter_port dd 0
	.Adapter_IRQ db 0
	.Adapter_init_block dd 0
	.Adapter_PCI_address dd 0

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
	mov [gs:ebp], dword $10222000
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
	invoke ISystem, ISystem.Set_DS_space

	call Search_Am79C970A
	call PCI_Config_Am79C970A
	call Start_Am79C970A

	mov [fs:ebx + Var.Adapter_port], dword Port

	mov [gs:ebp], ebx
	mov [gs:ebp + 4], dword 1
	invoke INetwork, INetwork.Add_adapter

	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

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

Port = $6000

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

Search_Am79C970A:
	mov ecx, $80000000
	.Loop:
	mov eax, ecx
	Choose_PCI_Reg
	Read_PCI_Reg
	cmp eax, $20001022
	je .Found

	add ecx, $800
	cmp ecx, $80FFF800 + $800
	je .Not_found
	jmp .Loop

	.Found:
	mov [fs:ebx + Var.Adapter_PCI_address], ecx

	.Return:
	ret

	.Not_found:
	mov [fs:$B8000], byte '!'
	mov [fs:$B8001], byte 1010b
	cli
	hlt

PCI_Config_Am79C970A:
	; IOAR
	lea eax, [ecx + 4 * 4]
	Choose_PCI_Reg
	Read_PCI_Reg
	mov eax, Port + 1
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
	mov [fs:ebx + Var.Adapter_IRQ], al
	ret

Start_Am79C970A:
	; Reset
	mov edx, Port + $10
	xor eax, eax
	out dx, eax

	mov edx, Port + $18
	in eax, dx

	mov edx, Port + $10
	out dx, eax

	; CSR0 - Stop
	mov edx, Port + $14
	xor eax, eax
	out dx, eax

	mov edx, Port + $10
	mov eax, 4
	out dx, eax

	; BCR20
	mov edx, Port + $14
	mov eax, 20
	out dx, eax

	mov edx, Port + $1C
	mov eax, $103
	out dx, eax

	; CSR 1 & CSR 2 - Initialization Block Address
	mov edx, Port + $14
	mov eax, 1
	out dx, eax

	mov edx, Port + $10
	mov eax, esi
	and eax, $FFFF
	out dx, eax

	mov edx, Port + $14
	mov eax, 2
	out dx, eax

	mov edx, Port + $10
	mov eax, esi
	shr eax, 16
	out dx, eax

	; Create Initialization Block
	mov [fs:ebx + Var.Adapter_init_block], dword 0
	mov [ds:0], dword $80
	mov [ds:4], dword $29056100
	mov [ds:8], dword $000C
	mov [ds:$C], dword 0
	mov [ds:$10], dword 0
	mov [ds:$14], esi
	add [ds:$14], dword $20
	mov [ds:$18], esi
	add [ds:$18], dword $30

	; Receive descriptor
	mov [ds:$20], dword $0
	mov [ds:$24], dword $300F001
	mov [ds:$28], esi
	add [ds:$28], dword $1000
	mov [ds:$2C], dword 0

	; Transmit descriptor
	mov [ds:$30], dword $0
	mov [ds:$34], dword $300F000
	mov [ds:$38], esi
	add [ds:$38], dword $2000
	mov [ds:$3C], dword 0

	; CSR0 - Init
	mov edx, Port + $14
	xor eax, eax
	out dx, eax

	mov edx, Port + $10
	mov eax, 11b
	out dx, eax

	lea eax, [ebx + Procedure_IRQ_Handler]
	mov [gs:ebp + 1], eax
	mov al, [fs:ebx + Var.Adapter_IRQ]
	mov [gs:ebp], al
	invoke IInterrupt, IInterrupt.Install_IRQ_handler

	ret

Procedure_IRQ_Handler:
	pusha
	push gs

	mov ax, 8 * 8
	mov gs, ax
	mov ebp, 16

	xor eax, eax
	mov dx, Port + $3E
	in ax, dx
	out dx, ax

	invoke IInterrupt, IInterrupt.Send_EOI

	pop gs
	popa
	ret