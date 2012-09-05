; Loader2.asm - Early stage module loader v3 - PXE Boot
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
include 'include\Errcode.inc'

org $7C00

use16

PXE_Loader:
	call Procedure_PXE_Start

	push Var.Interrupt_Module
	push dword $10000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Memory_Module
	push dword $11000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Video_Module
	push dword $12000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Utility_Module
	push dword $13000
	call Function_Download_File
	test ax, ax
	jnz Abort

	call Procedure_PXE_Finish

Switch_to_Protected_Mode:
	cli
	call enable_A20
	xor ax, ax
	mov ds, ax
	lgdt [ds:GDT_Desc]
	mov eax, cr0
	or eax, 1
	mov cr0, eax
	jmp $8:Begin

include 'Loader3_p1.inc'
include 'Loader3_p2.inc'

Var:
	.Interrupt_Module db 9,0,'8259A.bin'
	.Memory_Module db 10,0,'Memory.bin'
	.Video_Module db 7,0,'VGA.bin'
	.Utility_Module db 11,0,'Utility.bin'

Halt:
	hlt
	jmp Halt

Abort:
	call Print_Failure
	jmp Halt

enable_A20:
	call .a20wait
	mov al, $AD
	out $64, al

	call .a20wait
	mov al, $D0
	out $64, al

	call .a20wait2
	in al, $60
	push ax

	call .a20wait
	mov al, $D1
	out $64, al

	call .a20wait
	pop ax
	or al, 2
	out $60, al

	call .a20wait
	mov al, $AE
	out $64, al

	call .a20wait
	ret

.a20wait:
	in al, $64
	test al, 2
	jnz .a20wait
	ret
.a20wait2:
	in al, $64
	test al, 1
	jz .a20wait2
	ret

GDT:
	gdt_null:
		dq 0
	gdt_code:
		dw $FFFF
		dw 0
		db 0
		db 10011010b
		db 11001111b
		db 0
	gdt_data:
		dw $FFFF
		dw 0
		db 0
		db 10010010b
		db 11001111b
		db 0
	gdt_end:
GDT_Desc:
	dw gdt_end - GDT
	dd GDT

use32

Begin:
	mov ax, $10
	mov ds, ax
	mov ss, ax
	mov es, ax
	mov esp, $400000

	mov eax, $10000
	call eax
	mov eax, $11000
	call eax
	mov eax, $12000
	call eax
	mov eax, $13000
	call eax

	sti

	call dword [IVideo.Clear_Screen]
	call Init_Memory_Table
	call Search_RTL8139
	call PCI_Config_RTL8139
	call Start_RTL8139

	push Var32.Text2
	call dword [IUtility.Write_String]
	call dword [IVideo.New_Line]

	call Calculate_IP_Checksum

	mov [esp-1], byte $20
	dec esp
	push Procedure_Timer_ISR
	call dword [IInterrupt.Install_ISR]

	mov [esp-1], byte 0
	dec esp
	call dword [IInterrupt.Enable_IRQ]

	.Loop:
	hlt
	jmp .Loop

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

Init_Memory_Table:
	push 0
	push $3FFFFF
	push 0
	call dword [IMemory.Create_Region]

	push $0
	push $7E00
	push $8DFF
	push $1
	call dword [IMemory.Mark_Memory]

	push $0
	push $11000
	push $11FFF
	push $2
	call dword [IMemory.Mark_Memory]

	push $0
	push $12000
	push $12FFF
	push $3
	call dword [IMemory.Mark_Memory]

	push $0
	push $13000
	push $13FFF
	push $4
	call dword [IMemory.Mark_Memory]

	ret

Print_Memory_Table:
	xor ecx, ecx

	.Loop:
	push dword ecx
	call dword [IUtility.Write_Cardinal_Hex]
	mov [esp-1], byte ' '
	dec esp
	call dword [IUtility.Write_Char]

	push dword [4+ecx]
	call dword [IUtility.Write_Cardinal_Hex]
	mov [esp-1], byte ' '
	dec esp
	call dword [IUtility.Write_Char]

	push dword [4+ecx+4]
	call dword [IUtility.Write_Cardinal_Hex]
	mov [esp-1], byte ' '
	dec esp
	call dword [IUtility.Write_Char]

	push dword [4+ecx+8]
	call dword [IUtility.Write_Cardinal_Hex]

	call dword [IVideo.New_Line]

	add ecx, 12
	cmp [4+ecx+4], dword 0
	jne .Loop
	call dword [IVideo.New_Line]
	ret

Function_Load_Module:
	.Offset equ dword [ebp+12]
	.Filename equ dword [ebp+8]

	enter 0, 0

	xor eax, eax

	.Return:
	leave
	ret 8
	restore .Offset
	restore .Filename

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