; 8259A.ASM - IInterrupt 8259A PIC Module
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

IInterrupt = $100400
; Function 1: Install_ISR (INum : Byte; ISR_Entry : Address)
; Function 2: Init_PIC
; Function 3: Mask_all_IRQ
; Function 4: Enable_IRQ (IRQNum : Byte)
; Function 5: Send_EOI

jmp Function_Init
Interface:
	.Install_ISR dd Function_Install_ISR
	.Init_PIC dd Function_Init_PIC
	.Mask_all_IRQ dd Function_Mask_all_IRQ
	.Enable_IRQ dd Function_Enable_IRQ
	.Send_EOI dd Function_Send_EOI

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, IInterrupt
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, IInterrupt + 4 * 5
		jna .Loop

	add [fs:ebx + IDTR.Base], ebx
	lidt [fs:ebx + IDTR]

	call Function_Init_PIC
	call Function_Mask_all_IRQ

	lea eax, [ebx + Procedure_INT_13]
	mov [gs:ebp], byte 13
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	pop esi
	pop edi
	pop ebx
	ret

Function_Install_ISR:
	.INum equ byte [gs:ebp - 5]
	.ISR_Entry equ dword [gs:ebp - 4]

	push ebp
	add ebp, 5
	push ebx

	mov ebx, [fs:IInterrupt]
	add ebx, IDT
	xor eax, eax
	mov al, .INum
	shl eax, 3
	add ebx, eax

	mov eax, .ISR_Entry
	mov [fs:ebx], ax
	mov byte [fs:ebx + 5], 10001110b
	shr eax, 16
	mov [fs:ebx + 6], ax

	mov ebx, [fs:IInterrupt]
	lidt [fs:ebx + IDTR]

	xor eax, eax
	pop ebx

	pop ebp
	ret

	restore .INum
	restore .ISR_Entry

Function_Init_PIC:
	mov al, $11
	out $20, al
	out $A0, al

	mov al, $20
	out $21, al
	mov al, $28
	out $A1, al

	mov al, 4
	out $21, al
	mov al, 2
	out $A1, al

	mov al, 1
	out $21, al
	out $A1, al

	xor eax, eax
	ret

Function_Mask_all_IRQ:
	push ebx

	mov ebx, [fs:IInterrupt]
	mov al, 11111011b
	mov [fs:ebx + Static.IRQMask1], al
	out $21, al
	mov al, 11111111b
	mov [fs:ebx + Static.IRQMask2], al
	out $A1, al

	pop ebx
	xor eax, eax
	ret

Function_Enable_IRQ:
	.IRQNum equ byte [gs:ebp - 1]

	push ebp
	inc ebp

	push ebx
	push ecx

	mov ebx, [fs:IInterrupt]
	xor ecx, ecx
	mov cl, .IRQNum

	mov ax, word [fs:ebx + Static.IRQMask1]
	btr ax, cx
	mov word [fs:ebx + Static.IRQMask1], ax

	out $21, al
	mov al, ah
	out $A1, al

	pop ecx
	pop ebx

	pop ebp
	ret

	restore .IRQNum

Function_Send_EOI:
	mov al, 100000b
	out $20, al
	ret

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

Procedure_INT_13:
	cli
	mov ecx, ebx
	mov ebx, [fs:IInterrupt]

	lea eax, [ebx + Static.Text]
	mov [gs:ebp], eax
	mov [gs:ebp + 4], dword $FFFE0000
	mov [gs:ebp + 8], dword 31
	invoke ISystem.Copy_code_to_data

	mov [gs:ebp], dword $FFFE0000
	mov [gs:ebp + 4], word 31
	invoke IVideo.Write_Telex

	mov eax, [ss:esp + 4]
	Write_register eax

	invoke IVideo.New_Line

	lea eax, [ebx + Static.Text2]
	mov [gs:ebp], eax
	mov [gs:ebp + 4], dword $FFFE0000
	mov [gs:ebp + 8], dword 11
	invoke ISystem.Copy_code_to_data

	mov [gs:ebp], dword $FFFE0000
	mov [gs:ebp + 4], word 11
	invoke IVideo.Write_Telex

	xor eax, eax
	mov ax, ss
	Write_register ecx

	hlt

Static:
	.IRQMask1 db 0
	.IRQMask2 db 0
	.Text db 'General Protection Fault! EIP: '
	.Text2 db 'ModuleIdx: '
IDT:
	repeat 256
		dw 0
		dw 8
		db 0
		db 00001110b
		dw 0
	end repeat
IDTR:
	.Limit dw 2047
	.Base dd IDT