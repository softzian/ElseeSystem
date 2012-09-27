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

	pop esi
	pop edi
	pop ebx
	ret

Function_Install_ISR:
	.INum equ byte [ebp + 12]
	.ISR_Entry equ dword [ebp + 8]

	enter 0, 0
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
	leave
	ret 5
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
	.IRQNum equ byte [ebp + 8]

	enter 0, 0
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
	leave
	ret 1
	restore .IRQNum

Function_Send_EOI:
	mov al, 100000b
	out $20, al
	ret

Static:
	.IRQMask1 db 0
	.IRQMask2 db 0
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