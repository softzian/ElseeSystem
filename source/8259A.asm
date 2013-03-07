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

IInterrupt = $100010
; Function 1: Install_ISR (INum : Byte; ISR_Entry : Address)
; Function 2: Install_IRQ_handler (IRQ : Byte; Entry_point : Address)
; Function 3: Install_IRQ_direct_handler (IRQ : Byte; Entry_point : Address)
; Function 4: Mask_all_IRQ
; Function 5: Enable_IRQ (IRQ : Byte)
; Function 6: Disable_IRQ (IRQ : Byte)
; Function 7: Send_EOI

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Install_ISR
	dd Function_Install_IRQ_handler
	dd Function_Install_IRQ_direct_handler
	dd Function_Mask_all_IRQ
	dd Function_Enable_IRQ
	dd Function_Disable_IRQ
	dd Function_Send_EOI

Header:

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:IInterrupt], eax
	mov [fs:IInterrupt + 4], esi

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 7
		jb .Loop

	mov esi, $7000
	xor eax, eax
	;.Loop2:
	;        mov [fs:esi + eax], dword $380000
	;        mov [fs:esi + eax + 4], dword $500
	;        add eax, 8
	;        cmp eax, $100
	;        jb .Loop2

	.Loop3:
		mov [fs:esi + eax], dword $80000
		mov [fs:esi + eax + 4], dword $E00
		add eax, 8
		cmp eax, $800
		jb .Loop3

	lidt [fs:ebx + IDTR]

	call Function_Init_PIC
	call Function_Mask_all_IRQ

	lea eax, [ebx + Procedure_INT_13]
	mov [gs:ebp], byte 13
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_3]
	mov [gs:ebp], byte $23
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_4]
	mov [gs:ebp], byte $24
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_5]
	mov [gs:ebp], byte $25
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_6]
	mov [gs:ebp], byte $26
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_7]
	mov [gs:ebp], byte $27
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_8]
	mov [gs:ebp], byte $28
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_9]
	mov [gs:ebp], byte $29
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_10]
	mov [gs:ebp], byte $2A
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_11]
	mov [gs:ebp], byte $2B
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_12]
	mov [gs:ebp], byte $2C
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_13]
	mov [gs:ebp], byte $2D
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_14]
	mov [gs:ebp], byte $2E
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	lea eax, [ebx + Procedure_IRQ_15]
	mov [gs:ebp], byte $2F
	mov [gs:ebp + 1], eax
	call Function_Install_ISR

	mov [gs:ebp], dword 5
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], dword ebx
	invoke IModule, IModule.Register_Module

	pop esi
	pop edi
	pop ebx
	ret

IDTR:
	.Limit dw 2047
	.Base dd $7000

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

Function_Install_ISR: ; Function 1
	.INum equ byte [gs:ebp - 5]
	.ISR_Entry equ dword [gs:ebp - 4]

	push ebp
	add ebp, 5
	push ebx

	mov ebx, $7000
	xor eax, eax
	mov al, .INum
	shl eax, 3
	add ebx, eax

	mov eax, .ISR_Entry
	mov [fs:ebx], ax
	bts word [fs:ebx + 4], 15
	shr eax, 16
	mov [fs:ebx + 6], ax

	xor eax, eax
	pop ebx

	pop ebp
	ret

	restore .INum
	restore .ISR_Entry

Function_Install_IRQ_handler: ; Function 2
	.IRQ equ byte [gs:ebp - 5]
	.Entry_point equ dword [gs:ebp - 4]

	push ebp
	add ebp, 5
	push ebx

	cmp .IRQ, 16
	jae .Error1

	mov ebx, [fs:IInterrupt]
	add ebx, IRQ_List
	movzx eax, .IRQ
	shl eax, 4
	lea ebx, [ebx + eax * 4]

	xor eax, eax
	.Loop:
		cmp [fs:ebx + eax], dword 0
		je .Found
		add eax, 4
		cmp eax, 16 * 4
		jb .Loop
		jmp .Error2

	.Found:
	add ebx, eax
	mov eax, .Entry_point
	mov [fs:ebx], eax

	xor eax, eax

	.Return:
	pop ebx
	pop ebp
	ret

	.Error1:
	mov eax, -1
	jmp .Return

	.Error2:
	mov eax, -2
	jmp .Return

	restore .IRQ
	restore .Entry_point

Function_Install_IRQ_direct_handler: ; Function 3
	.IRQ equ byte [gs:ebp]
	.Entry_point equ dword [gs:ebp + 1]

	cmp .IRQ, 2
	jae .Error1

	add .IRQ, $20
	call Function_Install_ISR
	ret

	restore .IRQ
	restore .Entry_point

	.Error1:
	mov eax, -1
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
	.IRQ equ byte [gs:ebp - 1]

	push ebp
	inc ebp

	push ebx
	push ecx

	mov ebx, [fs:IInterrupt]
	xor ecx, ecx
	mov cl, .IRQ

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

Function_Disable_IRQ:
	.IRQ equ byte [gs:ebp - 1]

	push ebp
	inc ebp

	push ebx
	push ecx

	mov ebx, [fs:IInterrupt]
	xor ecx, ecx
	mov cl, .IRQ

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
	.IRQ equ byte [ss:esp + 4]

	mov al, 100000b
	out $20, al

	cmp .IRQ, 8
	jb .Return

	out $A0, al

	.Return: ret 1

	restore .IRQ

Procedure_IRQ_3:
	pusha
	pushf
	mov eax, 3
	jmp Procedure_IRQ_Handler

Procedure_IRQ_4:
	pusha
	pushf
	mov eax, 4
	jmp Procedure_IRQ_Handler

Procedure_IRQ_5:
	pusha
	pushf
	mov eax, 5
	jmp Procedure_IRQ_Handler

Procedure_IRQ_6:
	pusha
	pushf
	mov eax, 6
	jmp Procedure_IRQ_Handler

Procedure_IRQ_7:
	pusha
	pushf
	mov eax, 7
	jmp Procedure_IRQ_Handler

Procedure_IRQ_8:
	pusha
	pushf
	mov eax, 8
	jmp Procedure_IRQ_Handler

Procedure_IRQ_9:
	pusha
	pushf
	mov eax, 9
	jmp Procedure_IRQ_Handler

Procedure_IRQ_10:
	pusha
	pushf
	mov eax, 10
	jmp Procedure_IRQ_Handler

Procedure_IRQ_11:
	pusha
	pushf
	mov eax, 11
	jmp Procedure_IRQ_Handler

Procedure_IRQ_12:
	pusha
	pushf
	mov eax, 12
	jmp Procedure_IRQ_Handler

Procedure_IRQ_13:
	pusha
	pushf
	mov eax, 13
	jmp Procedure_IRQ_Handler

Procedure_IRQ_14:
	pusha
	pushf
	mov eax, 14
	jmp Procedure_IRQ_Handler

Procedure_IRQ_15:
	pusha
	pushf
	mov eax, 15
	jmp Procedure_IRQ_Handler

Procedure_IRQ_Handler:
	dec esp
	mov [ss:esp], al

	mov ebx, 8 * 8
	mov ds, bx
	add bl, 16
	mov gs, bx
	mov ebp, 0

	mov ebx, [fs:IInterrupt]
	add ebx, IRQ_List
	shl eax, 6
	add ebx, eax

	xor eax, eax
	.Loop:
		mov ecx, [fs:ebx + eax]
		test ecx, ecx
		jz .Next

		push eax
		push ebx
		call ecx
		pop ebx
		pop eax

		.Next:
		add eax, 4
		cmp eax, 64
		jb .Loop

	.Return:
	mov eax, 3 * 8
	mov ds, ax
	add al, 24
	mov gs, ax

	call Function_Send_EOI

	popf
	popa
	iret

Copy_loop:
	mov al, [fs:ebx + ecx]
	mov [ds:$10 + ecx], al
	inc ecx
	cmp ecx, edx
	jb Copy_loop
	ret

macro Write str, x
{
	push ebx
	push ecx
	push edx

	mov eax, x
	push eax
	lea ebx, [str]

	movzx edx, byte [fs:ebx]
	inc ebx
	xor ecx, ecx
	call Copy_loop

	mov [gs:ebp], dword $10
	mov [gs:ebp + 4], dx
	invoke ICoreVideo, ICoreVideo.Write_Telex

	pop eax
	Write_register eax

	invoke ICoreVideo, ICoreVideo.New_Line

	pop edx
	pop ecx
	pop ebx
}

Procedure_INT_13:
	mov cx, 4 * 8
	mov ds, cx

	invoke ICoreVideo, ICoreVideo.Clear_Screen

	mov ecx, ebx
	mov ebx, [fs:IInterrupt]

	mov eax, [ss:esp + 4]
	Write ebx + Static.Text, eax
	Write ebx + Static.Text2, [ss:_ModuleIdx]
	Write ebx + Static.Text3, [ss:_ThreadIdx]
	Write ebx + Static.Text4, ecx

	mov ecx, esp
	.Loop:
		mov eax, [ss:ecx]
		Write_register eax
		add ecx, 4
		cmp ecx, _ThreadIdx
		jne .Loop

	invoke ICoreVideo, ICoreVideo.New_Line

	xor ecx, ecx
	.Loop2:
		mov eax, [gs:ecx]
		Write_register eax
		add ecx, 4
		cmp ecx, ebp
		jb .Loop2

	hlt

Static:
	.IRQMask1 db 0
	.IRQMask2 db 0
	.Text db 31,'General Protection Fault! EIP: '
	.Text2 db 11,'ModuleIdx: '
	.Text3 db 11,'ThreadIdx: '
	.Text4 db 5,'EBX: '
	.Text5 db 5,'EBP: '

IRQ_List:
	dd 256 dup 0