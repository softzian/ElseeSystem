; 8042.ASM - IKeyboard 8042 Module
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

IKeyboard = $100400
; Function 1: Init_Keyboard

Function_Init:
	push ebx
	push edi

	mov ebx, eax
	mov edi, IKeyboard
	cld
	stosd
	lea eax, [ebx+Function_Init_Keyboard]
	stosd

	call Function_Init_Keyboard

	cli

	dec esp
	mov byte [esp], $21
	lea eax, [ebx+Procedure_IRQ1]
	push eax
	call dword [IInterrupt.Install_ISR]

	dec esp
	mov byte [esp], 1
	call dword [IInterrupt.Enable_IRQ]

	sti

	xor eax, eax
	pop edi
	pop ebx
	ret

Function_Init_Keyboard:
	.loop1:
	in al, $64
	test al, 10b
	jnz .loop1

	mov al, $AA
	out $64, al

	.loop2:
	in al, $64
	test al, 1
	jz .loop2

	in al, $60
	cmp al, $55
	jne .Keyboard_Fault

	mov al, $AB
	out $64, al

	.loop3:
	in al, $64
	test al, 1
	jz .loop3

	in al, $60
	cmp al, 0
	jne .Keyboard_Fault

	mov al, $60
	out $64, al

	.loop4:
	in al, $64
	test al, 10b
	jnz .loop4

	mov al, 00101001b
	out $60, al

	xor eax, eax
	ret
	.Keyboard_Fault:
	mov eax, -1
	ret

Procedure_IRQ1:
	pusha

	.loop1:
	in al, $64
	test al, 1
	jz .out1
	in al, $60

	mov [esp-1], al
	mov byte [esp-2], Format_Hex
	sub esp, 2
	call dword [ISysUtils.Write_Byte]

	dec esp
	mov byte [esp], ' '
	call dword [ISysUtils.Write_Char]

	jmp .loop1

	.out1:
	test al, 10b
	jnz .loop1

	call dword [IInterrupt.Send_EOI]
	popa
	iret