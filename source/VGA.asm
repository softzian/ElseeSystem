; VGA.asm - IVideo Standard VGA Module
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

IVideo = $100600
; Function 1: Write_Telex (var Text : Array of Char; Count : Word)
; Function 2: Clear_Display

Function_Init:
	push ebx
	push edi

	cld
	mov ebx, eax
	mov edi, IVideo
	stosd
	lea eax, [ebx+Function_Write_Telex]
	stosd
	lea eax, [ebx+Function_Clear_Display]
	stosd

	xor eax, eax
	pop edi
	pop ebx
	ret

Function_Write_Telex:
	.Text equ dword [ebp+10]
	.Count equ word [ebp+8]

	enter 0, 0
	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, [IVideo]

	mov ax, .Count
	cmp ax, 0
	je .Error1
	cmp ax, 2000
	ja .Error1
	add ax, [ebx+Var.Cursor]
	cmp ax, 2000
	jb .Write

	mov word [ebx+Var.Cursor], 0	; Reset cursor position if the remaining display space is not enough

	.Write:
	mov esi, .Text

	mov edi, $B8000
	xor eax, eax
	mov ax, [ebx+Var.Cursor]
	shl eax, 1
	add edi, eax	; EDI points to current Cursor position in Video memory

	xor ecx, ecx
	mov cx, .Count
	.Copy_Text_to_Video_mem:
	mov al, [esi]
	mov [edi], al
	mov byte [edi+1], 00001111b
	inc esi
	add edi, 2
	loop .Copy_Text_to_Video_mem

	.Update_Cursor_position:
	mov ax, .Count
	add [ebx+Var.Cursor], ax
	xor eax, eax

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 6
	.Error1:
	mov eax, INVALID_COUNT
	jmp .Return

	restore .Count
	restore .Text

Function_Clear_Display:
	push edi
	push ecx
	xor eax, eax
	mov edi, $B8000
	mov ecx, 1000
	rep stosd
	pop ecx
	pop edi
	ret

Var:
	.Cursor dw 0