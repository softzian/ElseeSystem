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
; Function 2: Clear_Screen
; Function 3: Set_Cursor (x, y : Byte)
; Function 4: Get_Cursor (var x, var y : Byte)
; Function 5: New_Line
; Function 6: Move_Cursor (Step : Integer16)

Function_Init:
	push ebx
	push edi

	mov ebx, eax
	mov edi, IVideo
	mov [edi], eax
	lea eax, [ebx+Function_Write_Telex]
	mov [edi+4], eax
	lea eax, [ebx+Function_Clear_Screen]
	mov [edi+8], eax
	lea eax, [ebx+Function_Set_Cursor]
	mov [edi+12], eax
	lea eax, [ebx+Function_Get_Cursor]
	mov [edi+16], eax
	lea eax, [ebx+Function_New_Line]
	mov [edi+20], eax
	lea eax, [ebx+Function_Move_Cursor]
	mov [edi+24], eax

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
	mov byte [edi+1], 00001111b
	mov [edi], al
	inc esi
	add edi, 2
	loop .Copy_Text_to_Video_mem

	.Update_Cursor_position:
	mov ax, .Count
	add [ebx+Var.Cursor], ax
	xor eax, eax

	.Set_Flag:
	bts word [ebx+Var.Flag], Write_Flag

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

Function_Clear_Screen:
	push ebx
	push edi
	push ecx

	.Clear_Screen:
	xor eax, eax
	mov edi, $B8000
	mov ecx, 1000
	rep stosd

	.Clear_Flag:
	mov ebx, [IVideo]
	mov word [ebx+Var.Flag], 0

	pop ecx
	pop edi
	pop ebx
	ret

Function_Set_Cursor:
	.x equ byte [esp+5]
	.y equ byte [esp+4]
	mov al, .x
	mov ah, .y
	push ebx
	mov ebx, [IVideo]
	mov [ebx+Var.Cursor], ax
	btr word [ebx+Var.Flag], Write_Flag
	pop ebx
	ret 2
	restore .x
	restore .y

Function_Get_Cursor:
	.x equ dword [esp+12]
	.y equ dword [esp+8]

	push ebx

	mov ebx, [IVideo]
	mov ax, [ebx+Var.Cursor]
	mov ebx, .x
	mov byte [ebx], al
	mov ebx, .y
	mov byte [ebx], ah

	pop ebx
	leave
	ret 8
	restore .x
	restore .y

Function_New_Line:
	push ebx

	mov ebx, [IVideo]

	mov ax, [ebx+Var.Cursor]
	mov bl, 80
	div bl

	mov ebx, [IVideo]
	cmp ah, 0
	jne .j1

	bt word [ebx+Var.Flag], Write_Flag
	jc .Return

	.j1:
	neg ah
	add ah, 80
	shr ax, 8
	add ax, [ebx+Var.Cursor]

	cmp ax, 4000
	jb .j2
	xor ax, ax
	.j2: mov [ebx+Var.Cursor], ax

	.Return:
	btr word [ebx+Var.Flag], Write_Flag
	pop ebx
	ret

Function_Move_Cursor:
	.Step equ word [esp+12]  ; Step : Integer16
	push ebx
	push edi

	xor eax, eax
	mov ebx, [IVideo]
	xor edi, edi
	mov di, [ebx+Var.Cursor]

	mov ax, .Step
	test ax, ax
	jz .Return
	jns .Positive_Step

	test edi, edi
	jz .Return

	neg ax
	cmp eax, edi
	jbe .j1
	mov eax, edi
	.j1:

	shr eax, 1
	jnc .j2
	mov [$B8000+edi*2-2], word 0
	dec edi
	test eax, eax
	jz .Finish
	.j2:

	.Loop:
	mov [$B8000+edi*2-4], dword 0
	sub edi, 2
	dec eax
	test eax, eax
	jnz .Loop
	jmp .Finish

	.Positive_Step:
	add edi, eax
	cmp di, 4000
	jb .Finish
	sub di, 4000
	call Function_Clear_Screen

	.Finish:
	mov [ebx+Var.Cursor], di

	.Return:
	xor eax, eax
	pop edi
	pop ebx
	ret 2
	restore .Step

Const:
	Write_Flag = 0
Var:
	.Cursor dw 0
	.Flag dw 0