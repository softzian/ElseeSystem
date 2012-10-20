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

include 'include\Header.inc'
include 'include\errcode.inc'
use32

IVideo = $100800
; Function 1: Write_Telex (var Text : Array of Char; Count : Word)
; Function 2: Clear_Screen
; Function 3: Set_Cursor (x, y : Byte)
; Function 4: Get_Cursor (var x, var y : Byte)
; Function 5: New_Line
; Function 6: Move_Cursor (Step : Integer16)

; Function 7: Write (WindowIdx : Byte; in Text : Array of Char; Count : Word)
; Function 8: Switch_Window (WindowIdx : Byte)

; Function 9: Blit_text (in Src : Buffer; Cursor, Count : Cardinal)

jmp Function_Init
Interface:
	dd Function_Write_Telex
	dd Function_Clear_Screen
	dd Function_Set_Cursor
	dd Function_Get_Cursor
	dd Function_New_Line
	dd Function_Move_Cursor

	dd Function_Write
	dd Function_Switch_Window

	dd Function_Blit_text

Const:
	NumOf_Windows = 2
	Write_Flag = 0
Error_Code:
	INVALID_COUNT = -1
	INVALID_WINDOW_IDX = -2

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, IVideo
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, IVideo + 4 * 9
		jna .Loop

	pop esi
	pop edi
	pop ebx
	ret

Function_Write_Telex:
	.Text equ dword [ebp + 10]
	.Count equ word [ebp + 8]

	enter 0, 0
	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, [fs:IVideo]

	mov ax, .Count
	cmp ax, 0
	je .Error1
	cmp ax, 2000
	ja .Error1
	add ax, [fs:ebx + Var.Cursor]
	cmp ax, 2000
	jb .Write

	mov word [fs:ebx + Var.Cursor], 0    ; Reset cursor position if the remaining display space is not enough

	.Write:
	mov esi, .Text

	mov edi, $B8000
	xor eax, eax
	mov ax, [fs:ebx + Var.Cursor]
	shl eax, 1
	add edi, eax	; EDI points to current Cursor position in Video memory

	xor ecx, ecx
	mov cx, .Count
	.Copy_Text_to_Video_mem:
	mov al, [esi]
	mov byte [fs:edi + 1], 00001111b
	mov [fs:edi], al
	inc esi
	add edi, 2
	loop .Copy_Text_to_Video_mem

	.Update_Cursor_position:
	mov ax, .Count
	add [fs:ebx + Var.Cursor], ax
	xor eax, eax

	.Set_Flag:
	bts word [fs:ebx + Var.Flag], Write_Flag

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

	.Loop:
		mov [fs:edi], eax
		add edi, 4
		loop .Loop

	.Clear_Flag:
	mov ebx, [fs:IVideo]
	mov word [fs:ebx+Var.Flag], 0

	pop ecx
	pop edi
	pop ebx
	ret

Function_Set_Cursor:	; Isn't working yet
	.x equ byte [esp + 5]
	.y equ byte [esp + 4]
	mov al, .x
	mov ah, .y
	push ebx
	mov ebx, [fs:IVideo]
	mov [fs:ebx + Var.Cursor], ax
	btr word [fs:ebx + Var.Flag], Write_Flag
	pop ebx
	ret 2
	restore .x
	restore .y

Function_Get_Cursor:	; Isn't working yet
	.x equ dword [esp + 12]
	.y equ dword [esp + 8]

	push ebx

	mov ebx, [fs:IVideo]
	mov ax, [fs:ebx + Var.Cursor]
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

	mov ebx, [fs:IVideo]

	mov ax, [fs:ebx + Var.Cursor]
	mov bl, 80
	div bl

	mov ebx, [fs:IVideo]
	cmp ah, 0
	jne .j1

	bt word [fs:ebx + Var.Flag], Write_Flag
	jc .Return

	.j1:
	neg ah
	add ah, 80
	shr ax, 8
	add ax, [fs:ebx + Var.Cursor]

	cmp ax, 2000
	jb .j2
	xor ax, ax
	.j2: mov [fs:ebx + Var.Cursor], ax

	.Return:
	btr word [fs:ebx + Var.Flag], Write_Flag
	pop ebx
	ret

Function_Move_Cursor:
	.Step equ word [esp + 12]  ; Step : Integer16
	push ebx
	push edi

	xor eax, eax
	mov ebx, [fs:IVideo]
	xor edi, edi
	mov di, [fs:ebx + Var.Cursor]

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
	mov [fs:$B8000 + edi * 2 - 2], word 0
	dec edi
	test eax, eax
	jz .Finish
	.j2:

	.Loop:
	mov [fs:$B8000 + edi * 2 - 4], dword 0
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
	mov [fs:ebx + Var.Cursor], di

	.Return:
	xor eax, eax
	pop edi
	pop ebx
	ret 2
	restore .Step

Function_Write: 	; Function 7
	.WindowIdx equ byte [ebp + 14]	; WindowIdx : Byte
	.Text equ dword [ebp + 10]	; in Text : Array of Char
	.Count equ word [ebp + 8]	; Count : Word

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push esi
	push edi

	xor ecx, ecx
	mov cx, .Count
	test ecx, ecx
	je .Error1	; Invalid count
	cmp ecx, 2000
	ja .Error1

	mov ebx, [fs:IVideo]

	xor eax, eax
	mov al, .WindowIdx

	test al, al
	jz .Error2	; Invalid window idx
	cmp al, NumOf_Windows
	ja .Error2

	dec al
	mov ebx, [fs:ebx + Windows_table + eax * 4]

	xor edi, edi
	mov di, [ebx]

	lea eax, [ecx + edi]
	cmp eax, 2000
	jb .Next
	xor edi, edi

	.Next:
	mov esi, .Text
	shl edi, 1
	add edi, 4

	.Loop:
		mov al, [esi]
		mov ah, 00001111b
		mov [ebx + edi], ax
		inc esi
		add edi, 2
		loop .Loop

	sub edi, 4
	shr edi, 1
	mov [ebx], di
	bts word [ebx + 2], Write_Flag

	mov ebx, [fs:IVideo]
	mov al, .WindowIdx
	cmp al, [fs:ebx + Var.Active_window]
	jne .Finish

	push .Text
	push .Count
	call Function_Write_Telex

	.Finish:
	xor eax, eax
	.Return:
	pop edi
	pop esi
	pop ecx
	pop ebx
	leave
	ret 7
	.Error1:
	mov eax, INVALID_COUNT
	jmp .Return
	.Error2:
	mov eax, INVALID_WINDOW_IDX
	jmp .Return

	restore .WindowIdx
	restore .Text
	restore .Count

Function_Switch_Window:
	.WindowIdx equ byte [ebp + 8]

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov al, .WindowIdx
	test al, al
	jz .Error1	; Invalid window idx
	cmp al, NumOf_Windows
	ja .Error1

	mov ebx, [fs:IVideo]
	cmp al, [fs:ebx + Var.Active_window]
	je .Return

	dec al
	mov esi, [fs:ebx + Windows_table + eax * 4]
	mov eax, [esi]
	mov dword [fs:ebx + Var.Cursor], eax

	xor ecx, ecx
	add esi, 4
	.Loop:
		mov eax, [esi + ecx]
		mov [fs:$B8000 + ecx], eax
		add ecx, 4
		cmp ecx, 4000
		jb .Loop

	mov al, .WindowIdx
	mov [fs:ebx + Var.Active_window], al
	xor eax, eax

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 1
	.Error1:
	mov eax, INVALID_WINDOW_IDX
	jmp .Return

	restore .WindowIdx

Function_Blit_text:
	.Src equ dword [ebp + 16]	; in Src : Buffer
	.Cursor equ dword [ebp + 12]	; Cursor : Cardinal
	.Count equ dword [ebp + 8]	; Count : Cardinal

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, .Src
	mov edx, .Cursor
	mov ecx, .Count
	xor esi, esi
	.Loop:
		mov eax, [ebx]
		mov [fs:$B8000 + edx * 2], al

		mov esi, [ebx + 4]
		mov eax, esi
		call Function_15bit_RGB_to_4bit_RGBI
		mov edi, eax
		mov eax, esi
		shr eax, 15
		call Function_15bit_RGB_to_4bit_RGBI
		shl al, 4
		or eax, edi

		mov [fs:$B8000 + edx * 2 + 1], al

		inc edx
		add ebx, 8
		dec ecx
		jnz .Loop

	xor eax, eax
	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 12

	restore .Src
	restore .Cursor
	restore .Count

Function_15bit_RGB_to_4bit_RGBI:
	; bl - rgb; bh - intensify; 16 bit high of ebx - result
	push ebx
	xor ebx, ebx

	.Blue:
	mov bl, al
	and bl, 11111b
	cmp bl, 8
	jb .Green
	inc bh
	bts ebx, 16
	cmp bl, 24
	jb .Green
	inc bh

	.Green:
	shr eax, 5
	mov bl, al
	and bl, 11111b
	cmp bl, 8
	jb .Red
	inc bh
	bts ebx, 17
	cmp bl, 24
	jb .Red
	inc bh

	.Red:
	shr eax, 5
	mov bl, al
	and bl, 11111b
	cmp bl, 8
	jb .Intensify
	inc bh
	bts ebx, 18
	cmp bl, 24
	jb .Intensify
	inc bh

	.Intensify:
	cmp bh, 3
	jbe .Return
	bts ebx, 19

	.Return:
	shr ebx, 16
	mov eax, ebx
	pop ebx
	ret

Var:
	.Cursor dw 0
	.Flag dw 0
	.Active_window db 1
Windows_table:
	dd 0 dup 2