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
; Function 3: New_Line

; Function 4: Blit_text (in Src : Buffer; Cursor, Count : Cardinal)
; Function 5: Blit_char (Char : UTF32_char; Attribute, Cursor : Cardinal)

jmp Function_Init
Interface:
	dd Function_Write_Telex
	dd Function_Clear_Screen
	dd Function_New_Line

	dd Function_Blit_text
	dd Function_Write_char
	dd Function_Scroll_screen

Const:
	Write_Flag = 0
Error_Code:
	INVALID_COUNT = -1

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

		cmp edi, IVideo + 4 * 6
		jna .Loop

	pop esi
	pop edi
	pop ebx
	ret

Function_Write_Telex:	; Function 1
	.Text equ dword [gs:ebp - 6]
	.Count equ word [gs:ebp - 2]

	push ebp
	add ebp, 6

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

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_COUNT
	jmp .Return

	restore .Count
	restore .Text

Function_Clear_Screen:	; Function 2
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

Function_New_Line:	; Function 3
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

Function_Blit_text:	; Function 4
	.Src equ dword [gs:ebp - 12]	   ; in Src : Buffer
	.Cursor equ dword [gs:ebp - 8]	  ; Cursor : Cardinal
	.Count equ dword [gs:ebp - 4]	   ; Count : Cardinal

	push ebp
	add ebp, 12

	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, .Src
	mov edx, .Cursor
	mov ecx, .Count

	.Loop:
		mov eax, [ds:ebx]
		mov [fs:$B8000 + edx * 2], al

		mov esi, [ds:ebx + 4]
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

	pop ebp
	ret

	restore .Src
	restore .Cursor
	restore .Count

Function_Write_char:	 ; Function 5
	.Char equ dword [gs:ebp - 12]		; Char : UTF32_char
	.Attribute equ dword [gs:ebp - 8]	; Attribute : Cardinal
	.X equ word [gs:ebp - 4]		; X : Card16
	.Y equ word [gs:ebp - 2]		; Y : Card16

	push ebp
	add ebp, 12

	push ebx
	push ecx
	push edx

	mov ebx, [cs:IVideo]
	xor edx, edx

	mov dx, .X
	mov ax, .Y

	cmp dx, $100
	jae .Return
	cmp ax, $100
	jae .Return

	mov cl, [cs:ebx + Var.Width]
	cmp dl, cl
	jae .Return
	cmp al, [cs:ebx + Var.Height]
	jae .Return

	mul cl
	add dx, ax

	mov eax, .Char
	mov [fs:$80000 + edx * 2], al
	mov [fs:$B8000 + edx * 2], al

	mov eax, .Attribute
	call Function_32bit_attr_to_VGA_attr

	mov [fs:$80000 + edx * 2 + 1], al
	mov [fs:$B8000 + edx * 2 + 1], al

	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	restore .Char
	restore .Attribute
	restore .X
	restore .Y

Function_32bit_attr_to_VGA_attr:
	; EAX: Input
	; AL : Output

	push edi

	push eax
	call Function_15bit_RGB_to_4bit_RGBI
	mov edi, eax
	pop eax

	shr eax, 15
	call Function_15bit_RGB_to_4bit_RGBI
	shl al, 4
	or eax, edi

	pop edi
	ret

Function_15bit_RGB_to_4bit_RGBI:
	; ax - input
	; bl - rgb; bh - intensify; 16 bit high of ebx - result
	; al - output

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

Function_Scroll_screen: 	; Function 6
	.Attribute equ dword [gs:ebp - 4] ; Attribute : Cardinal

	push ebp
	add ebp, 4

	push ebx
	push ecx
	push edx

	mov ebx, [cs:IVideo]
	xor edx, edx
	mov dl, [cs:ebx + Var.Width]
	mov bx, [cs:ebx + Var.Screen_size]
	and ebx, $FFFF
	xor ecx, ecx

	.Loop1:
		mov ax, [fs:$80000 + edx * 2]
		mov [fs:$80000 + ecx * 2], ax
		inc ecx
		inc edx
		cmp edx, ebx
		jb .Loop1

	mov eax, .Attribute
	call Function_32bit_attr_to_VGA_attr
	mov ah, al
	xor al, al

	.Loop2:
		mov [fs:$80000 + ecx * 2], ax
		inc ecx
		cmp ecx, ebx
		jb .Loop2

	shl ebx, 1
	xor ecx, ecx

	.Loop3:
		mov al, [fs:$80000 + ecx]
		mov [fs:$B8000 + ecx], al
		inc ecx
		cmp ecx, ebx
		jb .Loop3

	.Return:
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	restore .Attribute

Var:
	.Width db 80
	.Height db 25
	.Screen_size dw 2000
	.Cursor dw 0
	.Flag dw 0