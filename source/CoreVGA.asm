; CoreVGA.asm - Core VGA driver module
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

ICoreVideo = $100020
; Function 1: Write_Telex (var Text : Array of Char; Count : Word)
; Function 2: Clear_Screen
; Function 3: New_Line

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Write_Telex
	dd Function_Clear_Screen
	dd Function_New_Line
Header:

ICoreVideo_Error_code:
	INVALID_COUNT = -1
	INVALID_CURSOR = -2
	VIDEO_BUFFER_OVERFLOW = -3
	CANNOT_CREATE_HANDLE = -4
	ALLOCATE_MEMORY_PROBLEM = -6
	CANNOT_SWITCH_MEMORY_CONTEXT = -7
	INVALID_CONTEXT_HANDLE = -8
	NOT_TEXT_MODE_CONTEXT = -9
	UNSUPPORT_CONTEXT_TYPE = -10

Const:
	Write_Flag = 0

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:ICoreVideo], eax
	mov [fs:ICoreVideo + 4], esi

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 3
		jb .Loop

	; Register module
	mov [gs:ebp], dword 4
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], ebx
	invoke IModule, IModule.Register_Module

	test eax, eax
	jnz .Error1

	xor eax, eax

	.Return:
	pop esi
	pop edi
	pop ebx
	ret

	.Error1:
	mov eax, 1
	jmp .Return


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

	mov ebx, [fs:ICoreVideo]

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
	mov ebx, [fs:ICoreVideo]
	mov word [fs:ebx + Var.Flag], 0

	pop ecx
	pop edi
	pop ebx
	ret

Function_New_Line:	; Function 3
	push ebx

	mov ebx, [fs:ICoreVideo]

	mov ax, [fs:ebx + Var.Cursor]
	mov bl, 80
	div bl

	mov ebx, [fs:ICoreVideo]
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

Function_Write_Telex2:	 ; Function 4
	.Text equ dword [gs:ebp - 6]
	.Count equ word [gs:ebp - 2]

	push ebp
	add ebp, 6

	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, [fs:ICoreVideo]

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
	mov al, [fs:esi]
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

Var:
	.Cursor dw 0
	.Flag dw 0
	.Active_context dd 0
	.Allocator1 dd 0