; Editor.asm - Simple editor
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

jmp Function_Init

Const:
	; Global variables:
	File_ = 0
	Context = 4
	Input = 8
	File_pointer = 12
	File_lin = 16
	File_col = 18
	Screen_lin = 20
	Screen_col = 22
	Line_array = 24

	; Char code
	CR = 13
	LF = 10
	TAB = 9
	BACKSPACE = 8

Function_Init:
	; Allocate program memory set
	mov [gs:ebp], dword $1000
	invoke ISystem.Allocate
	mov ebx, [ss:_Result]

	; Allocate a "file"
	mov [gs:ebp], dword $1000
	invoke ISystem.Allocate
	mov eax, [ss:_Result]
	mov [ds:ebx + File_], eax

	; Create video context
	mov [gs:ebp], dword 1
	invoke IVideo.Alloc_context
	mov eax, [ss:_Result]
	mov [ds:ebx + Context], eax
	mov [gs:ebp], eax
	invoke IVideo.Switch_context

	; Create input queue
	mov [gs:ebp], dword 128
	invoke ISystem.Create_Message_Queue
	mov eax, [ss:_Result]
	mov [ds:ebx + Input], eax
	mov [gs:ebp], dword eax
	invoke IKeyboard.Set_target_queue

	; Initialize values
	mov ax, 1
	xor ecx, ecx
	mov [ds:ebx + File_pointer], ecx
	mov [ds:ebx + File_lin], ax
	mov [ds:ebx + File_col], ax
	mov [ds:ebx + Screen_lin], ax
	mov [ds:ebx + Screen_col], ax
	mov [ds:ebx + Line_array], ecx
	mov [ds:ebx + Line_array + 4], ax

Function_Main:
	mov eax, [ds:ebx + Input]
	mov [gs:ebp], eax
	invoke ISystem.Get_Message

	test eax, eax
	jnz .Wait

	mov eax, [ss:_Result] ; Virtual code
	mov edx, [ss:_Result + 4] ; Scancode
	mov ecx, [ss:_Result + 8] ; Keyboard status
	bt ecx, 0
	jc .Wait
	cmp eax, $FF
	ja .Wait

	bt ecx, 1
	jnc .Write
	mov [gs:ebp], edx
	invoke IKeyboard.Shift_character
	mov eax, [ss:_Result]

	.Write:
	call Write_to_file
	call Update_screen

	.Wait:
	invoke IThread.Yield
	jmp Function_Main

Write_to_file:
	push esi
	push ecx
	push edx

	mov esi, [ds:ebx + File_]
	mov ecx, [ds:ebx + File_pointer]
	cmp eax, CR
	je .New_line
	cmp eax, BACKSPACE
	je .Delete

	.Normal:
	mov [ds:esi + ecx], al
	inc dword [ds:ebx + File_pointer]
	inc word [ds:ebx + File_col]
	movzx edx, word [ds:ebx + File_lin]
	inc word [ds:ebx + Line_array + edx * 8 - 8 + 4]
	jmp .Return

	.New_line:
	mov [ds:esi + ecx], byte CR
	mov [ds:esi + ecx + 1], byte LF
	add ecx, 2
	mov [ds:ebx + File_pointer], ecx
	inc word [ds:ebx + File_lin]
	mov [ds:ebx + File_col], word 1
	movzx edx, word [ds:ebx + File_lin]
	mov [ds:ebx + Line_array + edx * 8 - 8], ecx
	mov [ds:ebx + Line_array + edx * 8 - 8 + 4], word 1
	jmp .Return

	.Delete:
	test ecx, ecx
	jz .Return
	cmp [ds:esi + ecx - 1], byte LF
	jne .Del_char
	cmp [ds:esi + ecx - 2], byte CR
	je .Del_line
		.Del_char:
		dec dword [ds:ebx + File_pointer]
		dec word [ds:ebx + File_col]
		movzx edx, word [ds:ebx + File_lin]
		dec word [ds:ebx + Line_array + edx * 8 - 8 + 4]
		jmp .Return
		.Del_line:
		sub ecx, 2
		mov [ds:ebx + File_pointer], ecx
		dec word [ds:ebx + File_lin]
		movzx edx, word [ds:ebx + File_lin]
		push word [ds:ebx + Line_array + edx * 8 - 8 + 4]
		pop word [ds:ebx + File_col]

	.Return:
	pop edx
	pop ecx
	pop esi
	ret

Update_screen:
	push eax
	push ecx
	push edx
	push esi

	mov eax, [ds:ebx + Context]
	mov [gs:ebp], eax
	invoke IVideo.Clear_text_screen

	mov ax, [ds:ebx + File_col]
	cmp ax, [ds:ebx + Screen_col]
	jb .Adjust_screen1
	sub ax, [ds:ebx + Screen_col]
	cmp ax, 80
	jb .Verify_line
	jmp .Adjust_screen2
		.Adjust_screen1:
		mov [ds:ebx + Screen_col], ax
		jmp .Verify_line
		.Adjust_screen2:
		sub ax, 79
		add [ds:ebx + Screen_col], ax

	.Verify_line:
	mov ax, [ds:ebx + File_lin]
	cmp ax, [ds:ebx + Screen_lin]
	jb .Adjust_screen3
	sub ax, [ds:ebx + Screen_lin]
	cmp ax, 25
	jb .Update
	jmp .Adjust_screen4
		.Adjust_screen3:
		mov [ds:ebx + Screen_lin], ax
		jmp .Update
		.Adjust_screen4:
		sub ax, 24
		add [ds:ebx + Screen_lin], ax

	.Update:
	movzx edx, word [ds:ebx + File_lin]
	movzx ecx, word [ds:ebx + Screen_lin]
	xor esi, esi
	.Loop:
	call Update_line
	inc ecx
	inc esi
	cmp ecx, edx
	jbe .Loop

	mov cx, [ds:ebx + File_lin]
	mov dx, [ds:ebx + File_col]
	sub cx, [ds:ebx + Screen_lin]
	sub dx, [ds:ebx + Screen_col]
	mov eax, [ds:ebx + Context]
	mov [gs:ebp], eax
	mov [gs:ebp + 4], dl
	mov [gs:ebp + 5], cl
	invoke IVideo.Set_text_cursor

	pop esi
	pop edx
	pop ecx
	pop eax
	ret

Update_line:
	push eax
	push edx

	mov eax, [ds:ebx + Line_array + ecx * 8 - 8]
	movzx edx, word [ds:ebx + Screen_col]
	add eax, edx
	dec eax
	mov edx, [ds:ebx + File_]
	add eax, edx
	mov [gs:ebp + 4], eax

	movzx eax, word [ds:ebx + Line_array + ecx * 8 - 8 + 4]
	movzx edx, word [ds:ebx + Screen_col]
	sub eax, edx
	cmp al, 80
	jbe .j1
	mov al, 80
	.j1: mov [gs:ebp + 8], ax

	mov [gs:ebp + 10], byte 1111b
	mov [gs:ebp + 11], byte 0
	mov eax, esi
	mov [gs:ebp + 12], al

	mov eax, [ds:ebx + Context]
	mov [gs:ebp], eax
	invoke IVideo.Write_text_line

	pop edx
	pop eax
	ret