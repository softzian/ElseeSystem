; Console_p1.inc - IConsole Module part 1
; Written in 2012 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

Function_Write_console_char:		; Function 2
	.Console equ dword [gs:ebp - 8] ; Console : Handle_type
	.Char equ dword [gs:ebp - 4]	; Char : UTF32_char

	push ebp
	add ebp, 8
	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov esi, [fs:IConsole]
	mov esi, [fs:esi + Var.Console_table]
	mov eax, .Console
	call Translate_console_handle

	mov ecx, [ss:_ThreadIdx]
	cmp [ds:eax + Console_lock_owner], ecx
	jne .Error1

	mov edi, eax
	mov eax, .Char
	call Write_console_char

	xor eax, eax

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop ebp
	ret

	.Error1:
	mov eax, YOU_DO_NOT_OWN_LOCK
	jmp .Return

	restore .Console
	restore .Char

Write_console_char:
	; EDI : Console
	; EAX : Char

	.Begin:
	mov ebx, [ds:edi + Console_screen]
	call Write_console_char_p1

	cmp al, 2
	jna .Next2
	ret

	.Next2:
	call Get_lock_and_Active_console
	cmp edi, ecx
	jne .Return_release_lock

	test al, al
	jz .Next

	mov esi, eax
	
	call Scroll_video_screen
	
	mov eax, esi

	.Next:
	cmp al, 2
	je .Return_release_lock

	mov eax, [ss:_Result]
	mov esi, [ss:_Result + 4]
	call Update_char

	.Return_release_lock:
	call Release_lock
	ret

Write_console_char_p1:
	; EBX : Screen
	; EAX : Char

	mov ecx, [ds:ebx + Screen_cursor]
	mov esi, [ds:ebx + Screen_offset]

	cmp eax, 13
	je .CR
	cmp eax, 10
	je .LF
	cmp eax, 8
	je .Backspace
	cmp eax, 9
	je .Tab

	.Normal:
	mov [ds:ebx + Screen_buffer + esi * 8], eax
	mov eax, [ds:ebx + Screen_attribute]
	mov [ds:ebx + Screen_buffer + esi * 8 + 4], eax
	mov [ss:_Result], ecx
	mov [ss:_Result + 4], esi
	inc ecx
	inc esi
	mov [ds:ebx + Screen_cursor], ecx
	mov [ds:ebx + Screen_offset], esi
	call Check_cursor
	jmp .Return

	.CR:
	xor edx, edx
	mov eax, ecx
	xor ecx, ecx
	mov cx, [ds:ebx + Screen_width]
	div ecx
	sub [ds:ebx + Screen_cursor], edx
	sub [ds:ebx + Screen_offset], edx
	mov al, 13
	jmp .Return

	.LF:
	xor eax, eax
	mov ax, [ds:ebx + Screen_width]
	add ecx, eax
	add esi, eax
	mov [ds:ebx + Screen_cursor], ecx
	mov [ds:ebx + Screen_offset], esi
	call Check_cursor
	test al, al
	jnz .LF_j1
	mov al, 10
	jmp .Return
	.LF_j1:
	mov al, 2
	jmp .Return

	.Backspace:
	test ecx, ecx
	jz .Return
	dec ecx
	dec esi
	cmp esi, -1
	jne .Backspace_j1
	add esi, [ds:ebx + Screen_size]
	.Backspace_j1:
	xor eax, eax
	mov [ds:ebx + Screen_buffer + esi * 8], eax
	mov eax, [ds:ebx + Screen_attribute]
	mov [ds:ebx + Screen_buffer + esi * 8 + 4], eax
	mov [ds:ebx + Screen_offset], esi
	mov [ds:ebx + Screen_cursor], ecx
	mov [ss:_Result], ecx
	mov [ss:_Result + 4], esi
	xor al, al
	jmp .Return

	.Tab:

	.Return:
	ret

Scroll_video_screen:
	; EBX = Console_screen

	mov eax, [ds:ebx + Screen_attribute]
	mov [gs:ebp], eax

	mov eax, [ds:ebx + Screen_rect]
	mov [gs:ebp + 4], eax

	mov eax, [ds:ebx + Screen_rect + 4]
	mov [gs:ebp + 8], eax

	invoke IVideo.Scroll_rect
	ret

Check_cursor:
	mov eax, [ds:ebx + Screen_size]
	cmp esi, eax
	jb .next1

	sub esi, eax
	mov [ds:ebx + Screen_offset], esi

	.next1:
	cmp ecx, eax
	jb .no_scroll

	movzx ecx, word [ds:ebx + Screen_width]
	sub [ds:ebx + Screen_cursor], ecx
	sub [ss:_Result], ecx

	mov eax, esi
	xor edx, edx
	div ecx
	sub esi, edx

	mov eax, [ds:ebx + Screen_attribute]
	.loop:
		mov [ds:ebx + Screen_buffer + esi * 8], dword 0
		mov [ds:ebx + Screen_buffer + esi * 8 + 4], eax
		inc esi
		dec ecx
		jnz .loop
	mov al, 1
	ret

	.no_scroll:
	xor al, al
	ret
	
Update_char:	; Update one position in screen buffer to display
	; EAX = cursor
	; ESI = offset
	; EBX = Screen
	
	call Linear_cursor_to_XY_cursor

	cmp cx, [ds:ebx + Screen_X]
	jb .Return
	cmp ax, [ds:ebx + Screen_Y]
	jb .Return
	sub cx, [ds:ebx + Screen_X]
	sub ax, [ds:ebx + Screen_Y]

	add cx, [ds:ebx + Screen_rect]
	add ax, [ds:ebx + Screen_rect + 2]

	mov edx, [ds:ebx + Screen_buffer + esi * 8]
	mov esi, [ds:ebx + Screen_buffer + esi * 8 + 4]

	mov [gs:ebp], edx
	mov [gs:ebp + 4], esi
	mov [gs:ebp + 8], cx
	mov [gs:ebp + 10], ax
	invoke IVideo.Write_text_char
	
	.Return:
	ret

Linear_cursor_to_XY_cursor:
	; EBX = Screen
	; EAX = Cursor
	; Result in CX and AX
	
	push edx

	xor edx, edx
	movzx ecx, word [ds:ebx + Screen_width]
	div ecx

	mov ecx, edx
	
	pop edx
	ret