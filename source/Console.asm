; Console.asm - IConsole Module
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

IConsole = $100E00
; Function 1: Alloc_console : Handle_type
; Function 2: Write_console_char (Console : Handle_type; Char : UTF32_char)
; Function 3: Read_console_char (Console : Handle_type) : UTF32_char;
; Function 4: Switch_console (Console : Handle_type)

jmp Function_Init
Interface:
	dd Function_Alloc_console
	dd Function_Write_console_char
	dd Function_Read_console_char
	dd Function_Switch_console

Const:
	; Console record
	Console_screen = 8
	Console_input = 4
	Console_lock = 0

	; Screen Buffer record
	Screen_cursor = 0
	Screen_offset = 4
	Screen_attribute = 8
	Screen_size = 12
	Screen_width = 16
	Screen_height = 18
	Screen_X = 20
	Screen_Y = 22

Error_Code:
	CANNOT_ALLOC_MEMORY = -1
	STRING_LENGTH_IS_ZERO = -2
	CANNOT_ALLOC_QUEUE = -3

Var:
	.Active_console dd 0
	.Queue dd 0
	.Lock dd 0

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, IConsole
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, IConsole + 4 * 4
		jna .Loop

	mov [gs:ebp], dword 1024
	invoke ISystem.Create_Message_Queue

	mov eax, [ss:_Result]
	mov [fs:ebx + Var.Queue], eax

	mov [gs:ebp], eax
	invoke IKeyboard.Set_target_queue

	lea eax, [ebx + Main_thread]
	mov [gs:ebp], eax
	invoke IThread.New_Thread

	mov eax, [ss:_Result]
	mov [gs:ebp], eax
	invoke IThread.Start

	.Return:
	pop esi
	pop edi
	pop ebx
	ret

Main_thread:
	mov ebx, [fs:IConsole]
	mov esi, [fs:ebx + Var.Queue]

	.Message_loop:
	mov [gs:ebp], esi
	invoke ISystem.Get_Message

	test eax, eax
	jnz .Wait

	mov eax, [ss:_Result]
	mov ecx, [ss:_Result + 4]
	mov edx, [ss:_Result + 8]

	mov edi, [fs:ebx + Var.Active_console]
	mov edi, [ds:edi + Console_input]

	mov [gs:ebp], edi
	mov [gs:ebp + 4], eax
	mov [gs:ebp + 8], ecx
	mov [gs:ebp + 12], edx
	invoke ISystem.Send_Message

	.Wait:
	invoke IThread.Yield
	jmp .Message_loop

Function_Alloc_console: 	; Function 1
	push ebx
	push ecx

	mov [gs:ebp], dword 1024
	invoke ISystem.Create_Message_Queue

	test eax, eax
	jnz .Error2

	mov ebx, [ss:_Result]

	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], dword 16
	invoke ISystem.Allocate

	test eax, eax
	jnz .Error1

	mov ecx, [ss:_Result]

	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], dword (24 + 2000 * 8)
	invoke ISystem.Allocate

	mov eax, [ss:_Result]

	mov [ds:ecx + Console_lock], dword 0   ; Lock
	mov [ds:ecx + Console_input], ebx   ; Input queue
	mov [ds:ecx + Console_screen], eax   ; Screen buffer

	mov [ds:eax + Screen_cursor], dword 0			; Cursor
	mov [ds:eax + Screen_offset], dword 0			; Offset
	mov [ds:eax + Screen_attribute], dword $3E007FFF	; Default attribute
	mov [ds:eax + Screen_size], dword 2000			; Size in character
	mov [ds:eax + Screen_width], word 80			; Width
	mov [ds:eax + Screen_height], word 25			; Height
	mov [ds:eax + Screen_X], word 0 			; X
	mov [ds:eax + Screen_Y], word 0 			; Y

	mov [ss:_Result], ecx
	xor eax, eax

	.Return:
	pop ecx
	pop ebx
	ret

	.Error1:
	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], ebx
	invoke ISystem.Deallocate
	mov eax, CANNOT_ALLOC_MEMORY
	jmp .Return

	.Error2:
	mov eax, CANNOT_ALLOC_QUEUE
	jmp .Return

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

	mov edi, .Console
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

	restore .Console
	restore .Char

Write_console_char:
	; EDI : Console
	; EAX : Char

	.Spinlock:
		lock bts dword [ds:edi + Console_lock], 0
		jnc .Begin
		invoke IThread.Yield
		jmp .Spinlock

	.Begin:
	mov ebx, [ds:edi + Console_screen]
	call Write_console_char_p1

	cmp al, 2
	ja .Release_lock

	mov esi, [cs:IConsole]
	cmp edi, [cs:esi + Var.Active_console]
	jne .Release_lock

	test al, al
	jz .Next

	mov esi, eax
	mov eax, [ds:ebx + Screen_attribute]
	mov [gs:ebp], eax
	invoke IVideo.Scroll_screen
	mov eax, esi

	.Next:
	cmp al, 2
	je .Release_lock

	mov eax, [ss:_Result]
	xor edx, edx
	xor ecx, ecx
	mov cx, [ds:ebx + Screen_width]
	div ecx

	mov ecx, eax
	cmp dx, [ds:ebx + Screen_X]
	jb .Release_lock
	cmp cx, [ds:ebx + Screen_Y]
	jb .Release_lock
	sub dx, [ds:ebx + Screen_X]
	sub cx, [ds:ebx + Screen_Y]

	mov esi, [ss:_Result + 4]
	mov eax, [ds:ebx + 24 + esi * 8]
	mov esi, [ds:ebx + 24 + esi * 8 + 4]

	mov [gs:ebp], eax
	mov [gs:ebp + 4], esi
	mov [gs:ebp + 8], dx
	mov [gs:ebp + 10], cx
	invoke IVideo.Write_char

	.Release_lock:
	lock btr dword [ds:edi + Console_lock], 0
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
	mov [ds:ebx + 24 + esi * 8], eax
	mov eax, [ds:ebx + Screen_attribute]
	mov [ds:ebx + 24 + esi * 8 + 4], eax
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
	mov [ds:ebx + 24 + esi * 8], eax
	mov eax, [ds:ebx + Screen_attribute]
	mov [ds:ebx + 24 + esi * 8 + 4], eax
	mov [ds:ebx + Screen_offset], esi
	mov [ds:ebx + Screen_cursor], ecx
	mov [ss:_Result], ecx
	mov [ss:_Result + 4], esi
	xor al, al
	jmp .Return

	.Tab:

	.Return:
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

	xor ecx, ecx
	mov cx, [ds:ebx + Screen_width]
	sub [ds:ebx + Screen_cursor], ecx

	mov eax, esi
	xor edx, edx
	div ecx
	sub esi, edx

	mov eax, [ds:ebx + Screen_attribute]
	.loop:
		mov [ds:ebx + 24 + esi * 8], dword 0
		mov [ds:ebx + 24 + esi * 8 + 4], eax
		inc esi
		dec ecx
		jnz .loop
	mov al, 1
	ret

	.no_scroll:
	xor al, al
	ret

Function_Read_console_char:
	.Console equ dword [gs:ebp - 4]
	.t equ dword [gs:ebp - 4]

	push ebp
	add ebp, 4
	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, .Console
	call Read_console_char

	mov edi, ebx
	mov eax, [ss:_Result]
	mov .t, eax

	call Write_console_char

	cmp .t, 13
	jne .Finish

	mov eax, 10
	call Write_console_char

	.Finish:
	mov eax, .t
	mov [ss:_Result], eax

	xor eax, eax

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop ebp
	ret

	restore .Console
	restore .t

Read_console_char:
	; EBX : Console

	.Spinlock:
		lock bts dword [ds:ebx + Console_lock], 1
		jnc .Begin
		invoke IThread.Yield
		jmp .Spinlock

	.Begin:
	mov eax, [ds:ebx + Console_input]
	mov [gs:ebp], eax
	invoke ISystem.Get_Message

	test eax, eax
	jnz .Wait

	bt dword [ss:_Result + 8], 0
	jc .Wait

	mov eax, [ss:_Result]
	cmp eax, $E000
	jb .Release_lock
	cmp eax, $F8FF
	ja .Release_lock

	.Wait:
	invoke IThread.Yield
	jmp .Begin

	.Release_lock:
	lock btr dword [ds:ebx + Console_lock], 1

	ret

Function_Switch_console:	; Function 4
	.Console equ dword [gs:ebp - 4]    ; Console : Handle_type

	push ebp
	add ebp, 4
	push ebx
	push ecx

	mov ebx, [fs:IConsole]
	mov ecx, [fs:ebx + Var.Active_console]
	mov eax, .Console
	cmp ecx, eax
	je .Return

	.Spinlock1:
		lock bts dword [fs:ebx + Var.Lock], 0
		jnc .Spinlock2
		invoke IThread.Yield
		jmp .Spinlock1

	.Spinlock2:
		lock bts dword [ds:ecx + Console_lock], 0
		jnc .Spinlock3
		invoke IThread.Yield
		jmp .Spinlock2

	.Spinlock3:
		lock bts dword [ds:ecx + Console_lock], 1
		jnc .Begin
		invoke IThread.Yield
		jmp .Spinlock3

	.Begin:
	mov [fs:ebx + Var.Active_console], eax

	mov ebx, [ds:eax + Console_screen]
	mov eax, [ds:eax + Screen_size]




	.Done:
	mov ebx, [fs:IConsole]
	lock btr dword [fs:ebx + Var.Lock], 0
	xor eax, eax

	.Return:
	pop ebx
	pop ebp
	ret

	restore .Console

Update_screen:
	; EBX : Screen

	mov esi, [ds:ebx + Screen_offset]
	mov edi, [ds:ebx + Screen_cursor]
	xor ecx, ecx
	mov cx, [ds:ebx + Screen_width]
	xor edx, edx

	.Loop:
		mov eax, edi
		div ecx

		cmp dx, [ds:ebx + Screen_X]
		jb .Next1
		sub dx, [ds:ebx + Screen_X]
		cmp ax, [ds:ebx + Screen_Y]
		jb .Next1
		sub ax, [ds:ebx + Screen_Y]

		mov [gs:ebp], dword 0
		push dword [ds:ebx + Screen_attribute]
		pop dword [gs:ebp + 4]
		mov [gs:ebp + 8], dx
		mov [gs:ebp + 10], ax
		invoke IVideo.Write_char