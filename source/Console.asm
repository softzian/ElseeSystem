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
; Function 2: Write_console_char (Console : Address; Char : UTF32_char)
; Function 3: Read_console_char (Console : Address) : UTF32_char;
; Function 4: Switch_console (Console : Handle_type)

; Function 5: Lock_console (Console : Handle_type) : Address
; Function 6: Release_console (Console : Handle_type)

jmp Function_Init
Interface:
	dd Function_Alloc_console
	dd Function_Write_console_char
	dd Function_Read_console_char
	dd Function_Switch_console

	dd Function_Lock_console
	dd Function_Release_console

Const:
	; Console record
	Console_screen = 8
	Console_input = 4
	Console_lock = 0
	Console_lock_owner = 12

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
	YOU_DO_NOT_OWN_LOCK = -4
	CONSOLE_TABLE_FULL = -5

Var:
	.Console_table dd 0
	.Active_console dd 0
	.Queue dd 0

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

		cmp edi, IConsole + 4 * 6
		jna .Loop

	; Create console table
	mov [gs:ebp], dword 16
	mov [gs:ebp + 4], dword 16
	invoke IData.Create_table

	mov eax, [ss:_Result]
	mov [fs:ebx + Var.Console_table], eax

	; Create input queue
	mov [gs:ebp], dword 1024
	invoke ISystem.Create_Message_Queue

	mov eax, [ss:_Result]
	mov [fs:ebx + Var.Queue], eax

	mov [gs:ebp], eax
	invoke IKeyboard.Set_target_queue

	; Create processing thread
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
	mov edi, [fs:ebx + Var.Queue]
	mov esi, [fs:ebx + Var.Console_table]

	.Message_loop:
	mov [gs:ebp], edi
	invoke ISystem.Get_Message

	test eax, eax
	jnz .Wait

	mov eax, [ss:_Result]
	mov ecx, [ss:_Result + 4]
	mov edx, [ss:_Result + 8]

	mov [gs:ebp + 4], eax
	mov [gs:ebp + 8], ecx
	mov [gs:ebp + 12], edx

	mov eax, [fs:ebx + Var.Active_console]
	mov eax, [ds:eax + Console_input]

	mov [gs:ebp], eax
	invoke ISystem.Send_Message

	.Wait:
	invoke IThread.Yield
	jmp .Message_loop


Function_Alloc_console: 	; Function 1
	push ebx
	push ecx
	push edx
	push esi

	; Step 1 - Add an entry in console table
	mov ebx, [fs:IConsole]
	mov esi, [fs:ebx + Var.Console_table]

	mov [gs:ebp], esi
	invoke IData.Access_table

	mov [gs:ebp], esi
	invoke IData.Add_table_entry

	test eax, eax
	jnz .Error3

	mov edx, [ss:_Result]
	mov ecx, [ss:_Result + 4]

	; Step 2 - Create input message queue
	mov [gs:ebp], dword 1024
	invoke ISystem.Create_Message_Queue

	test eax, eax
	jnz .Error2

	mov ebx, [ss:_Result]

	; Step 3 - Create screen buffer
	mov [gs:ebp], dword (24 + 2000 * 8)
	invoke ISystem.Allocate

	test eax, eax
	jnz .Error1

	mov eax, [ss:_Result]

	; Step 4 - Finish
	mov [ds:esi + ecx + Console_lock], dword 0		; Lock
	mov [ds:esi + ecx + Console_lock_owner], dword 0	; The thread that lock this console
	mov [ds:esi + ecx + Console_input], ebx 		; Input queue
	mov [ds:esi + ecx + Console_screen], eax		; Screen buffer

	mov [ds:eax + Screen_cursor], dword 0			; Cursor
	mov [ds:eax + Screen_offset], dword 0			; Offset
	mov [ds:eax + Screen_attribute], dword $3E007FFF	; Default attribute
	mov [ds:eax + Screen_size], dword 2000			; Size in character
	mov [ds:eax + Screen_width], word 80			; Width
	mov [ds:eax + Screen_height], word 25			; Height
	mov [ds:eax + Screen_X], word 0 			; X
	mov [ds:eax + Screen_Y], word 0 			; Y

	mov [gs:ebp], esi
	invoke IData.Finish_access_table

	mov [ss:_Result], edx
	xor eax, eax

	.Return:
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

	.Error1:
	mov eax, CANNOT_ALLOC_MEMORY
	jmp .Return

	.Error2:
	mov eax, CANNOT_ALLOC_QUEUE
	jmp .Return

	.Error3:
	mov [gs:ebp], esi
	invoke IData.Finish_modify_table

	mov eax, CONSOLE_TABLE_FULL
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
	ja .Return

	mov esi, [cs:IConsole]
	cmp edi, [cs:esi + Var.Active_console]
	jne .Return

	test al, al
	jz .Next

	mov esi, eax
	
	mov eax, [ds:ebx + Screen_attribute]
	mov [gs:ebp], eax
	invoke IVideo.Scroll_screen
	
	mov eax, esi

	.Next:
	cmp al, 2
	je .Return

	mov eax, [ss:_Result]
	mov esi, [ss:_Result + 4]
	
	call Update_char

	.Return: ret

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
	
Update_char:	; Update one position in screen buffer to display
	; EAX = cursor
	; ESI = offset
	; EBX = Screen
	
	xor edx, edx
	xor ecx, ecx
	mov cx, [ds:ebx + Screen_width]
	div ecx

	mov ecx, eax
	cmp dx, [ds:ebx + Screen_X]
	jb .Return
	cmp cx, [ds:ebx + Screen_Y]
	jb .Return
	sub dx, [ds:ebx + Screen_X]
	sub cx, [ds:ebx + Screen_Y]

	mov eax, [ds:ebx + 24 + esi * 8]
	mov esi, [ds:ebx + 24 + esi * 8 + 4]

	mov [gs:ebp], eax
	mov [gs:ebp + 4], esi
	mov [gs:ebp + 8], dx
	mov [gs:ebp + 10], cx
	invoke IVideo.Write_text_char
	
	.Return:
	ret

Function_Read_console_char:	; Function 3
	.Console equ dword [gs:ebp - 4]
	.t equ dword [gs:ebp - 4]

	push ebp
	add ebp, 4
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

	mov ebx, eax
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

	.Error1:
	mov eax, YOU_DO_NOT_OWN_LOCK
	jmp .Return

	restore .Console
	restore .t

Read_console_char:
	; EBX : Console

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
	jb .Return
	cmp eax, $F8FF
	ja .Return

	.Wait:
	invoke IThread.Yield
	jmp .Begin

	.Return: ret

Function_Switch_console:	; Function 4
	.Console equ dword [gs:ebp - 4]    ; Console : Handle_type

	push ebp
	add ebp, 4
	push ebx
	push ecx
	push esi
	
	mov ebx, [fs:IConsole]

	mov esi, [fs:ebx + Var.Console_table]
	mov [gs:ebp], esi
	invoke IData.Modify_table

	mov eax, .Console
	call Translate_console_handle

	mov ecx, [fs:ebx + Var.Active_console]

	cmp ecx, eax
	je .Return

	.Begin:
	mov [fs:ebx + Var.Active_console], eax
	
	mov ebx, [ds:eax + Console_screen]
	call Update_screen

	.Done:
	mov [gs:ebp], esi
	invoke IData.Finish_modify_table

	xor eax, eax

	.Return:
	pop esi
	pop ecx
	pop ebx
	pop ebp
	ret

	restore .Console

Function_Cardinal_to_HexStr_32:
	.Num equ dword [gs:ebp - 8]
	.HexStr equ dword [gs:ebp - 4]

	push ebp
	add ebp, 8
	push ebx
	push ecx
	push edx
	push edi

	mov edx, .Num
	xor ebx, ebx
	mov edi, .HexStr

	mov cl, 7
	.Loop:
	mov eax, edx
	shl cl, 2
	shr eax, cl
	shr cl, 2
	and al, $F

	cmp al, $A
	jae .j1
	add al, '0' - 0
	jmp .j2
	.j1: add al, 'A' - $A
	.j2: inc ebx

	mov [ds:edi + ebx - 1], al

	.Continue_loop:
	dec cl
	jns .Loop

	.Return:
	xor eax, eax
	pop edi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	restore .Num
	restore .HexStr
	
Calculate_offset:
	; EBX = Screen
	; EAX = Cursor value
	; ESI = return value

	mov esi, [ds:ebx + Screen_offset]
	add esi, eax
	sub esi, [ds:ebx + Screen_cursor]
	
	cmp esi, [ds:ebx + Screen_size]
	jb  .Return
	
	cmp eax, [ds:ebx + Screen_cursor]
	jna .jp1
		sub esi, [ds:ebx + Screen_size]
		ret
		.jp1:
		add esi, [ds:ebx + Screen_size]
		
	.Return: ret
	
XY_cursor_to_linear_cursor:
	; EBX = Screen
	; AX = Y
	; CX = X
	; EAX <- return value
	
	push edx

	and eax, $FFFF
	xor edx, edx
	mov dx, [ds:ebx + Screen_width]
	mul edx
	
	and ecx, $FFFF
	add eax, ecx
	
	pop edx
	ret
		
Update_row:
	; EBX = Screen
	; EAX = row number (less than $10000)
	
	push esi
	push eax	; Save row number
	
	mov cx, [ds:ebx + Screen_X]
	call XY_cursor_to_linear_cursor
	
	call Calculate_offset
		
	.Write_to_display:
	pop eax ; Return row number from stack
	
	lea esi, [ebx + 24 + esi * 8]
	mov [gs:ebp], esi
	mov [gs:ebp + 4], word 0
	
	sub ax, [ds:ebx + Screen_Y]
	mov [gs:ebp + 6], ax
	
	invoke IVideo.Get_display_size
	mov ax, [ss:_Result]	; display width
	mov [gs:ebp + 8], eax
	
	invoke IVideo.Write_text_line
	
	.Return:
	pop esi
	ret

Update_screen:
	; EBX : Screen
	
	push ecx
	push edx
	push esi
	push edi

	mov ax, [ds:ebx + Screen_Y]
	mov cx, [ds:ebx + Screen_X]
	call XY_cursor_to_linear_cursor

	call Calculate_offset

	invoke IVideo.Get_display_size
	movzx edx, word [ss:_Result + 2]	; height
	xor ecx, ecx

	mov edi, [ds:ebx + Screen_size]

	.Loop:
		lea eax, [ebx + 24 + esi * 8]
		mov [gs:ebp], eax
		mov [gs:ebp + 4], word 0
		mov [gs:ebp + 6], cx
		movzx eax, word [ss:_Result]
		mov [gs:ebp + 8], eax
		invoke IVideo.Write_text_line

		inc ecx
		cmp ecx, edx
		jae .Return

		movzx eax, word [ds:ebx + Screen_width]
		add esi, eax
		cmp esi, edi
		jb .Loop
		sub esi, edi
		jmp .Loop
		
	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	ret

Function_Lock_console:	; Function 5
	.Console equ dword [gs:ebp - 4] ; Console : Handle_type

	push ebp
	add ebp, 4

	push esi

	mov esi, [fs:IConsole]
	mov esi, [fs:esi + Var.Console_table]

	mov [gs:ebp], esi
	invoke IData.Access_table

	mov eax, .Console
	call Translate_console_handle

	.Spinlock:
		lock bts dword [ds:eax + Console_lock], 0
		jnc .Finish
		invoke IThread.Yield
		jmp .Spinlock

	.Finish:
	mov esi, [ss:_ThreadIdx]
	mov [ds:eax + Console_lock_owner], esi

	xor eax, eax

	.Return:
	pop esi

	pop ebp
	ret

	restore .Console

Function_Release_console:  ; Function 6
	.Console equ dword [gs:ebp - 4] ; Console : Handle_type

	push ebp
	add ebp, 4

	push ecx
	push esi

	mov esi, [fs:IConsole]
	mov esi, [fs:esi + Var.Console_table]

	mov eax, .Console
	call Translate_console_handle

	mov ecx, [ss:_ThreadIdx]
	cmp [ds:eax + Console_lock_owner], ecx
	jne .Error1

	xor ecx, ecx
	mov [ds:eax + Console_lock_owner], ecx
	lock btr dword [ds:eax + Console_lock], 0

	mov [gs:ebp], esi
	invoke IData.Finish_access_table

	.Return:
	pop esi
	pop ecx

	pop ebp
	ret

	.Error1:
	mov eax, YOU_DO_NOT_OWN_LOCK
	jmp .Return

	restore .Console

Translate_console_handle:
	; EAX : Console
	; ESI : Console_table
	; Result in EAX

	mov [gs:ebp], esi
	mov [gs:ebp + 4], eax
	invoke IData.Get_table_entry

	mov eax, [ss:_Result]
	add eax, esi
	ret