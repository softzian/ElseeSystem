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
; Function 2: Write_console (Console : Handle_type; in Str : raw_UTF32_string; Count : Cardinal)
; Function 3: Read_console (Console : Handle_type; out Str : raw_UTF32_string; Max_count : Cardinal)
; Function 4: Switch_console (Console : Handle_type)

jmp Function_Init
Interface:
	dd Function_Alloc_console
	dd Function_Write_console
	dd 0 ;Function_Read_console
	dd Function_Switch_console

Const:
	; Console record
	Console_screen = 8
	Console_input = 4
	Console_lock = 0

	; Screen Buffer record
	Screen_cursor = 0
	Screen_attribute = 4
	Screen_size = 8
	Screen_width = 12
	Screen_height = 14

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
	; Local variable
	.Data equ dword [gs:ebp - 4]	; Data : Array

	add ebp, 4
	mov ebx, [fs:IConsole]

	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], dword $400
	invoke ISystem.Allocate

	mov esi, [ss:_Result]
	mov .Data, esi

	.Message_loop:
	mov eax, [fs:ebx + Var.Queue]
	mov [gs:ebp], eax
	invoke ISystem.Get_Message

	test eax, eax
	jnz .Wait

	mov eax, [ss:_Result]
	mov [ds:esi], eax

	mov eax, [fs:ebx + Var.Active_console]
	mov [gs:ebp], eax
	mov [gs:ebp + 4], esi
	mov [gs:ebp + 8], dword 1
	call Function_Write_console

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
	mov [gs:ebp + 4], dword (16 + 2000 * 8)
	invoke ISystem.Allocate

	mov eax, [ss:_Result]

	mov [ds:ecx + Console_lock], dword 0   ; Lock
	mov [ds:ecx + Console_input], ebx   ; Input queue
	mov [ds:ecx + Console_screen], eax   ; Screen buffer

	mov [ds:eax + Screen_cursor], dword 0			; Cursor
	mov [ds:eax + Screen_attribute], dword $3E007FFF       ; Default attribute
	mov [ds:eax + Screen_size], dword 2000		  ; Size in character
	mov [ds:eax + Screen_width], word 80		  ; Width
	mov [ds:eax + Screen_height], word 25		   ; Height

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

Function_Write_console: 	; Function 2
	.Console equ dword [gs:ebp - 12]	; Console : Handle_type
	.Str equ dword [gs:ebp - 8]		; in Str : raw_UTF32_string
	.Count equ dword [gs:ebp - 4]		; Count : Cardinal

	push ebp
	add ebp, 12
	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov edx, .Count
	test edx, edx
	jz .Error1

	mov ebx, .Console

	.Spinlock:
		lock bts dword [ds:ebx + Console_lock], 0
		jnc .Begin
		invoke IThread.Yield
		jmp .Spinlock

	.Begin:
	xor ecx, ecx
	mov esi, .Str

	mov ebx, [ds:ebx + Console_screen]
	mov eax, [ds:ebx + Screen_cursor]
	lea edi, [ebx + 16 + eax * 8]
	mov ebx, [ds:ebx + Screen_attribute]

	.Loop:
		mov eax, [ds:esi + ecx * 4]
		mov [ds:edi + ecx * 8], eax
		mov [ds:edi + ecx * 8 + 4], ebx
		inc ecx
		cmp ecx, edx
		jb .Loop

	; Check if this console is active console
	mov ebx, .Console
	mov eax, [fs:IConsole]
	cmp ebx, [fs:eax + Var.Active_console]
	jne .Done

	mov esi, [ds:ebx + Console_screen]
	mov ecx, [ds:esi + Screen_cursor]
	lea eax, [esi + 16 + ecx * 8]

	mov [gs:ebp], eax
	mov [gs:ebp + 4], ecx
	mov [gs:ebp + 8], edx
	invoke IVideo.Blit_text

	; Update cursor
	add [ds:esi + Screen_cursor], edx

	.Done: xor eax, eax
	.Unlock: lock btr dword [ds:ebx + Console_lock], 0

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, STRING_LENGTH_IS_ZERO
	jmp .Return

	restore .Console
	restore .Count
	restore .Str

Function_Switch_console:	; Function 4
	.Console equ dword [gs:ebp - 4]    ; Console : Handle_type

	push ebp
	add ebp, 4
	push ebx

	mov ebx, [fs:IConsole]
	mov eax, .Console
	cmp eax, [fs:ebx + Var.Active_console]
	je .Return

	.Spinlock:
		lock bts dword [fs:ebx + Var.Lock], 0
		jnc .Begin
		invoke IThread.Yield
		jmp .Spinlock

	.Begin:
	mov eax, [ds:eax + Console_screen]
	lea ebx, [eax + 16]
	mov eax, [ds:eax + Screen_size]

	mov [gs:ebp], ebx
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], eax
	invoke IVideo.Blit_text

	mov eax, .Console
	mov ebx, [fs:IConsole]
	mov [fs:ebx + Var.Active_console], eax

	.Done:
	lock btr dword [fs:ebx + Var.Lock], 0
	xor eax, eax

	.Return:
	pop ebx
	pop ebp
	ret

	restore .Console
