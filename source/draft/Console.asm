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
; Function 1: Alloc_console (out Console : Handle_type)
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
	NumOf_Windows = 2
	Write_Flag = 0
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

	sub esp, 4
	mov eax, esp
	push eax
	push 1024
	invoke ISystem.Create_Message_Queue
	pop eax

	mov [fs:ebx + Var.Queue], eax

	push eax
	invoke IKeyboard.Set_target_queue

	sub esp, 4
	mov eax, esp
	push ebx	; ModuleIdx
	add ebx, Main_thread
	push ebx	; Start_point
	push $2000	; Stack_size
	push eax
	invoke IThread.New_Thread
	pop eax

	pop esi
	pop edi
	pop ebx
	ret

Main_thread:
	mov ebp, esp
	sub esp, 16
	mov ebx, [fs:IConsole]
	xor ecx, ecx

	.Message_loop:
	lea eax, [ebp - 16]
	push dword [fs:ebx + Var.Queue]
	push eax
	invoke ISystem.Get_Message

	test eax, eax
	jnz .Wait

	push dword [fs:ebx + Var.Active_console]
	lea eax, [ebp - 16]
	push eax
	push 1
	call Function_Write_console

	.Wait:
	invoke IThread.Yield
	jmp .Message_loop

Function_Alloc_console: 	; Function 1
	.Console equ dword [ebp + 8]	; out Console : Handle_type

	push ebp
	mov ebp, esp
	push ebx

	push .Console
	push 1024
	invoke ISystem.Create_Message_Queue

	test eax, eax
	jnz .Error2

	mov ebx, .Console
	mov ebx, [ebx]

	push 0
	push .Console
	push (2000 * 8 + 4 * 6)
	invoke ISystem.Allocate

	test eax, eax
	jnz .Error1

	mov eax, .Console
	mov eax, [eax]
	mov [eax], dword 0	; Lock
	mov [eax + 4], ebx	; Message queue
	mov [eax + 8], dword 0	; Cursor
	mov [eax + 12], dword $3E007FFF ; Default attribute
	mov [eax + 16], dword 2000	; Size in character
	mov [eax + 20], word 80 	; Width
	mov [eax + 22], word 25 	; Height

	xor eax, eax

	.Return:
	pop ebx
	leave
	ret 4
	.Error1:
	mov eax, CANNOT_ALLOC_MEMORY
	jmp .Return
	.Error2:
	mov eax, CANNOT_ALLOC_QUEUE
	jmp .Return

	restore .Console

Function_Write_console: 	; Function 2
	.Console equ dword [ebp + 16]	; Console : Handle_type
	.Str equ dword [ebp + 12]	; in Str : raw_UTF32_string
	.Count equ dword [ebp + 8]	; Count : Cardinal

	push ebp
	mov ebp, esp
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
		lock bts dword [ebx], 0
		jnc .Begin
		invoke IThread.Yield
		jmp .Spinlock

	.Begin:
	xor ecx, ecx
	mov esi, .Str
	mov eax, [ebx + 8]
	lea edi, [ebx + 24 + eax * 8]
	mov ebx, [ebx + 12]
	.Loop:
		mov eax, [esi + ecx * 4]
		mov [edi + ecx * 8], eax
		mov [edi + ecx * 8 + 4], ebx
		inc ecx
		cmp ecx, edx
		jb .Loop

	; Check if this console is active console
	mov ebx, .Console
	mov eax, [fs:IConsole]
	cmp ebx, [fs:eax + Var.Active_console]
	jne .Done

	mov ecx, [ebx + 8]
	lea eax, [ebx + 24 + ecx * 8]
	push eax
	push ecx
	push edx
	invoke IVideo.Blit_text

	; Update cursor
	add [ebx + 8], edx

	.Done: xor eax, eax
	.Unlock: lock btr dword [ebx], 0

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 12
	.Error1:
	mov eax, STRING_LENGTH_IS_ZERO
	jmp .Return
	.Error2:
	mov eax, CANNOT_ALLOC_MEMORY
	jmp .Unlock

	restore .Console
	restore .Str

Procedure_Convert:	; Convert text in String format to console format
			; and put it in screen buffer
	; EBX Console : Handle_type
	; ESI in Str : String
	push eax
	push ecx
	push edx

	sub esp, 4
	mov eax, esp
	push 0
	push eax
	mov eax, [esi]
	shl eax, 1
	push eax
	invoke ISystem.Allocate
	pop edx

	test eax, eax
	jnz .Error1

	push esi
	push edx
	invoke IConvert.UTF16_to_raw_UTF32

	push esi
	push edi
	xor ecx, ecx
	mov esi, [esi]	; Length
	mov edi, [ebx + 12]	; Attribute

	.Loop:
		mov eax, [edx + ecx * 4]
		mov [ebx + 24 + ecx * 8], eax
		mov [ebx + 24 + ecx * 8 + 4], edi
		inc ecx
		cmp ecx, esi
		jb .Loop
	pop edi
	pop esi

	push 0
	push edx
	invoke ISystem.Deallocate

	.Return:
	pop edx
	pop ecx
	pop eax
	ret
	.Error1:
	mov esi, 0
	jmp .Return

Function_Switch_console:	; Function 4
	.Console equ dword [ebp + 8]	; Console : Handle_type

	push ebp
	mov ebp, esp
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
	lea ebx, [eax + 24]
	push ebx
	push 0
	push dword [eax + 16]
	invoke IVideo.Blit_text

	mov eax, .Console
	mov ebx, [fs:IConsole]
	mov [fs:ebx + Var.Active_console], eax

	.Done:
	lock btr dword [fs:ebx + Var.Lock], 0
	xor eax, eax

	.Return:
	pop ebx
	leave
	ret 4

	restore .Console
