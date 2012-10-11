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
; Function 2: Write_console (Console : Handle_type; in Str : String)
; Function 3: Read_console (Console : Handle_type; out Str : String; Max_size : Cardinal)
; Function 4: Switch_console (Console : Handle_type)

Const:
	NumOf_Windows = 2
	Write_Flag = 0
Error_Code:
	CANNOT_ALLOC_MEMORY = -1
	STRING_LENGTH_IS_ZERO = -2

jmp Function_Init
Interface:
	dd Function_Alloc_console
	dd Function_Write_console
	dd 0 ;Function_Read_console
	dd Function_Switch_console

Var:
	.Active_console dd 0
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

	pop esi
	pop edi
	pop ebx
	ret

Function_Alloc_console: 	; Function 1
	.Console equ dword [ebp + 8]	; out Console : Handle_type

	push ebp
	mov ebp, esp

	push 0
	push .Console
	push (2000 * 8 + 4 * 4)
	invoke ISystem.Allocate

	test eax, eax
	jne .Error1

	mov eax, .Console
	mov eax, [eax]
	mov [eax], dword 0
	mov [eax + 8], dword 0
	mov [eax + 12], dword $3E007FFF
	mov [eax + 16], dword 2000
	mov [eax + 20], word 80
	mov [eax + 22], word 25

	xor eax, eax

	.Return:
	leave
	ret 4
	.Error1:
	mov eax, CANNOT_ALLOC_MEMORY
	jmp .Return

	restore .Console

Function_Write_console: 	; Function 2
	.Console equ dword [ebp + 12]	; Console : Handle_type
	.Str equ dword [ebp + 8]	; in Str : String

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push esi

	mov esi, .Str
	cmp [esi], dword 0
	je .Error1

	mov ebx, .Console

	.Spinlock:
		lock bts dword [ebx], 0
		jnc .Begin
		invoke IThread.Yield
		jmp .Spinlock

	.Begin:
	call Procedure_Convert	; parameters are ebx and esi
	test esi, esi
	jz .Error2

	; Check if this console is active console
	mov eax, [fs:IConsole]
	cmp ebx, [fs:eax + Var.Active_console]
	jne .Done

	mov ecx, [ebx + 8]
	lea eax, [ebx + 24 + ecx * 8]
	push eax
	push ecx
	push dword [esi]
	invoke IVideo.Blit_text

	; Update cursor
	mov eax, [esi]
	add [ebx + 8], eax

	.Done: xor eax, eax
	.Unlock: lock btr dword [ebx], 0

	.Return:
	pop esi
	pop ecx
	pop ebx
	leave
	ret 8
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
