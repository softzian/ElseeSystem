; Convert.asm - Conversion library
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

IConvert = $100C00
; Function 1: UTF16_to_raw_UTF32 (in Str : String; out Buff : Array of Char32)

Const:

Error_Code:
	CANNOT_ALLOC_MEMORY = -1
	STRING_LENGTH_IS_ZERO = -2

jmp Function_Init
Interface:
	dd Function_UTF16_to_raw_UTF32

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	mov edi, IConvert
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, IConvert + 4 * 1
		jna .Loop

	pop esi
	pop edi
	pop ebx
	ret

Function_UTF16_to_raw_UTF32:	; Function 1, BAD implement, need rework
	.Str equ dword [ebp + 12]
	.Buff equ dword [ebp + 8]

	push ebp
	mov ebp, esp
	push ecx
	push esi
	push edi

	mov esi, .Str
	xor eax, eax
	mov edi, .Buff
	mov ecx, [esi]
	add esi, 12

	.Loop:
		mov ax, [esi]
		mov [edi], eax
		add esi, 2
		add edi, 4
		loop .Loop

	.Return:
	pop edi
	pop esi
	pop ecx
	leave
	ret 8

	restore .Str
	restore .Buff

