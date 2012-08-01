; Sysutils.asm - Utility Functions Module
; Written in 2012 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

use32
include 'include\Header.inc'
include 'include\errcode.inc'

ISysUtils = $100A00
; Function 1: Byte_to_HexStr (Num : Byte; var HexStr : Array of Char)
; Function 2: Write_Byte (Value : Byte; Format : Byte)
; Function 3: Write_Char (Value : Char)
; Function 4: Write_String (var Str : String)
; Function 5: Create_Ring_Buffer (var Buffer : Array; Count : LongWord)
; Function 6: Ring_Buffer_Read (var Buffer : Array; var Out : Byte)
; Function 7: Ring_Buffer_Write (var Buffer : Array; In : Byte)
; Function 8: Clear_Ring_Buffer (var Buffer : Array)

Function_Init:
	push ebx
	push edi

	cld
	mov ebx, eax
	mov edi, ISysUtils
	stosd
	lea eax, [ebx+Function_Byte_to_HexStr]
	stosd
	lea eax, [ebx+Function_Write_Byte]
	stosd
	lea eax, [ebx+Function_Write_Char]
	stosd
	lea eax, [ebx+Function_Write_String]
	stosd
	lea eax, [ebx+Function_Create_Ring_Buffer]
	stosd
	lea eax, [ebx+Function_Ring_Buffer_Read]
	stosd
	lea eax, [ebx+Function_Ring_Buffer_Write]
	stosd
	lea eax, [ebx+Function_Clear_Ring_Buffer]
	stosd

	xor eax, eax
	pop edi
	pop ebx
	ret

Function_Byte_to_HexStr:
	.Num equ byte [ebp+12]
	.HexStr equ dword [ebp+8]

	enter 0, 0
	push edi

	mov al, .Num
	mov ah, al
	mov edi, .HexStr

	and al, $0F
	shr ah, 4

	cmp al, $A
	jb j1
	add al, 'A' - $A
	jmp j2
	j1: add al, '0'

	j2:
	cmp ah, $A
	jb j3
	add ah, 'A' - $A
	jmp j4
	j3: add ah, '0'

	j4:
	mov [edi], ah
	mov [edi+1], al

	pop edi
	leave
	ret 5
	restore .Num
	restore .HexStr

Function_Write_Byte:
	.Value equ byte [ebp+9]
	.Format equ byte [ebp+8]

	.SizeOf_Buf = 8
	.Buf equ ebp-.SizeOf_Buf

	enter 0, .SizeOf_Buf

	mov al, .Format
	cmp .Format, Format_Hex
	je .Hex
	jne .Error1

	.Hex:
	dec esp
	mov al, .Value
	mov [esp], al
	lea eax, [.Buf]
	push eax
	call Function_Byte_to_HexStr

	lea eax, [.Buf]
	push eax
	sub esp, 2
	mov word [esp], 2
	call dword [IVideo.Write_Telex]

	xor eax, eax

	.Return:
	leave
	ret 2
	.Error1:
	mov eax, FORMAT_NOT_SUPPORTED
	jmp .Return
	restore .Value
	restore .Format

Function_Write_Char:
	.Value equ byte [esp+4]
	lea eax, .Value
	push eax
	sub esp, 2
	mov word [esp], 1
	call dword [IVideo.Write_Telex]
	ret 1
	restore .Value

Function_Write_String:

Function_Create_Ring_Buffer:
	.Buffer equ dword [esp+12]
	.Count equ dword [esp+8]

	push ebx

	cmp .Count, 0
	je .Error1

	mov ebx, .Buffer
	mov eax, .Count
	add eax, 12
	mov [ebx+4], eax
	mov [ebx+8], eax
	add eax, ebx
	mov [ebx], eax

	xor eax, eax

	.Return:
	pop ebx
	ret 8
	.Error1:
	mov eax, INVALID_COUNT
	jmp .Return
	restore .Buffer
	restore .Count

Function_Ring_Buffer_Read:
	.Buffer equ dword [esp+12]
	.Out equ dword [esp+8]

	push ebx

	mov ebx, .Buffer
	mov eax, [ebx+4]

	cmp eax, [ebx+8]
	je .Buffer_Empty

	inc eax
	cmp eax, [ebx]
	jna .j1

	lea eax, [ebx+12]

	.j1: mov [ebx+4], eax
	mov bl, [eax]
	mov eax, .Out
	mov [eax], bl

	xor eax, eax

	.Return:
	pop ebx
	ret 8
	.Buffer_Empty:
	mov eax, BUFFER_EMPTY
	jmp .Return
	restore .Buffer
	restore .Out

Function_Ring_Buffer_Write:
	.Buffer equ dword [esp+9]
	.In equ byte [esp+8]

	push ebx

	mov ebx, .Buffer
	mov eax, [ebx+8]

	inc eax
	cmp eax, [ebx]
	jna .j1

	lea eax, [ebx+12]

	.j1:
	cmp eax, [ebx+4]
	jne .j2

	inc eax
	cmp eax, [ebx]
	jna .j2

	lea eax, [ebx+12]

	.j2:
	mov [ebx+8], eax
	mov bl, .In
	mov [eax], bl

	xor eax, eax

	.Return:
	pop ebx
	ret 5
	restore .Buffer
	restore .Out

Function_Clear_Ring_Buffer:
	.Buffer equ dword [esp+8]
	push ebx
	mov ebx, .Buffer
	mov eax, [ebx+8]
	mov [ebx+4], eax
	pop ebx
	ret 4