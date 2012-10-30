; Utility.asm - Utility Functions Module
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

IUtility = $100A00
; Function 1: Cardinal_to_HexStr (Num : Cardinal; var HexStr : Old_string; Count : Word)
; Function 2: Write_Cardinal_Hex (Num : LongWord)
; Function 3: Write_Char (Ch : Char)
; Function 4: Write_String (var Str : Old_string)

; Function 11: HexStr_to_Cardinal (var Num : Cardinal; var HexStr : Old_string; Word_size : Byte)
; Function 12: HexChar_to_Byte (var Num : Byte; HexChar : Char)

Error_Code:
	INVALID_COUNT = -1
	NOT_LARGE_ENOUGH = -2
	NULL_STRING = -3
	STRING_IS_NOT_A_NUMBER = -4

Function_Init:
	push ebx
	push edi

	mov ebx, eax
	mov edi, IUtility
	mov [fs:edi], eax

	lea eax, [ebx+Function_Cardinal_to_HexStr]
	mov [fs:edi + 4], eax
	lea eax, [ebx+Function_Write_Cardinal_Hex]
	mov [fs:edi + 8], eax
	lea eax, [ebx+Function_Write_Char]
	mov [fs:edi + 12], eax
	lea eax, [ebx+Function_Write_String]
	mov [fs:edi + 16], eax

	lea eax, [ebx+Function_HexStr_to_Cardinal]
	mov [fs:edi + 44], eax
	lea eax, [ebx+Function_HexChar_to_Byte]
	mov [fs:edi + 48], eax

	xor eax, eax
	pop edi
	pop ebx
	ret

Function_Cardinal_to_HexStr:
	.Num equ dword [ebp + 14]
	.HexStr equ dword [ebp + 10]
	.Count equ word [ebp + 8]

	._Count equ word [esp + 4]
	._HexStr equ dword [esp + 6]
	._Num equ dword [esp + 10]

	cmp ._Count, 0
	je .Error1

	cmp ._Num, 0
	je .Zero_case

	push ebp
	mov ebp, esp
	push ebx
	push ecx
	push edx
	push edi

	mov edx, .Num
	or ch, True
	xor ebx, ebx
	mov edi, .HexStr

	mov cl, 7
	.Loop:
	mov eax, edx
	shl cl, 2
	shr eax, cl
	shr cl, 2
	and al, $F

	test al, al
	setz ah
	and ah, ch
	jnz .Continue_loop
	xor ch, ch

	cmp al, $A
	jae .j1
	add al, '0' - 0
	jmp .j2
	.j1: add al, 'A' - $A
	.j2:

	inc ebx
	cmp bx, .Count
	ja .Error2

	mov [edi + 2 + ebx - 1], al

	.Continue_loop:
	dec cl
	jns .Loop

	mov [edi], bx

	xor eax, eax

	.Return:
	pop edi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 10

	.Error1:
	mov eax, INVALID_COUNT
	ret 10

	.Zero_case:
	mov eax, ._HexStr
	mov word [eax], 1
	mov byte [eax + 2], '0' - 0
	xor eax, eax
	ret 10

	.Error2:
	mov [edi], word 0
	mov eax, NOT_LARGE_ENOUGH
	jmp .Return

	restore .Num
	restore .HexStr
	restore .Count
	restore ._Num
	restore ._HexStr
	restore ._Count

Function_Write_Cardinal_Hex:
	.Num equ dword [ebp + 8]
	._Num equ dword [esp + 4]
	; Local Var:
	.Str equ dword [ebp - 10] ; Str : Old_string[8]

	mov eax, ._Num
	test eax, eax
	je .Zero_case

	push ebp
	mov ebp, esp
	sub esp, 10

	push eax
	lea eax, .Str
	push eax
	mov [esp - 2], word 8
	sub esp, 2
	call Function_Cardinal_to_HexStr

	lea eax, .Str
	push eax
	call Function_Write_String

	leave
	xor eax, eax
	ret 4
	.Zero_case:
	mov [esp - 1], byte '0'
	dec esp
	call Function_Write_Char
	xor eax, eax
	ret 4

	restore .Num
	restore ._Num
	restore .Str

Function_Write_Char:
	.Value equ byte [esp + 4]
	lea eax, .Value
	push eax
	sub esp, 2
	mov word [esp], 1
	call dword [cs:IVideo.Write_Telex]
	ret 1
	restore .Value

Function_Write_String:
	.Str equ dword [esp + 8]
	push ebx
	mov ebx, .Str
	mov eax, ebx
	add ebx, 2
	mov eax, [eax]
	push ebx
	mov [esp - 2], ax
	sub esp, 2
	call dword [cs:IVideo.Write_Telex]
	pop ebx
	ret 4
	restore .Str

Function_HexStr_to_Cardinal:
	.Num equ dword [ebp + 13] ; var Num : Cardinal
	.HexStr equ dword [ebp + 9] ; var HexStr : Old_string
	.Word_size equ byte [ebp + 8] ; Word_size : Byte
	._HexStr equ dword [esp + 5]
	._Word_size equ byte [esp + 4]
	; Local Var:
	.t equ byte [ebp + 9] ; var t : Byte

	mov al, ._Word_size
	cmp al, 1
	je .Continue
	cmp al, 2
	je .Continue
	cmp al, 4
	je .Continue
	.Error1:
	mov eax, INVALID_COUNT
	ret 9

	.Continue:
	mov eax, ._HexStr
	cmp [eax], word 0
	je .Error2	; Null string error

	push ebp
	mov ebp, esp
	dec esp
	push ebx
	push ecx
	push esi

	mov esi, .HexStr
	xor eax, eax
	xor ecx, ecx
	mov ax, [esi]
	xor ebx, ebx

	.Loop1: ; Loop until found a non-zero
	inc ecx
	cmp [esi + 1 + ecx], byte '0'
	setne bl
	jne .Out1
	cmp eax, ecx
	ja .Loop1
	.Out1:

	test ebx, ebx
	jz .Zero_case

	sub eax, ecx
	mov bx, ax
	add ax, 2
	shr eax, 1
	cmp al, .Word_size
	ja .Error3	; This value is greater than maximum of 32 bit integer

	lea esi, [esi + 1 + ecx]
	mov ax, bx
	mov ch, al
	xor ebx, ebx

	.Loop2:
	lea eax, .t
	push eax
	mov al, [esi]
	mov [esp - 1], al
	dec esp
	call Function_HexChar_to_Byte
	test eax, eax
	jnz .Error4	; Character is not a hexadecimal number

	mov al, .t
	mov cl, ch
	shl cl, 2
	shl eax, cl
	add ebx, eax

	dec ch
	js .Out2
	inc esi
	jmp .Loop2
	.Out2:

	.Finish:
	mov al, .Word_size
	mov esi, .Num
	.al_4:
	cmp al, 4
	jne .al_2
	mov [esi], ebx
	.al_2:
	cmp al, 2
	jne .al_1
	mov [esi], bx
	.al_1:
	mov [esi], bl

	xor eax, eax

	.Return:
	pop esi
	pop ecx
	pop ebx
	leave
	ret 9
	.Zero_case:
	xor ebx, ebx
	jmp .Finish
	.Error2:
	mov eax, NULL_STRING
	ret 9
	.Error3:
	mov eax, NOT_LARGE_ENOUGH
	jmp .Return
	.Error4:
	mov eax, STRING_IS_NOT_A_NUMBER
	jmp .Return

	restore .Num
	restore .HexStr
	restore .Word_size
	restore ._HexStr
	restore ._Word_size
	restore .t

Function_HexChar_to_Byte:	; Function 12
	.Num equ dword [esp + 9]  ; var Num : Byte
	.HexChar equ byte [esp + 8]	  ; HexChar : Char

	push edi

	mov al, .HexChar
	mov edi, .Num
	cmp al, '0'
	jb .Not_a_number

	cmp al, '9'
	ja .j1

	sub al, '0'
	jmp .Success

	.j1:
	cmp al, 'A'
	jb .Not_a_number

	cmp al, 'F'
	ja .j2

	sub al, 'A' - 10
	jmp .Success

	.j2:
	cmp al, 'a'
	jb .Not_a_number

	cmp al, 'f'
	ja .Not_a_number

	sub al, 'a' - 10

	.Success:
	mov [edi], al
	xor eax, eax
	.Return:
	pop edi
	ret 5
	.Not_a_number:
	mov eax, STRING_IS_NOT_A_NUMBER
	jmp .Return

	restore .Num
	restore .HexChar