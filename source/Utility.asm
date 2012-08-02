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
; Function 1: Unsigned_to_HexStr1 (Num : Byte; var HexStr : String1; Count : Word)
; Function 2: Write_Unsigned_Hex (Num : LongWord)
; Function 3: Write_Char (Ch : Char)
; Function 4: Write_String1 (var Str : String1)
; Function 5: Create_Ring_Buffer (var Buffer : Ring_Buffer; Count : LongWord)
; Function 6: Ring_Buffer_Read (var Buffer : Ring_Buffer; var Out : Byte)
; Function 7: Ring_Buffer_Write (var Buffer : Ring_Buffer; In : Byte)
; Function 8: Clear_Ring_Buffer (var Buffer : Ring_Buffer)
; Function 9: Read_String1 (var Str : String1; Count : Word)
; Function 10: Read_Char (var Ch : Char)
; Function 11: HexStr1_to_Unsigned (var Num : Unsigned_Integer; var HexStr : String1; Word_size : Byte)
; Function 12: HexChar_to_Byte (var Num : Byte; HexChar : Char)

Function_Init:
	push ebx
	push edi

	cld
	mov ebx, eax
	mov edi, IUtility
	mov [edi], eax
	lea eax, [ebx+Function_Unsigned_to_HexStr1]
	mov [edi+4], eax
	lea eax, [ebx+Function_Write_Unsigned_Hex]
	mov [edi+8], eax
	lea eax, [ebx+Function_Write_Char]
	mov [edi+12], eax
	lea eax, [ebx+Function_Write_String1]
	mov [edi+16], eax
	lea eax, [ebx+Function_Create_Ring_Buffer]
	mov [edi+20], eax
	lea eax, [ebx+Function_Ring_Buffer_Read]
	mov [edi+24], eax
	lea eax, [ebx+Function_Ring_Buffer_Write]
	mov [edi+28], eax
	lea eax, [ebx+Function_Clear_Ring_Buffer]
	mov [edi+32], eax
	lea eax, [ebx+Function_Read_String1]
	mov [edi+36], eax
	lea eax, [ebx+Function_Read_Char]
	mov [edi+40], eax
	lea eax, [ebx+Function_HexStr1_to_Unsigned]
	mov [edi+44], eax
	lea eax, [ebx+Function_HexChar_to_Byte]
	mov [edi+48], eax

	xor eax, eax
	pop edi
	pop ebx
	ret

Function_Unsigned_to_HexStr1:
	.Num equ dword [ebp+14]
	.HexStr equ dword [ebp+10]
	.Count equ word [ebp+8]

	._Count equ word [esp+4]
	._HexStr equ dword [esp+6]
	._Num equ dword [esp+10]

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

	mov [edi+2+ebx-1], al

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
	mov byte [eax+2], '0' - 0
	xor eax, eax
	ret 10

	.Error2:
	mov [edi], word 0
	mov eax, BUFFER_NOT_LARGE_ENOUGH
	jmp .Return

	restore .Num
	restore .HexStr
	restore .Count
	restore ._Num
	restore ._HexStr
	restore ._Count

Function_Write_Unsigned_Hex:
	.Num equ dword [ebp+8]
	._Num equ dword [esp+4]
	; Local Var:
	.Str equ dword [ebp-10] ; Str : String[8]

	mov eax, ._Num
	test eax, eax
	je .Zero_case

	push ebp
	mov ebp, esp
	sub esp, 10

	push eax
	lea eax, .Str
	push eax
	mov [esp-2], word 8
	sub esp, 2
	call Function_Unsigned_to_HexStr1

	lea eax, .Str
	push eax
	call Function_Write_String1

	leave
	xor eax, eax
	ret 4
	.Zero_case:
	mov [esp-1], byte '0'
	dec esp
	call Function_Write_Char
	xor eax, eax
	ret 4

	restore .Num
	restore ._Num
	restore .Str

Function_Write_Char:
	.Value equ byte [esp+4]
	lea eax, .Value
	push eax
	sub esp, 2
	mov word [esp], 1
	call dword [IVideo.Write_Telex]
	ret 1
	restore .Value

Function_Write_String1:
	.Str equ dword [esp+8]
	push ebx
	mov ebx, .Str
	mov eax, ebx
	add ebx, 2
	mov eax, [eax]
	push ebx
	mov [esp-2], ax
	sub esp, 2
	call dword [IVideo.Write_Telex]
	pop ebx
	ret 4
	restore .Str

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
	restore .In

Function_Clear_Ring_Buffer:
	.Buffer equ dword [esp+8]

	push ebx
	mov ebx, .Buffer
	mov eax, [ebx+8]
	mov [ebx+4], eax
	pop ebx
	ret 4

	restore .Buffer

Function_Read_String1:
	.Str equ dword [esp+10]
	.Count equ word [esp+8]

	push ebx
	mov ebx, .Str
	mov ax, .Count
	add ebx, 2
	mov [esp-6], ax
	mov [esp-4], ebx
	sub ebx, 2
	mov [esp-10], ebx
	sub esp, 10
	call dword [IKeyboard.Read]
	pop ebx
	ret 6

	restore .Str
	restore .Count

Function_Read_Char:
	.Ch equ dword [esp+8]
	push ebx

	mov ebx, .Ch
	mov [esp-6], ebx
	lea eax, [esp-2]
	mov word [esp-8], 1
	mov [esp-12], eax
	sub esp, 12
	call dword [IKeyboard.Read]

	cmp word [esp], 0
	je .Set_Result_to_Nul
	jne .Return

	.Set_Result_to_Nul:
	mov byte [ebx], 0

	.Return:
	add esp, 2
	pop ebx
	ret 4

	restore .Ch

Function_HexStr1_to_Unsigned:
	.Num equ dword [ebp+13] ; var Num : Unsigned_Integer
	.HexStr equ dword [ebp+9] ; var HexStr : String1
	.Word_size equ byte [ebp+8] ; Word_size : Byte
	._HexStr equ dword [esp+5]
	._Word_size equ byte [esp+4]
	; Local Var:
	.t equ byte [ebp+9] ; var t : Byte

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
	je .Error2	; Nul string error

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
	cmp [esi+1+ecx], byte '0'
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

	lea esi, [esi+1+ecx]
	mov ax, bx
	mov ch, al
	xor ebx, ebx

	.Loop2:
	lea eax, .t
	push eax
	mov al, [esi]
	mov [esp-1], al
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
	mov eax, BUFFER_NOT_LARGE_ENOUGH
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
	.Num equ dword [esp+9]	; var Num : Byte
	.HexChar equ byte [esp+8]	; HexChar : Char

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