; 8042.ASM - IKeyboard 8042 Module
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

IKeyboard = $100400
; Function 1: Init_Keyboard
; Function 2: Read (var Buffer : Array of Char; Count : Word; var NumberOfCharsRead : Word)

Function_Init:
	push ebx
	push edi

	mov ebx, eax
	mov edi, IKeyboard
	cld
	stosd
	lea eax, [ebx+Function_Init_Keyboard]
	stosd
	lea eax, [ebx+Function_Read]
	stosd

	call Function_Init_Keyboard

	cli

	dec esp
	mov byte [esp], $21
	lea eax, [ebx+Procedure_IRQ1]
	push eax
	call dword [IInterrupt.Install_ISR]

	dec esp
	mov byte [esp], 1
	call dword [IInterrupt.Enable_IRQ]

	sti

	xor eax, eax
	pop edi
	pop ebx
	ret

Function_Init_Keyboard:
	.loop1:
	in al, $64
	test al, 10b
	jnz .loop1

	mov al, $AA
	out $64, al

	.loop2:
	in al, $64
	test al, 1
	jz .loop2

	in al, $60
	cmp al, $55
	jne .Keyboard_Fault

	mov al, $AB
	out $64, al

	.loop3:
	in al, $64
	test al, 1
	jz .loop3

	in al, $60
	cmp al, 0
	jne .Keyboard_Fault

	mov al, $60
	out $64, al

	.loop4:
	in al, $64
	test al, 10b
	jnz .loop4

	mov al, 00101001b
	out $60, al

	xor eax, eax
	ret
	.Keyboard_Fault:
	mov eax, -1
	ret

Function_Read:
	.Buffer equ dword [ebp+14]
	.Count equ word [ebp+12]
	.NumberOfCharsRead equ dword [ebp+8]

	enter 0, 0
	push ebx
	push ecx

	mov ebx, [IKeyboard]
	mov cx, .Count
	mov eax, .Buffer

	cmp cx, 0
	je .Error1

	mov [ebx+Var.Buffer], eax
	mov [ebx+Var.Count], cx

	bts [ebx+Var.Flag], Read_bit
	.Loop_until_finish_Reading:
	hlt
	bt [ebx+Var.Flag], Read_bit
	jc .Loop_until_finish_Reading

	sub cx, [ebx+Var.Count]
	mov ebx, .NumberOfCharsRead
	mov [ebx], cx
	xor eax, eax

	.Return:
	pop ecx
	pop ebx
	leave
	ret 10
	.Error1:
	mov eax, INVALID_COUNT
	jmp .Return

	restore .Buffer
	restore .Count
	restore .NumberOfCharsRead

Procedure_IRQ1:
	pusha

	xor eax, eax
	mov ebx, [IKeyboard]

	.loop1:
	in al, $64
	test al, 1
	jz .out1
	in al, $60

	mov si, ax

	.Case_AL_of:
		._F0:
		cmp al, $F0
		jne ._E0
		bts word [ebx+Var.Flag], Release_bit
		jmp .End_Case

		._E0:
		cmp al, $E0
		jne ._Shift
		bts word [ebx+Var.Flag], Escape_bit
		jmp .End_Case

		._Shift:
		cmp al, $12
		je @f
		cmp al, $59
		je @f
		jne .Else
		@@: btr word [ebx+Var.Flag], Release_bit
		jc ._Release_Shift
		._Hold_Shift:
		bts word [ebx+Var.Flag], Shift_bit
		jmp .End_Case
		._Release_Shift:
		btr word [ebx+Var.Flag], Shift_bit
		jmp .End_Case
	.Else:
		; Special key with escape code is ignored for now
		; Scancode after release code is ignored
		; And if the Read_bit isn't set, the key will be ignored too
		btr word [ebx+Var.Flag], Release_bit
		setc ah
		btr word [ebx+Var.Flag], Escape_bit
		setc dl
		or ah, dl
		bt word [ebx+Var.Flag], Read_bit
		setnc dl
		or ah, dl
		cmp ah, 1
		je .End_Case

		cmp al, $5A	; Enter key scancode is $5A
		je ._Stop_Reading

		bt word [ebx+Var.Flag], Shift_bit
		jc ._Use_Shift_Scancode

		._Use_Normal_Scancode:
		add ebx, Scancodes_Table
		jmp ._Translate_Scancode

		._Use_Shift_Scancode:
		add ebx, Shift_Scancodes_Table

		._Translate_Scancode:
		xlatb
		cmp al, 0
		je .End_Case

		mov ebx, [IKeyboard]
		mov edi, [ebx+Var.Buffer]
		mov [edi], al

		dec esp
		mov [esp], al
		call dword [ISysUtils.Write_Char]

		dec word [ebx+Var.Count]
		cmp word [ebx+Var.Count], 0
		je ._Stop_Reading
		inc dword [ebx+Var.Buffer]
		jmp .End_Case

		._Stop_Reading:
		btr word [ebx+Var.Flag], Read_bit
		call dword [IVideo.New_Line]
	.End_Case:

	jmp .loop1

	.out1:
	test al, 10b
	jnz .loop1

	call dword [IInterrupt.Send_EOI]
	popa
	iret

Const:
	Release_bit = 0
	Shift_bit = 1
	Escape_bit = 2
	Read_bit = 3

Var:
	.Flag dw 0
	.Buffer dd $FFFFFFFF
	.Count dw 0

Scancodes_Table:
	._0 db $00
	._1 db $00 ; F9
	._2 db $00
	._3 db $00 ; F5
	._4 db $00 ; F3
	._5 db $00 ; F1
	._6 db $00 ; F2
	._7 db $00 ; F12
	._8 db $00
	._9 db $00 ; F10
	._A db $00 ; F8
	._B db $00 ; F6
	._C db $00 ; F4
	._D db $00 ; Tab
	._E db '`' ; `
	._F db $00

	._10 db $00
	._11 db $00 ; Alt
	._12 db $00 ; Shift
	._13 db $00
	._14 db $00 ; Ctrl
	._15 db 'q' ; q
	._16 db '1' ; 1
	._17 db $00
	._18 db $00
	._19 db $00
	._1A db 'z' ; z
	._1B db 's' ; s
	._1C db 'a' ; a
	._1D db 'w' ; w
	._1E db '2' ; 2
	._1F db $00

	._20 db $00
	._21 db 'c' ; c
	._22 db 'x' ; x
	._23 db 'd' ; d
	._24 db 'e' ; e
	._25 db '4' ; 4
	._26 db '3' ; 3
	._27 db $00
	._28 db $00
	._29 db ' ' ; Space
	._2A db 'v' ; v
	._2B db 'f' ; f
	._2C db 't' ; t
	._2D db 'r' ; r
	._2E db '5' ; 5
	._2F db $00

	._30 db $00
	._31 db 'n' ; n
	._32 db 'b' ; b
	._33 db 'h' ; h
	._34 db 'g' ; g
	._35 db 'y' ; y
	._36 db '6' ; 6
	._37 db $00
	._38 db $00
	._39 db $00
	._3A db 'm' ; m
	._3B db 'j' ; j
	._3C db 'u' ; u
	._3D db '7' ; 7
	._3E db '8' ; 8
	._3F db $00

	._40 db $00
	._41 db ',' ; ,
	._42 db 'k' ; k
	._43 db 'i' ; i
	._44 db 'o' ; o
	._45 db '0' ; 0
	._46 db '9' ; 9
	._47 db $00
	._48 db $00
	._49 db '.' ; .
	._4A db '/' ; /
	._4B db 'l' ; l
	._4C db ';' ; ;
	._4D db 'p' ; p
	._4E db '-' ; -
	._4F db $00

	._50 db $00
	._51 db $00
	._52 db '''' ; '
	._53 db $00
	._54 db '[' ; [
	._55 db '=' ; =
	._56 db $00
	._57 db $00
	._58 db $00
	._59 db $00 ; Right Shift
	._5A db $00 ; Enter
	._5B db ']' ; ]
	._5C db $00
	._5D db '\' ; \
	._5E db $00
	._5F db $00

	repeat 160
	db $00
	end repeat

Shift_Scancodes_Table:
	._0 db $00
	._1 db $00 ; F9
	._2 db $00
	._3 db $00 ; F5
	._4 db $00 ; F3
	._5 db $00 ; F1
	._6 db $00 ; F2
	._7 db $00 ; F12
	._8 db $00
	._9 db $00 ; F10
	._A db $00 ; F8
	._B db $00 ; F6
	._C db $00 ; F4
	._D db $00 ; Tab
	._E db '~' ; `
	._F db $00

	._10 db $00
	._11 db $00 ; Alt
	._12 db $00 ; Shift
	._13 db $00
	._14 db $00 ; Ctrl
	._15 db 'Q' ; q
	._16 db '!' ; 1
	._17 db $00
	._18 db $00
	._19 db $00
	._1A db 'Z' ; z
	._1B db 'S' ; s
	._1C db 'A' ; a
	._1D db 'W' ; w
	._1E db '@' ; 2
	._1F db $00

	._20 db $00
	._21 db 'C' ; c
	._22 db 'X' ; x
	._23 db 'D' ; d
	._24 db 'E' ; e
	._25 db '$' ; 4
	._26 db '#' ; 3
	._27 db $00
	._28 db $00
	._29 db ' ' ; Space
	._2A db 'V' ; v
	._2B db 'F' ; f
	._2C db 'T' ; t
	._2D db 'R' ; r
	._2E db '%' ; 5
	._2F db $00

	._30 db $00
	._31 db 'N' ; n
	._32 db 'B' ; b
	._33 db 'H' ; h
	._34 db 'G' ; g
	._35 db 'Y' ; y
	._36 db '^' ; 6
	._37 db $00
	._38 db $00
	._39 db $00
	._3A db 'M' ; m
	._3B db 'J' ; j
	._3C db 'U' ; u
	._3D db '&' ; 7
	._3E db '*' ; 8
	._3F db $00

	._40 db $00
	._41 db '<' ; ,
	._42 db 'K' ; k
	._43 db 'I' ; i
	._44 db 'O' ; o
	._45 db ')' ; 0
	._46 db '(' ; 9
	._47 db $00
	._48 db $00
	._49 db '>' ; .
	._4A db '?' ; /
	._4B db 'L' ; l
	._4C db ':' ; ;
	._4D db 'P' ; p
	._4E db '_' ; -
	._4F db $00

	._50 db $00
	._51 db $00
	._52 db '"' ; '
	._53 db $00
	._54 db '{' ; [
	._55 db '+' ; =
	._56 db $00
	._57 db $00
	._58 db $00
	._59 db $00 ; Right Shift
	._5A db $00 ; Enter
	._5B db '}' ; ]
	._5C db $00
	._5D db '|' ; \
	._5E db $00
	._5F db $00

	repeat 160
	db $00
	end repeat