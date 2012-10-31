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

IKeyboard = $100200
; Function 1: Init_Keyboard
; Function 2: Read_Keyboard_Buffer (var Ch : Char)
; Function 3: Clear_Keyboard_Buffer
; Function 4: Set_target_queue (Queue : Address)

jmp Function_Init
Interface:
	dd Function_Set_target_queue

Virtual_Codes:
	VK_LCTRL = $E500
	VK_RCTRL = $E501
	VK_LALT = $E502
	VK_RALT = $E503
	VK_LSHIFT = $E504
	VK_RSHIFT = $E505
	VK_F1 = $E506
	VK_F2 = $E507
	VK_F3 = $E508
	VK_F4 = $E509
	VK_F5 = $E50A
	VK_F6 = $E50B
	VK_F7 = $E50C
	VK_F8 = $E50D
	VK_F9 = $E50E
	VK_F10 = $E50F
	VK_F11 = $E510
	VK_F12 = $E511
Error_Code:
	BUFFER_EMPTY = -1
	INVALID_COUNT = -2

Function_Init:
	push ebp
	mov ebp, [gs:0]

	push ebx
	push esi
	push edi

	mov ebx, eax
	mov edi, IKeyboard
	lea esi, [eax + Interface]
	mov [fs:edi], eax
	add edi, 4

	.Loop:
		mov eax, [fs:esi]
		add eax, ebx
		mov [fs:edi], eax

		add edi, 4
		add esi, 4

		cmp edi, IKeyboard + 4 * 1
		jna .Loop

	cli

	call Function_Init_Keyboard

	lea eax, [ebx + Procedure_IRQ1]
	mov [gs:ebp], byte $21
	mov [gs:ebp + 1], eax
	add [gs:0], dword 5
	invoke IInterrupt.Install_ISR

	mov [gs:ebp], byte 1
	inc dword [gs:0]
	invoke IInterrupt.Enable_IRQ

	sti

	lea ebx, [ebx + Keyboard_Buffer]
	mov eax, 1024
	call Function_Create_Ring_Buffer

	mov ebx, [fs:IKeyboard]

	lea eax, [ebx + Processing_thread]
	mov [gs:ebp], eax
	add [gs:0], dword 4
	invoke IThread.New_Thread

	.Return:
	xor eax, eax

	pop edi
	pop esi
	pop ebx

	pop ebp
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

Function_Clear_Keyboard_Buffer:
	push ebx
	mov ebx, [fs:IKeyboard]
	add ebx, Keyboard_Buffer
	call Function_Clear_Ring_Buffer
	pop ebx
	ret

Function_Create_Ring_Buffer:
	.Buffer equ ebx
	.Count equ eax

	cmp eax, 0
	je .Error1

	add eax, 12
	mov [fs:ebx + 4], eax
	mov [fs:ebx + 8], eax
	add eax, ebx
	mov [fs:ebx], eax

	xor eax, eax
	ret

	.Error1:
	mov eax, INVALID_COUNT
	ret

	restore .Buffer
	restore .Count

Function_Ring_Buffer_Read:
	mov eax, [fs:ebx + 4]

	cmp eax, [fs:ebx + 8]
	je .Buffer_Empty

	inc eax
	cmp eax, [fs:ebx]
	jna .j1

	lea eax, [fs:ebx + 12]

	.j1: mov [fs:ebx + 4], eax
	mov al, [fs:eax]
	and eax, $FF
	ret
	.Buffer_Empty:
	mov ah, -1
	ret

Function_Ring_Buffer_Write:
	.Buffer equ dword [ebp - 5]
	.In equ byte [gs:ebp - 1]

	push ebp
	mov ebp, [gs:0]
	push ebx

	mov ebx, .Buffer
	mov eax, [fs:ebx + 8]

	inc eax
	cmp eax, [fs:ebx]
	jna .j1

	lea eax, [fs:ebx + 12]

	.j1:
	cmp eax, [fs:ebx + 4]
	jne .j2

	inc eax
	cmp eax, [fs:ebx]
	jna .j2

	lea eax, [fs:ebx + 12]

	.j2:
	mov [fs:ebx + 8], eax
	mov bl, .In
	mov [fs:eax], bl

	xor eax, eax

	.Return:
	pop ebx
	pop ebp
	sub [gs:0], dword 5
	ret

	restore .Buffer
	restore .In

Function_Clear_Ring_Buffer:
	mov eax, [fs:ebx + 8]
	mov [fs:ebx + 4], eax
	ret

Function_Set_target_queue:
	.Queue equ dword [gs:ebp - 4]	   ; Queue : Address

	push ebp
	mov ebp, [gs:0]
	push ebx

	mov ebx, [fs:IKeyboard]
	mov eax, .Queue
	mov [fs:ebx + Var.Target_queue], eax
	xor eax, eax

	pop ebx
	pop ebp
	sub [gs:0], dword 4
	ret

	restore .Queue

Procedure_IRQ1:
	pusha

	xor eax, eax
	mov ebx, [fs:IKeyboard]

	.loop1:
	in al, $64
	test al, 1
	jz .out1
	in al, $60

	.Next2:
	lea ecx, [ebx + Keyboard_Buffer]
	mov [gs:ebp], ecx
	mov [gs:ebp + 4], al
	add [gs:0], dword 5
	call Function_Ring_Buffer_Write

	jmp .loop1

	.out1:
	test al, 10b
	jnz .loop1

	invoke IInterrupt.Send_EOI
	popa
	iret

Processing_thread:
	mov ebx, [fs:IKeyboard]
	add ebx, Keyboard_Buffer
	call Function_Ring_Buffer_Read

	test ah, ah
	jnz .Buffer_is_empty

	.Translate:
	xor ecx, ecx
	mov ebx, [fs:IKeyboard]
	mov cx, [fs:ebx + US_Layout + eax * 2]

	test ecx, ecx
	jz .Set_flag

	.Send_message:
	mov edx, [fs:ebx + Var.Flag]
	mov [gs:ebp + 4], ecx
	mov [gs:ebp + 8], eax
	mov [gs:ebp + 12], edx

	mov edi, eax
	mov eax, [fs:ebx + Var.Target_queue]
	mov [gs:ebp], eax
	add [gs:0], dword 20
	invoke ISystem.Send_Message

	test eax, eax
	jnz .Resend
	mov eax, edi
	jmp .Set_flag

	.Resend:
	invoke IThread.Yield
	mov eax, edi
	jmp .Send_message

	.Set_flag:
	bt dword [fs:ebx + Var.Flag], Release_bit
	jc .Release_state

	.Press_state:
		cmp al, $F0
		je .F0
		cmp ecx, VK_LSHIFT
		je .Press_shift
		cmp ecx, VK_RSHIFT
		je .Press_shift
		jmp Processing_thread

		.F0:
		bts dword [fs:ebx + Var.Flag], Release_bit
		jmp Processing_thread
		.Press_shift:
		bts dword [fs:ebx + Var.Flag], Shift_bit
		jmp Processing_thread

	.Release_state:
		btr dword [fs:ebx + Var.Flag], Release_bit
		cmp ecx, VK_LSHIFT
		je .Release_shift
		cmp ecx, VK_RSHIFT
		je .Release_shift
		jmp Processing_thread

		.Release_shift:
		btr dword [fs:ebx + Var.Flag], Shift_bit
		jmp Processing_thread

	.Buffer_is_empty:
	invoke IThread.Yield
	jmp Processing_thread

Const:
	Release_bit = 0
	Shift_bit = 1
	Extended_bit = 2

Var:
	.Flag dd 0
	.Target_queue dd 0
Keyboard_Buffer db (1024+13) dup 0

US_Layout:
	._0 dw 0
	._1 dw VK_F9
	._2 dw 0
	._3 dw VK_F5
	._4 dw VK_F3
	._5 dw VK_F1
	._6 dw VK_F2
	._7 dw VK_F12
	._8 dw 0
	._9 dw VK_F10
	._A dw VK_F8
	._B dw VK_F6
	._C dw VK_F4
	._D dw $09 ; Tab
	._E dw '`' ; `
	._F dw 0

	._10 dw 0
	._11 dw VK_LALT
	._12 dw VK_LSHIFT
	._13 dw 0
	._14 dw VK_LCTRL
	._15 dw 'q' ; q
	._16 dw '1' ; 1
	._17 dw 0
	._18 dw 0
	._19 dw 0
	._1A dw 'z' ; z
	._1B dw 's' ; s
	._1C dw 'a' ; a
	._1D dw 'w' ; w
	._1E dw '2' ; 2
	._1F dw 0

	._20 dw 0
	._21 dw 'c' ; c
	._22 dw 'x' ; x
	._23 dw 'd' ; d
	._24 dw 'e' ; e
	._25 dw '4' ; 4
	._26 dw '3' ; 3
	._27 dw 0
	._28 dw 0
	._29 dw ' ' ; Space
	._2A dw 'v' ; v
	._2B dw 'f' ; f
	._2C dw 't' ; t
	._2D dw 'r' ; r
	._2E dw '5' ; 5
	._2F dw 0

	._30 dw 0
	._31 dw 'n' ; n
	._32 dw 'b' ; b
	._33 dw 'h' ; h
	._34 dw 'g' ; g
	._35 dw 'y' ; y
	._36 dw '6' ; 6
	._37 dw 0
	._38 dw 0
	._39 dw 0
	._3A dw 'm' ; m
	._3B dw 'j' ; j
	._3C dw 'u' ; u
	._3D dw '7' ; 7
	._3E dw '8' ; 8
	._3F dw 0

	._40 dw 0
	._41 dw ',' ; ,
	._42 dw 'k' ; k
	._43 dw 'i' ; i
	._44 dw 'o' ; o
	._45 dw '0' ; 0
	._46 dw '9' ; 9
	._47 dw 0
	._48 dw 0
	._49 dw '.' ; .
	._4A dw '/' ; /
	._4B dw 'l' ; l
	._4C dw  ';' ; ;
	._4D dw 'p' ; p
	._4E dw '-' ; -
	._4F dw 0

	._50 dw 0
	._51 dw 0
	._52 dw '''' ; '
	._53 dw 0
	._54 dw '[' ; [
	._55 dw '=' ; =
	._56 dw 0
	._57 dw 0
	._58 dw 0
	._59 dw VK_RSHIFT ; Right Shift
	._5A dw $0D ; Enter
	._5B dw ']' ; ]
	._5C dw 0
	._5D dw '\' ; \
	._5E dw 0
	._5F dw 0

	._60 dw 0
	._61 dw 0
	._62 dw 0
	._63 dw 0
	._64 dw 0
	._65 dw 0
	._66 dw $08 ; Backspace
	._67 dw 0
	._68 dw 0
	._69 dw 0
	._6A dw 0
	._6B dw 0
	._6C dw 0
	._6D dw 0
	._6E dw 0
	._6F dw 0

	repeat $90
	dw 0
	end repeat

Scancodes_Table:
	._0 db 0
	._1 db 0 ; F9
	._2 db 0
	._3 db 0 ; F5
	._4 db 0 ; F3
	._5 db 0 ; F1
	._6 db 0 ; F2
	._7 db 0 ; F12
	._8 db 0
	._9 db 0 ; F10
	._A db 0 ; F8
	._B db 0 ; F6
	._C db 0 ; F4
	._D db 0 ; Tab
	._E db '`' ; `
	._F db 0

	._10 db 0
	._11 db 0 ; Alt
	._12 db 0 ; Shift
	._13 db 0
	._14 db 0 ; Ctrl
	._15 db 'q' ; q
	._16 db '1' ; 1
	._17 db 0
	._18 db 0
	._19 db 0
	._1A db 'z' ; z
	._1B db 's' ; s
	._1C db 'a' ; a
	._1D db 'w' ; w
	._1E db '2' ; 2
	._1F db 0

	._20 db 0
	._21 db 'c' ; c
	._22 db 'x' ; x
	._23 db 'd' ; d
	._24 db 'e' ; e
	._25 db '4' ; 4
	._26 db '3' ; 3
	._27 db 0
	._28 db 0
	._29 db ' ' ; Space
	._2A db 'v' ; v
	._2B db 'f' ; f
	._2C db 't' ; t
	._2D db 'r' ; r
	._2E db '5' ; 5
	._2F db 0

	._30 db 0
	._31 db 'n' ; n
	._32 db 'b' ; b
	._33 db 'h' ; h
	._34 db 'g' ; g
	._35 db 'y' ; y
	._36 db '6' ; 6
	._37 db 0
	._38 db 0
	._39 db 0
	._3A db 'm' ; m
	._3B db 'j' ; j
	._3C db 'u' ; u
	._3D db '7' ; 7
	._3E db '8' ; 8
	._3F db 0

	._40 db 0
	._41 db ',' ; ,
	._42 db 'k' ; k
	._43 db 'i' ; i
	._44 db 'o' ; o
	._45 db '0' ; 0
	._46 db '9' ; 9
	._47 db 0
	._48 db 0
	._49 db '.' ; .
	._4A db '/' ; /
	._4B db 'l' ; l
	._4C db ';' ; ;
	._4D db 'p' ; p
	._4E db '-' ; -
	._4F db 0

	._50 db 0
	._51 db 0
	._52 db '''' ; '
	._53 db 0
	._54 db '[' ; [
	._55 db '=' ; =
	._56 db 0
	._57 db 0
	._58 db 0
	._59 db 0 ; Right Shift
	._5A db 13 ; Enter
	._5B db ']' ; ]
	._5C db 0
	._5D db '\' ; \
	._5E db 0
	._5F db 0

	._60 db 0
	._61 db 0
	._62 db 0
	._63 db 0
	._64 db 0
	._65 db 0
	._66 db 8 ; Backspace
	._67 db 0
	._68 db 0
	._69 db 0
	._6A db 0
	._6B db 0
	._6C db 0
	._6D db 0
	._6E db 0
	._6F db 0

	repeat $90
	db 0
	end repeat

Shift_Scancodes_Table:
	._0 db 0
	._1 db 0 ; F9
	._2 db 0
	._3 db 0 ; F5
	._4 db 0 ; F3
	._5 db 0 ; F1
	._6 db 0 ; F2
	._7 db 0 ; F12
	._8 db 0
	._9 db 0 ; F10
	._A db 0 ; F8
	._B db 0 ; F6
	._C db 0 ; F4
	._D db 0 ; Tab
	._E db '~' ; `
	._F db 0

	._10 db 0
	._11 db 0 ; Alt
	._12 db 0 ; Shift
	._13 db 0
	._14 db 0 ; Ctrl
	._15 db 'Q' ; q
	._16 db '!' ; 1
	._17 db 0
	._18 db 0
	._19 db 0
	._1A db 'Z' ; z
	._1B db 'S' ; s
	._1C db 'A' ; a
	._1D db 'W' ; w
	._1E db '@' ; 2
	._1F db 0

	._20 db 0
	._21 db 'C' ; c
	._22 db 'X' ; x
	._23 db 'D' ; d
	._24 db 'E' ; e
	._25 db '$' ; 4
	._26 db '#' ; 3
	._27 db 0
	._28 db 0
	._29 db ' ' ; Space
	._2A db 'V' ; v
	._2B db 'F' ; f
	._2C db 'T' ; t
	._2D db 'R' ; r
	._2E db '%' ; 5
	._2F db 0

	._30 db 0
	._31 db 'N' ; n
	._32 db 'B' ; b
	._33 db 'H' ; h
	._34 db 'G' ; g
	._35 db 'Y' ; y
	._36 db '^' ; 6
	._37 db 0
	._38 db 0
	._39 db 0
	._3A db 'M' ; m
	._3B db 'J' ; j
	._3C db 'U' ; u
	._3D db '&' ; 7
	._3E db '*' ; 8
	._3F db 0

	._40 db 0
	._41 db '<' ; ,
	._42 db 'K' ; k
	._43 db 'I' ; i
	._44 db 'O' ; o
	._45 db ')' ; 0
	._46 db '(' ; 9
	._47 db 0
	._48 db 0
	._49 db '>' ; .
	._4A db '?' ; /
	._4B db 'L' ; l
	._4C db ':' ; ;
	._4D db 'P' ; p
	._4E db '_' ; -
	._4F db 0

	._50 db 0
	._51 db 0
	._52 db '"' ; '
	._53 db 0
	._54 db '{' ; [
	._55 db '+' ; =
	._56 db 0
	._57 db 0
	._58 db 0
	._59 db 0 ; Right Shift
	._5A db 13 ; Enter
	._5B db '}' ; ]
	._5C db 0
	._5D db '|' ; \
	._5E db 0
	._5F db 0

	._60 db 0
	._61 db 0
	._62 db 0
	._63 db 0
	._64 db 0
	._65 db 0
	._66 db 8 ; Backspace
	._67 db 0
	._68 db 0
	._69 db 0
	._6A db 0
	._6B db 0
	._6C db 0
	._6D db 0
	._6E db 0
	._6F db 0

	repeat $90
	db 0
	end repeat