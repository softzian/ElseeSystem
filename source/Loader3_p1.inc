Function_Cardinal_to_HexStr:
	.Num equ word [bp+10]	; Num : Cardinal16
	.HexStr equ dword [bp+6]  ; var HexStr : String (Far)
	.Count equ word [bp+4]	  ; Count : Cardinal16

	push bp
	mov bp, sp
	push bx
	push cx
	push dx
	push es
	push di

	cmp .Count, 0
	je .Error1

	cmp .Num, 0
	je .Zero_case

	mov dx, .Num
	or ch, True
	xor bx, bx
	les di, .HexStr

	mov cl, 3
	.Loop:
	mov ax, dx
	shl cl, 2
	shr ax, cl
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

	inc bx
	cmp bx, .Count
	ja .Error2

	mov [es:di+2+bx-1], al

	.Continue_loop:
	dec cl
	jns .Loop

	mov [es:di], bx

	xor ax, ax

	.Return:
	pop di
	pop es
	pop dx
	pop cx
	pop bx
	leave
	ret 8

	.Error1:
	mov ax, -1	; Count = 0, exit
	jmp .Return

	.Zero_case:
	les di, .HexStr
	mov word [es:di], 1
	mov byte [es:di+2], '0' - 0
	xor ax, ax
	jmp .Return

	.Error2:
	mov [es:di], word 0
	mov ax, -2 ; Count is not large enough, exit
	jmp .Return

	restore .Num
	restore .HexStr
	restore .Count

Function_Write_Cardinal_Hex:
	.Num equ word [bp+4]
	; Local Var:
	.Str equ [bp-6] ; Str : String[4]

	push bp
	mov bp, sp
	sub sp, 6

	mov ax, .Num
	test ax, ax
	je .Zero_case

	push ax
	push ss
	lea ax, .Str
	push ax
	push word 4
	call Function_Cardinal_to_HexStr

	push ss
	lea ax, .Str
	push ax
	call Function_Write_String

	xor ax, ax

	.Return:
	leave
	ret 2

	.Zero_case:
	push '0'
	call Function_Write_Char
	jmp .Return

	restore .Num
	restore .Str

Function_Write_Char:
	.Value equ byte [bp+4]

	push bp
	mov bp, sp

	push ss
	lea ax, .Value
	push ax
	push word 1
	call Function_Write_Telex

	leave
	ret 2
	restore .Value

Function_Write_String:
	.Str equ dword [bp+4]	; var Str : String (Far)

	push bp
	mov bp, sp
	push es
	push di

	les di, .Str
	add di, 2
	push es
	push di
	push word [es:di-2]
	call Function_Write_Telex

	pop di
	pop es
	leave
	ret 4
	restore .Str

Function_Write_Telex:
	.Text equ dword [bp+6]	; var Text : Array of Char (Far)
	.Count equ word [bp+4]	; Count : Cardinal16
	; Local var
	.Cursor equ word [bp-2] ; Cursor : Cardinal16

	push bp
	mov bp, sp
	sub sp, 2

	push cx
	push ds
	push si
	push es
	push di

	xor ax, ax
	mov ds, ax

	mov ax, .Count
	cmp ax, 0
	je .Error1
	cmp ax, 2000
	ja .Error1

	add ax, [Var_p1.Cursor]
	cmp ax, 2000
	jb .Write

	mov word [Var_p1.Cursor], 0    ; Reset cursor position if the remaining display space is not enough

	.Write:
	mov ax, [Var_p1.Cursor]
	mov .Cursor, ax
	lds si, .Text

	mov ax, $B800
	mov es, ax
	xor di, di
	mov ax, .Cursor
	shl ax, 1
	add di, ax    ; ES:DI points to current Cursor position in Video memory

	mov cx, .Count
	.Copy_Text_to_Video_mem:
	mov al, [ds:si]
	mov byte [es:di+1], 00001111b
	mov [es:di], al
	inc si
	add di, 2
	loop .Copy_Text_to_Video_mem

	.Update_Cursor_position:
	xor ax, ax
	mov ds, ax
	mov ax, .Count
	add [Var_p1.Cursor], ax
	xor ax, ax

	.Return:
	pop di
	pop es
	pop si
	pop ds
	pop cx
	leave
	ret 6
	.Error1:
	mov ax, -1     ; Count is invalid, exit
	jmp .Return

	restore .Count
	restore .Text
	restore .Cursor

Print_Success:
	mov ax, $B800
	mov ds, ax
	mov byte [0], 'S'
	mov byte [1], 00001010b
	mov byte [2], 'u'
	mov byte [3], 00001010b
	mov byte [4], 'c'
	mov byte [5], 00001010b
	mov byte [6], 'c'
	mov byte [7], 00001010b
	mov byte [8], 'e'
	mov byte [9], 00001010b
	mov byte [10], 's'
	mov byte [11], 00001010b
	mov byte [12], 's'
	mov byte [13], 00001010b
	ret

Print_Failure:
	mov ax, $B800
	mov ds, ax
	mov byte [0], 'F'
	mov byte [1], 00001100b
	mov byte [2], 'a'
	mov byte [3], 00001100b
	mov byte [4], 'i'
	mov byte [5], 00001100b
	mov byte [6], 'l'
	mov byte [7], 00001100b
	mov byte [8], 'u'
	mov byte [9], 00001100b
	mov byte [10], 'r'
	mov byte [11], 00001100b
	mov byte [12], 'e'
	mov byte [13], 00001100b
	ret

Var_p1:
	.Cursor dw 0