; VGA.asm - IVideo Standard VGA driver module
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

IVideo = $10000C
; The first three functions are for debug purpose
; Function 1: Write_Telex (var Text : Array of Char; Count : Word)
; Function 2: Clear_Screen
; Function 3: New_Line

; Function 4: Alloc_context (Type : Cardinal) : Handle
; Function 5: Create_compatible_context (Context : Handle) : Handle
; Function 6: Dealloc_context (Context : Handle)

; Function 7: Lock_context (Context : Handle)
; Function 8: Release_context (Context : Handle)

; Function 9: Switch_context (Context : Handle)

; Function 10: Blit (Src : Context; Src_X, Src_Y : Cardinal; Dst : Context; Dst_X, Dst_Y, Width, Height : Cardinal)

; Text mode context functions
; Function 11: Write_text_char (Context : Handle; Char : Ansi_char; Attribute : Byte; X, Y : Byte)
; Function 12: Write_text_line (Context : Handle; var Src : Array of Ansi_char; Count : Card16; Attribute, X, Y : Byte)
; Function 13: Clear_text_screen (Context : Handle)
; Function 14: Set_text_cursor (Context : Handle; X, Y : Byte)

jmp near dword Function_Init
dd Header
Interface:
	dd Function_Write_Telex
	dd Function_Clear_Screen
	dd Function_New_Line
	dd Function_Write_Telex2

	dd Function_Alloc_context
	dd 0
	dd 0

	dd Function_Lock_context
	dd Function_Release_context

	dd Function_Switch_context

	dd 0

	dd Function_Write_text_char
	dd Function_Write_text_line
	dd Function_Clear_text_screen
	dd Function_Set_text_cursor

Header:

IVideo_Error_code:
	INVALID_COUNT = -1
	INVALID_CURSOR = -2
	VIDEO_BUFFER_OVERFLOW = -3
	CANNOT_CREATE_HANDLE = -4
	ALLOCATE_MEMORY_PROBLEM = -6
	CANNOT_SWITCH_MEMORY_CONTEXT = -7
	INVALID_CONTEXT_HANDLE = -8
	NOT_TEXT_MODE_CONTEXT = -9
	UNSUPPORT_CONTEXT_TYPE = -10

Const:
	Write_Flag = 0

	; Context record
	Context_type = 0
	Context_lock = 4

	; Text mode context record (extends from Context)
	Text_mode_context_width = 8
	Text_mode_context_height = 9
	Text_mode_context_size = 10
	Text_mode_context_cursor = 12
	Text_mode_context_buffer = 14

	; Context type
	VGA_TEXT_MODE_CONTEXT = 1

	Allocator1 equ [ds:$10]
	Allocator1.Init_allocator = 0
	Allocator1.Allocate = 4
	Allocator1.Deallocate = 8

Function_Init:
	push ebx
	push edi
	push esi

	mov ebx, eax
	lea esi, [eax + Interface]
	mov [fs:IVideo], eax

	xor eax, eax
	.Loop:
		add [fs:esi + eax], ebx
		add eax, 4
		cmp eax, 4 * 14
		jb .Loop

	; Register module
	mov [gs:ebp], dword 3
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], ebx
	invoke ISystem, ISystem.Register_Module

	test eax, eax
	jnz .Error1

	; Allocate address space
	;mov [gs:ebp], dword $10000
	;invoke ISystem, ISystem.Allocate

	;mov esi, [ss:_Result]

	;mov [gs:ebp], esi
	;mov [gs:ebp + 4], dword $F
	;invoke ISystem, ISystem.Add_address_space

	;test eax, eax
	;jnz .Error2

	;push dword [ss:_Result]
	;invoke ISystem, ISystem.Set_DS_space

	;mov [gs:ebp], dword 1
	;mov [gs:ebp + 4], dword 0
	;mov [gs:ebp + 8], dword 1
	;mov [gs:ebp + 12], dword 0
	;invoke ISystem, ISystem.Find_module

	;test eax, eax
	;jnz .Error3

	;mov edi, [ss:_Result]
	;mov Allocator1, edi
	;mov [gs:ebp], dword $1000
	;mov [gs:ebp + 4], dword $FFFF
	;invoke2 edi, Allocator1.Init_allocator

	;invoke ISystem, ISystem.Set_DS_space
	;add esp, 4

	xor eax, eax

	.Return:
	pop esi
	pop edi
	pop ebx
	ret

	.Error1:
	mov eax, 1
	jmp .Return

	.Error2:
	mov eax, 2
	jmp .Return

	.Error3:
	mov eax, 3
	jmp .Return

Function_Write_Telex:	; Function 1
	.Text equ dword [gs:ebp - 6]
	.Count equ word [gs:ebp - 2]

	push ebp
	add ebp, 6

	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, [fs:IVideo]

	mov ax, .Count
	cmp ax, 0
	je .Error1
	cmp ax, 2000
	ja .Error1
	add ax, [fs:ebx + Var.Cursor]
	cmp ax, 2000
	jb .Write

	mov word [fs:ebx + Var.Cursor], 0    ; Reset cursor position if the remaining display space is not enough

	.Write:
	mov esi, .Text

	mov edi, $B8000
	xor eax, eax
	mov ax, [fs:ebx + Var.Cursor]
	shl eax, 1
	add edi, eax	; EDI points to current Cursor position in Video memory

	xor ecx, ecx
	mov cx, .Count
	.Copy_Text_to_Video_mem:
	mov al, [esi]
	mov byte [fs:edi + 1], 00001111b
	mov [fs:edi], al
	inc esi
	add edi, 2
	loop .Copy_Text_to_Video_mem

	.Update_Cursor_position:
	mov ax, .Count
	add [fs:ebx + Var.Cursor], ax
	xor eax, eax

	.Set_Flag:
	bts word [fs:ebx + Var.Flag], Write_Flag

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_COUNT
	jmp .Return

	restore .Count
	restore .Text

Function_Clear_Screen:	; Function 2
	push ebx
	push edi
	push ecx

	.Clear_Screen:
	xor eax, eax
	mov edi, $B8000
	mov ecx, 1000

	.Loop:
		mov [fs:edi], eax
		add edi, 4
		loop .Loop

	.Clear_Flag:
	mov ebx, [fs:IVideo]
	mov word [fs:ebx + Var.Flag], 0

	pop ecx
	pop edi
	pop ebx
	ret

Function_New_Line:	; Function 3
	push ebx

	mov ebx, [fs:IVideo]

	mov ax, [fs:ebx + Var.Cursor]
	mov bl, 80
	div bl

	mov ebx, [fs:IVideo]
	cmp ah, 0
	jne .j1

	bt word [fs:ebx + Var.Flag], Write_Flag
	jc .Return

	.j1:
	neg ah
	add ah, 80
	shr ax, 8
	add ax, [fs:ebx + Var.Cursor]

	cmp ax, 2000
	jb .j2
	xor ax, ax
	.j2: mov [fs:ebx + Var.Cursor], ax

	.Return:
	btr word [fs:ebx + Var.Flag], Write_Flag
	pop ebx
	ret

Function_Write_Telex2:	 ; Function 4
	.Text equ dword [gs:ebp - 6]
	.Count equ word [gs:ebp - 2]

	push ebp
	add ebp, 6

	push ebx
	push ecx
	push edx
	push esi
	push edi

	mov ebx, [fs:IVideo]

	mov ax, .Count
	cmp ax, 0
	je .Error1
	cmp ax, 2000
	ja .Error1
	add ax, [fs:ebx + Var.Cursor]
	cmp ax, 2000
	jb .Write

	mov word [fs:ebx + Var.Cursor], 0    ; Reset cursor position if the remaining display space is not enough

	.Write:
	mov esi, .Text

	mov edi, $B8000
	xor eax, eax
	mov ax, [fs:ebx + Var.Cursor]
	shl eax, 1
	add edi, eax	; EDI points to current Cursor position in Video memory

	xor ecx, ecx
	mov cx, .Count
	.Copy_Text_to_Video_mem:
	mov al, [fs:esi]
	mov byte [fs:edi + 1], 00001111b
	mov [fs:edi], al
	inc esi
	add edi, 2
	loop .Copy_Text_to_Video_mem

	.Update_Cursor_position:
	mov ax, .Count
	add [fs:ebx + Var.Cursor], ax
	xor eax, eax

	.Set_Flag:
	bts word [fs:ebx + Var.Flag], Write_Flag

	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_COUNT
	jmp .Return

	restore .Count
	restore .Text

Function_Alloc_context: 	; Function 4
	.Type equ dword [gs:ebp - 4] ; Type : Cardinal

	push ebp
	add ebp, 4

	push ebx

	mov eax, .Type
	cmp eax, VGA_TEXT_MODE_CONTEXT
	je .VGA_text_mode_context
	jmp .Error1

	.VGA_text_mode_context:
	call Create_VGA_text_mode_context

	.Return:
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, UNSUPPORT_CONTEXT_TYPE
	jmp .Return

	restore .Type

Create_VGA_text_mode_context:
	; Result in EBX
	push ecx
	push edx
	push esi

	mov esi, [fs:IVideo]

	push esi
	invoke ISystem, ISystem.Set_DS_space

	; Allocate context record
	mov [gs:ebp], dword (Text_mode_context_buffer + 2000 * 2)
	invoke3 Allocator1, Allocator1.Allocate

	test eax, eax
	jnz .Error1

	mov ebx, [ss:_Result]

	mov eax, VGA_TEXT_MODE_CONTEXT
	bts eax, 31
	mov [ds:ebx + Context_type], eax
	mov [ds:ebx + Context_lock], dword 0

	mov [ds:ebx + Text_mode_context_width], byte 80
	mov [ds:ebx + Text_mode_context_height], byte 25
	mov [ds:ebx + Text_mode_context_size], word 2000

	mov ecx, 2000
	.Loop:
		mov [ds:ebx + Text_mode_context_buffer + ecx * 2 - 2], word $0F00
		loop .Loop

	mov [ss:_Result], ebx
	xor eax, eax

	.Return:
	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	pop esi
	pop edx
	pop ecx
	ret

	.Error1:
	mov eax, ALLOCATE_MEMORY_PROBLEM
	jmp .Return

Lock_context:
	lock bts dword [ds:eax + Context_lock], 0
	jc .Wait
	ret
	.Wait: invoke IThread, IThread.Yield
	jmp Lock_context

Function_Lock_context:		; Function 7
	.Context equ dword [gs:ebp - 4]

	push ebp
	add ebp, 4

	push dword [fs:IVideo]
	invoke ISystem, ISystem.Set_DS_space

	mov eax, .Context
	call Lock_context

	.Return:
	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	pop ebp
	ret

	restore .Context

Function_Release_context:	; Function 8
	.Context equ dword [gs:ebp - 4]

	push ebp
	add ebp, 4

	push dword [fs:IVideo]
	invoke ISystem, ISystem.Set_DS_space

	mov eax, .Context
	btr dword [ds:eax + Context_lock], 0

	.Return:
	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	pop ebp
	ret

	restore .Context

Function_Switch_context:	; Function 9
	.Context equ dword [gs:ebp - 4] ; Context : Handle

	push ebp
	add ebp, 4

	push ecx
	push edx
	push esi

	mov esi, [fs:IVideo]

	push esi
	invoke ISystem, ISystem.Set_DS_space

	mov ecx, .Context
	mov edx, [fs:esi + Var.Active_context]
	cmp edx, ecx
	je .Do_nothing

	mov eax, ecx
	call Lock_context

	bt dword [ds:ecx + Context_type], 31
	jnc .Error3

	test edx, edx
	jz .Active_console_invalid

	mov eax, edx
	call Lock_context

	call Restore_context
	mov [fs:esi + Var.Active_context], ecx

	.Release1:
	btr dword [ds:edx + Context_lock], 0

	.Release2:
	btr dword [ds:ecx + Context_lock], 0

	.Return:
	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	mov eax, esi

	pop esi
	pop edx
	pop ecx

	pop ebp
	ret

	.Error3:
	mov esi, CANNOT_SWITCH_MEMORY_CONTEXT
	jmp .Release2

	.Do_nothing:
	xor esi, esi
	jmp .Return

	.Active_console_invalid:
	call Restore_context
	mov [fs:esi + Var.Active_context], ecx
	xor esi, esi
	jmp .Release2

	restore .Context

Restore_context:
	mov eax, [ds:ecx + Context_type]
	btr eax, 31
	cmp eax, VGA_TEXT_MODE_CONTEXT
	je Restore_text_mode_context
	ret

Restore_text_mode_context:
	push ebx
	push ecx
	push edx

	movzx edx, word [ds:ecx + Text_mode_context_size]
	xor ebx, ebx
	.Loop:
		mov ax, [ds:ecx + Text_mode_context_buffer + ebx * 2]
		mov [fs:$B8000 + ebx * 2], ax
		inc ebx
		cmp ebx, edx
		jb .Loop

	mov ax, [ds:ecx + Text_mode_context_cursor]
	call Set_VGA_text_cursor

	pop edx
	pop ecx
	pop ebx
	ret

Function_Write_text_char:
	.Context equ dword [gs:ebp - 8] ; Context : Handle
	.Char equ byte [gs:ebp - 4] ; Char : Ansi_char
	.Attribute equ byte [gs:ebp - 3] ; Attribute : Byte
	.X equ byte [gs:ebp - 2] ; X : Byte
	.Y equ byte [gs:ebp - 1] ; Y : Byte

	push ebp
	add ebp, 8

	push ecx
	push edx
	push esi

	mov esi, [fs:IVideo]
	push esi
	invoke ISystem, ISystem.Set_DS_space

	mov ecx, .Context
	mov eax, [ds:ecx + Context_type]
	btr eax, 31
	cmp eax, VGA_TEXT_MODE_CONTEXT
	jne .Error2

	mov al, .Y
	mov dl, .X
	call Check_and_calculate_coordinate
	bt eax, 31
	jc .Error3

	movzx edx, ax
	mov al, .Char
	mov ah, .Attribute
	mov [ds:ecx + Text_mode_context_buffer + edx * 2], ax

	cmp ecx, [fs:esi + Var.Active_context]
	jne .Finish

	mov [fs:$B8000 + edx * 2], ax

	.Finish:
	xor eax, eax

	.Return:
	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	pop esi
	pop edx
	pop ecx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_CONTEXT_HANDLE
	jmp .Return
	.Error2:
	mov eax, NOT_TEXT_MODE_CONTEXT
	jmp .Return
	.Error3:
	mov eax, INVALID_CURSOR
	jmp .Return

	restore .Context
	restore .Char
	restore .Attribute
	restore .X
	restore .Y

Check_and_calculate_coordinate:
	cmp al, [ds:ecx + Text_mode_context_height]
	jae .Error

	mov dh, [ds:ecx + Text_mode_context_width]
	cmp dl, dh
	jae .Error

	mul dh
	add al, dl
	adc ah, 0

	btr eax, 31
	ret

	.Error:
	bts eax, 31
	ret

Function_Write_text_line:
	.Context equ dword [gs:ebp - 13] ; Context : Handle
	.Src equ dword [gs:ebp - 9] ; var Src : Array of Ansi_char
	.Count equ word [gs:ebp - 5] ; Count : Card16
	.Attribute equ byte [gs:ebp - 3] ; Attribute : Byte
	.X equ byte [gs:ebp - 2] ; X : Byte
	.Y equ byte [gs:ebp - 1] ; Y : Byte

	push ebp
	add ebp, 13

	push ebx
	push ecx
	push edx
	push esi
	push edi

	push dword [fs:IVideo]
	invoke ISystem, ISystem.Set_ES_space

	movzx edi, .Count
	test edi, edi
	jz .Error4

	mov esi, .Context
	mov eax, [es:esi + Context_type]
	btr eax, 31
	cmp eax, VGA_TEXT_MODE_CONTEXT
	jne .Error2

	mov al, .Y
	mov dl, .X
	call Check_and_calculate_coordinate
	bt eax, 31
	jc .Error3

	and eax, $0000FFFF
	push eax
	mov edx, eax
	add eax, edi
	cmp ax, [es:esi + Text_mode_context_size]
	ja .Error4

	mov ebx, .Src
	mov ah, .Attribute
	add esi, Text_mode_context_buffer
	add esi, edx
	xor edx, edx
	.Loop1:
		mov al, [ds:ebx + edx]
		mov [es:esi + edx * 2], ax
		inc edx
		cmp edx, edi
		jb .Loop1

	mov esi, [fs:IVideo]
	cmp ecx, [fs:esi + Var.Active_context]
	jne .Finish

	xor edx, edx
	pop esi
	shl esi, 1
	add esi, $B8000
	.Loop2:
		mov al, [ds:ebx + edx]
		mov [fs:esi + edx * 2], ax
		inc edx
		cmp edx, edi
		jb .Loop2

	.Finish:
	xor eax, eax

	.Return:
	invoke ISystem, ISystem.Set_ES_space
	add esp, 4

	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	.Error1:
	mov eax, INVALID_CONTEXT_HANDLE
	jmp .Return
	.Error2:
	mov eax, NOT_TEXT_MODE_CONTEXT
	jmp .Return
	.Error3:
	mov eax, INVALID_CURSOR
	jmp .Return
	.Error4:
	mov eax, INVALID_COUNT
	jmp .Return

	restore .Context
	restore .Src
	restore .Count
	restore .Attribute
	restore .X
	restore .Y

Function_Clear_text_screen:	; Function 13
	.Context equ dword [gs:ebp - 4] ; Context : Handle

	push ebp
	add ebp, 4

	push ecx
	push edx
	push esi

	mov esi, [fs:IVideo]
	push esi
	invoke ISystem, ISystem.Set_DS_space

	mov ecx, .Context

	mov eax, [ds:ecx + Context_type]
	btr eax, 31
	cmp eax, VGA_TEXT_MODE_CONTEXT
	jne .Error2

	movzx eax, word [ds:ecx + Text_mode_context_width]
	mul ah
	mov edx, eax

	xor eax, eax
	.Loop1:
	mov [ds:ecx + Text_mode_context_buffer + eax * 2], word $0F00
	inc eax
	cmp eax, edx
	jb .Loop1

	cmp ecx, [fs:esi + Var.Active_context]
	jne .Finish

	.Loop2:
	mov [fs:$B8000 + eax * 2 - 2], word $0F00
	dec eax
	test eax, eax
	jnz .Loop2

	.Finish:
	xor eax, eax

	.Return:
	invoke ISystem, ISystem.Set_DS_space

	pop esi
	pop edx
	pop ecx

	pop ebp
	ret

	.Error2:
	mov eax, NOT_TEXT_MODE_CONTEXT
	jmp .Return

	restore .Context

Set_VGA_text_cursor:
	; AX : Cursor position (linear)
	push ecx
	push edx

	mov cx, ax
	mov dx, $3D4
	in al, dx
	push eax
	mov al, $E
	out dx, al
	inc dl
	mov al, ch
	out dx, al
	dec dl
	mov al, $F
	out dx, al
	inc dl
	mov al, cl
	out dx, al
	dec dl
	pop eax
	out dx, al

	pop edx
	pop ecx
	ret

Function_Set_text_cursor:	; Function 14
	.Context equ dword [gs:ebp - 6] ; Context : Handle
	.X equ byte [gs:ebp - 2] ; X : Byte
	.Y equ byte [gs:ebp - 1] ; Y : Byte

	push ebp
	add ebp, 6

	push ecx
	push edx
	push esi

	mov esi, [fs:IVideo]
	push esi
	invoke ISystem, ISystem.Set_DS_space

	mov ecx, .Context

	mov eax, [ds:ecx + Context_type]
	btr eax, 31
	cmp eax, VGA_TEXT_MODE_CONTEXT
	jne .Error2

	mov al, .Y
	mov dl, .X
	call Check_and_calculate_coordinate
	bt eax, 31
	jc .Error3

	mov [ds:ecx + Text_mode_context_cursor], ax
	cmp ecx, [fs:esi + Var.Active_context]
	jne .Finish

	call Set_VGA_text_cursor

	.Finish:
	xor eax, eax

	.Return:
	invoke ISystem, ISystem.Set_DS_space
	add esp, 4

	pop esi
	pop edx
	pop ecx

	pop ebp
	ret

	.Error2:
	mov eax, NOT_TEXT_MODE_CONTEXT
	jmp .Return
	.Error3:
	mov eax, INVALID_CURSOR
	jmp .Return

	restore .Context
	restore .X
	restore .Y

Var:
	.Cursor dw 0
	.Flag dw 0
	.Active_context dd 0
	.Allocator1 dd 0