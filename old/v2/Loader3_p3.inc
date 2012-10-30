Init_base_system:
	; ------------------ ;
	; Init System module ;
	; ------------------ ;
	mov eax, $10000 	; System module
	call eax

	call Init_code_table

	push $7000
	invoke ISystem.Set_Active_Module

	call Init_Memory_Table

	; ------------------ ;
	; Init other modules ;
	; ------------------ ;
	push $13000
	invoke ISystem.Set_Active_Module
	mov eax, $13000 	; Interrupt
	call eax

	push $14000
	invoke ISystem.Set_Active_Module
	mov eax, $14000 	; Thread
	call eax

	push $15000
	invoke ISystem.Set_Active_Module
	mov eax, $15000 	; Keyboard
	call eax

	push $11000
	invoke ISystem.Set_Active_Module
	mov eax, $11000 	; Video
	call eax

	push $12000
	invoke ISystem.Set_Active_Module
	mov eax, $12000 	; Utility
	call eax

	push $16000
	invoke ISystem.Set_Active_Module
	mov eax, $16000 	; Console
	call eax

	push $17000
	invoke ISystem.Set_Active_Module
	mov eax, $17000 	; Conversion
	call eax

	push $7000
	invoke ISystem.Set_Active_Module

	; ------------------------------- ;
	; Create console and start thread ;
	; ------------------------------- ;
	cli

	sub esp, 4
	mov eax, esp
	push eax
	invoke IConsole.Alloc_console
	pop eax

	mov [fs:Var.Console], eax

	push dword [fs:Var.Console]
	invoke IConsole.Switch_console

	; Enable IRQ0, start threads
	mov [esp - 1], byte 0
	dec esp
	invoke IInterrupt.Enable_IRQ

	sti

	ret

Init_code_table:
	push $1FFFFF
	invoke ISystem.Create_Code_Region

	push $7000
	push $8FFF
	push 1
	invoke ISystem.Mark_Code

	push $10000
	push $10FFF
	push 1
	invoke ISystem.Mark_Code

	push $11000
	push $11FFF
	push 1
	invoke ISystem.Mark_Code

	push $12000
	push $12FFF
	push 1
	invoke ISystem.Mark_Code

	push $13000
	push $13FFF
	push 1
	invoke ISystem.Mark_Code

	push $14000
	push $14FFF
	push 1
	invoke ISystem.Mark_Code

	push $15000
	push $15FFF
	push 1
	invoke ISystem.Mark_Code

	push $16000
	push $16FFF
	push 1
	invoke ISystem.Mark_Code

	push $17000
	push $17FFF
	push 1
	invoke ISystem.Mark_Code

	ret

Print_Code_Table:
	xor ecx, ecx

	.Loop:
	push dword ecx
	call dword [cs:IUtility.Write_Cardinal_Hex]
	mov [esp - 1], byte ' '
	dec esp
	call dword [cs:IUtility.Write_Char]

	push dword [cs:4 + ecx]
	call dword [cs:IUtility.Write_Cardinal_Hex]
	mov [esp - 1], byte ' '
	dec esp
	call dword [cs:IUtility.Write_Char]

	push dword [cs:4 + ecx + 4]
	call dword [cs:IUtility.Write_Cardinal_Hex]
	mov [esp - 1], byte ' '
	dec esp
	call dword [cs:IUtility.Write_Char]

	push dword [cs:4 + ecx + 8]
	call dword [cs:IUtility.Write_Cardinal_Hex]

	call dword [cs:IVideo.New_Line]

	add ecx, 12
	cmp [cs:4 + ecx + 4], dword 0
	jne .Loop
	call dword [cs:IVideo.New_Line]
	ret

Init_Memory_Table:
	push $FFE00000
	push $FFFFFFFF
	push 0
	invoke ISystem.Create_Region

	push $FFE00000
	push $FFE7FFFF
	push $7000
	invoke ISystem.Mark_Memory
	ret

Print_Memory_Table:
	xor ecx, ecx

	.Loop:
	push dword ecx
	invoke IUtility.Write_Cardinal_Hex
	mov [esp - 1], byte ' '
	dec esp
	invoke IUtility.Write_Char

	push dword [$FFFFC000 + 4 + ecx]
	invoke IUtility.Write_Cardinal_Hex
	mov [esp - 1], byte ' '
	dec esp
	invoke IUtility.Write_Char

	push dword [$FFFFC000 + 4 + ecx + 4]
	invoke IUtility.Write_Cardinal_Hex
	mov [esp - 1], byte ' '
	dec esp
	invoke IUtility.Write_Char

	push dword [$FFFFC000 + 4 + ecx + 8]
	invoke IUtility.Write_Cardinal_Hex

	invoke IVideo.New_Line

	add ecx, 12
	cmp [$FFFFC000 + 4 + ecx + 4], dword 0
	jne .Loop
	invoke IVideo.New_Line
	ret

Init_Thread:
	sub esp, 4
	mov eax, esp
	push $7000
	push Thread_1
	push $2000
	push eax
	invoke IThread.New_Thread
	pop eax

	push eax
	invoke IThread.Start

	ret

Print_Thread_Table:
	mov ecx, 17

	.Loop:
	push dword ecx
	invoke IUtility.Write_Cardinal_Hex
	mov [esp - 1], byte ' '
	dec esp
	invoke IUtility.Write_Char

	push dword [$FFE80000 + ecx]
	invoke IUtility.Write_Cardinal_Hex
	mov [esp - 1], byte ' '
	dec esp
	invoke IUtility.Write_Char

	push dword [$FFE80000 + ecx + 4]
	invoke IUtility.Write_Cardinal_Hex
	mov [esp - 1], byte ' '
	dec esp
	invoke IUtility.Write_Char

	push dword [$FFE80000 + ecx + 8]
	invoke IUtility.Write_Cardinal_Hex
	mov [esp - 1], byte ' '
	dec esp
	invoke IUtility.Write_Char

	push dword [$FFE80000 + ecx + 12]
	invoke IUtility.Write_Cardinal_Hex
	mov [esp - 1], byte ' '
	dec esp
	invoke IUtility.Write_Char

	xor eax, eax
	mov al, [$FFE80000 + ecx + 16]
	push eax
	invoke IUtility.Write_Cardinal_Hex

	invoke IVideo.New_Line

	add ecx, 17
	cmp [$FFFFC000 + ecx], dword 0
	jne .Loop
	invoke IVideo.New_Line
	ret