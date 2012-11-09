macro Set_Active_Module x
{
	mov [ss:_ModuleIdx], dword x
}

Init_base_system:
	; ------------------ ;
	; Init System module ;
	; ------------------ ;
	mov eax, $10000 	; System module
	call eax

	call Init_code_table
	call Init_Memory_Table

	; ------------------ ;
	; Init other modules ;
	; ------------------ ;
	mov eax, $11000 	; Video
	Set_Active_Module eax
	call eax

	mov eax, $13000 	; Interrupt
	Set_Active_Module eax
	call eax

	mov eax, $14000 	; Thread
	Set_Active_Module eax
	call eax

	mov eax, $15000 	; Keyboard
	Set_Active_Module eax
	call eax

	;push $12000
	;invoke ISystem.Set_Active_Module
	;mov eax, $12000         ; Utility
	;call eax

	mov eax, $16000 	; Console
	Set_Active_Module eax
	call eax

	;push $17000
	;invoke ISystem.Set_Active_Module
	;mov eax, $17000         ; Conversion
	;call eax

	;push $7000
	;invoke ISystem.Set_Active_Module

	; ------------------------------- ;
	; Create console and start thread ;
	; ------------------------------- ;
	cli

	invoke IConsole.Alloc_console
	mov eax, [ss:_Result]
	mov [fs:Var.Console], eax

	mov [gs:ebp], eax
	invoke IConsole.Switch_console

	mov [gs:ebp], dword Main_thread
	invoke IThread.New_Thread

	mov eax, [ss:_Result]
	mov [gs:ebp], eax
	invoke IThread.Start

	; Enable IRQ0, start threads
	mov [gs:ebp], byte 0
	invoke IInterrupt.Enable_IRQ

	sti

	ret

Init_code_table:
	mov [gs:ebp], dword $1FFFFF
	invoke ISystem.Create_Code_Region

	mov [gs:ebp], dword $4000
	mov [gs:ebp + 4], dword $6FFF
	mov [gs:ebp + 8], dword 2
	invoke ISystem.Mark_Code

	mov [gs:ebp], dword $7000
	mov [gs:ebp + 4], dword $8FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem.Mark_Code

	mov [gs:ebp], dword $10000
	mov [gs:ebp + 4], dword $10FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem.Mark_Code

	mov [gs:ebp], dword $11000
	mov [gs:ebp + 4], dword $11FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem.Mark_Code

	;push $12000
	;push $12FFF
	;push 1
	;invoke ISystem.Mark_Code

	mov [gs:ebp], dword $13000
	mov [gs:ebp + 4], dword $13FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem.Mark_Code

	mov [gs:ebp], dword $14000
	mov [gs:ebp + 4], dword $14FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem.Mark_Code

	mov [gs:ebp], dword $15000
	mov [gs:ebp + 4], dword $15FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem.Mark_Code

	mov [gs:ebp], dword $16000
	mov [gs:ebp + 4], dword $16FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem.Mark_Code

	;push $17000
	;push $17FFF
	;push 1
	;invoke ISystem.Mark_Code

	mov [gs:ebp], dword $20000
	mov [gs:ebp + 4], dword $2FFFF
	mov [gs:ebp + 8], dword 2
	invoke ISystem.Mark_Code

	ret

Print_Code_Table:
	xor ecx, ecx

	.Loop:
	;push dword ecx
	;call dword [cs:IUtility.Write_Cardinal_Hex]
	;mov [esp - 1], byte ' '
	;dec esp
	;call dword [cs:IUtility.Write_Char]

	mov eax, [cs:4 + ecx]
	Write_register eax

	mov eax, [cs:4 + ecx + 4]
	Write_register eax

	mov eax, [cs:4 + ecx + 8]
	Write_register eax

	invoke IVideo.New_Line

	add ecx, 12
	cmp [cs:4 + ecx + 4], dword 0
	jne .Loop
	invoke IVideo.New_Line
	ret

Init_Memory_Table:
	mov [gs:ebp], dword $FFE00000
	mov [gs:ebp + 4], dword $FFFFFFFF
	mov [gs:ebp + 8], dword 0
	invoke ISystem.Create_Region

	ret

Print_Memory_Table:
	xor ecx, ecx

	.Loop:
	;push dword ecx
	;invoke IUtility.Write_Cardinal_Hex
	;mov [esp - 1], byte ' '
	;dec esp
	;invoke IUtility.Write_Char

	mov eax, [ds:$FFFFC000 + 4 + ecx]
	Write_register eax

	mov eax, [ds:$FFFFC000 + 4 + ecx + 4]
	Write_register eax

	mov eax, [ds:$FFFFC000 + 4 + ecx + 8]
	Write_register eax

	invoke IVideo.New_Line

	add ecx, 12
	cmp [ds:$FFFFC000 + 4 + ecx + 4], dword 0
	jne .Loop
	invoke IVideo.New_Line
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