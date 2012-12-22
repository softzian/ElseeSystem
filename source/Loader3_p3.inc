macro Set_Active_Module x
{
	mov [ss:_ModuleIdx], dword x
}

Init_base_system:
	; ------------------ ;
	; Init System module ;
	; ------------------ ;
	mov eax, $10000 	; System module
	Set_Active_Module eax
	add [fs:eax], eax
	Init_module eax

	call Init_code_table
	call Init_Memory_Table

	; ------------------ ;
	; Init other modules ;
	; ------------------ ;
	mov eax, $11000 	; Video
	Set_Active_Module eax
	add [fs:eax], eax
	Init_module eax

	mov eax, $13000 	; Interrupt
	Set_Active_Module eax
	add [fs:eax], eax
	Init_module eax

	mov eax, $14000 	; Thread
	Set_Active_Module eax
	add [fs:eax], eax
	Init_module eax

	;mov eax, $18000         ; Handle
	;Set_Active_Module eax
	;call eax

	;mov eax, $15000         ; Keyboard
	;Set_Active_Module eax
	;call eax

	;push $12000
	;invoke ISystem.Set_Active_Module
	;mov eax, $12000         ; Utility
	;call eax

	;mov eax, $16000         ; Console
	;Set_Active_Module eax
	;call eax

	;push $17000
	;invoke ISystem.Set_Active_Module
	;mov eax, $17000         ; Conversion
	;call eax

	;mov eax, $30000         ; Data
	;Set_Active_Module eax
	;call eax

	;mov eax, $31000         ; Network
	;Set_Active_Module eax
	;call eax

	;mov eax, $32000         ; RTL8139
	;Set_Active_Module eax
	;add [fs:eax], eax
	;call dword [fs:eax]

	Set_Active_Module $7000
	mov [gs:ebp], dword 0
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 1
	mov [gs:ebp + 16], dword $7000
	invoke ISystem, ISystem.Register_Module

	; ------------------------------- ;
	; Create console and start thread ;
	; ------------------------------- ;
	cli

	;invoke IConsole.Alloc_console
	;mov eax, [ss:_Result]
	;mov [fs:Var.Console], eax

	;mov [gs:ebp], eax
	;invoke IConsole.Switch_console

	;invoke IConsole.Alloc_console
	;mov eax, [ss:_Result]
	;mov [fs:Var.Console2], eax

	mov [gs:ebp], dword Main_thread
	invoke IThread, IThread.New_Thread

	mov eax, [ss:_Result]
	mov [gs:ebp], eax
	invoke IThread, IThread.Start

	; Enable IRQ0, start threads
	mov [gs:ebp], byte 0
	invoke IInterrupt, IInterrupt.Enable_IRQ

	sti

	ret

Init_code_table:
	mov [gs:ebp], dword $1FFFFF
	invoke ISystem, ISystem.Create_Code_Region

	mov [gs:ebp], dword $4000
	mov [gs:ebp + 4], dword $6FFF
	mov [gs:ebp + 8], dword 3
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $7000
	mov [gs:ebp + 4], dword $8FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $10000
	mov [gs:ebp + 4], dword $10FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $11000
	mov [gs:ebp + 4], dword $11FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	;push $12000
	;push $12FFF
	;push 1
	;invoke ISystem.Mark_Code

	mov [gs:ebp], dword $13000
	mov [gs:ebp + 4], dword $13FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $14000
	mov [gs:ebp + 4], dword $14FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $15000
	mov [gs:ebp + 4], dword $15FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $16000
	mov [gs:ebp + 4], dword $16FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	;push $17000
	;push $17FFF
	;push 1
	;invoke ISystem.Mark_Code

	mov [gs:ebp], dword $18000
	mov [gs:ebp + 4], dword $18FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $19000
	mov [gs:ebp + 4], dword $19FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $20000
	mov [gs:ebp + 4], dword $2FFFF
	mov [gs:ebp + 8], dword 2
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $30000
	mov [gs:ebp + 4], dword $30FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $31000
	mov [gs:ebp + 4], dword $31FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $32000
	mov [gs:ebp + 4], dword $32FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	ret

Print_Code_Table:
	xor ecx, ecx

	.Loop:
	mov eax, [fs:4 + ecx]
	Write_register eax

	mov eax, [fs:4 + ecx + 4]
	Write_register eax

	mov eax, [fs:4 + ecx + 8]
	Write_register eax

	invoke IVideo, IVideo.New_Line

	add ecx, 12
	cmp [fs:4 + ecx + 4], dword 0
	jne .Loop
	invoke IVideo, IVideo.New_Line
	ret

Init_Memory_Table:
	mov [gs:ebp], dword $200000
	mov [gs:ebp + 4], dword $3FFFFF
	mov [gs:ebp + 8], dword 0
	invoke ISystem, ISystem.Create_Region

	ret

Print_Memory_Table:
	xor ecx, ecx

	.Loop:

	mov eax, [ds:4 + ecx]
	Write_register eax

	mov eax, [ds:4 + ecx + 4]
	Write_register eax

	mov eax, [ds:4 + ecx + 8]
	Write_register eax

	invoke IVideo, IVideo.New_Line

	add ecx, 12
	cmp [ds:4 + ecx + 4], dword 0
	jne .Loop
	invoke IVideo, IVideo.New_Line
	ret