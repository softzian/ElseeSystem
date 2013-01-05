macro Set_Active_Module x
{
	mov [ss:_ModuleIdx], dword x
}

Init_base_system:
	; ------------------ ;
	; Init System module ;
	; ------------------ ;
	mov eax, $20000 	; System module
	Set_Active_Module eax
	Init_module eax

	call Init_Code_table
	call Init_Memory_Table

	; ------------------ ;
	; Init other modules ;
	; ------------------ ;
	mov eax, $21000 	; Video
	Set_Active_Module eax
	Init_module eax

	mov eax, $22000 	; Interrupt
	Set_Active_Module eax
	Init_module eax

	mov eax, $23000 	; Thread
	Set_Active_Module eax
	Init_module eax

	mov eax, $24000 	; Data
	Set_Active_Module eax
	Init_module eax

	mov eax, $25000 	; Network
	Set_Active_Module eax
	Init_module eax

	;mov eax, $32000         ; RTL8139
	;Set_Active_Module eax
	;Init_module eax

	mov eax, $27000 	; Am79C970
	Set_Active_Module eax
	Init_module eax

	Set_Active_Module $7000
	mov [gs:ebp], dword 1
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], dword $7000
	invoke ISystem, ISystem.Register_Module

	; ------------------------------- ;
	; Create console and start thread ;
	; ------------------------------- ;
	cli

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

Init_Code_table:
	mov [gs:ebp], dword $1FFFFF
	invoke ISystem, ISystem.Create_Code_Region

	mov [gs:ebp], dword $4000
	mov [gs:ebp + 4], dword $6FFF
	mov [gs:ebp + 8], dword 3
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $7000
	mov [gs:ebp + 4], dword $FFFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $10000
	mov [gs:ebp + 4], dword $1FFFF
	mov [gs:ebp + 8], dword 3
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $20000
	mov [gs:ebp + 4], dword $20FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $21000
	mov [gs:ebp + 4], dword $21FFF
	mov [gs:ebp + 8], dword 1
	invoke ISystem, ISystem.Mark_Code

	mov [gs:ebp], dword $22000
	mov [gs:ebp + 4], dword $22FFF
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
	invoke ISystem, ISystem.Create_Region

	mov [gs:ebp], dword $204000
	mov [gs:ebp + 4], dword $205FFF
	mov [gs:ebp + 8], dword $7000
	invoke ISystem, ISystem.Mark_Memory

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