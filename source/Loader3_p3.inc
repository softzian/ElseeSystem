Init_core_system:
	; ------------------- ;
	; Init Memory manager ;
	; ------------------- ;
	mov eax, $20000 	; Memory
	Set_Active_Module eax
	Init_module eax

	call Init_Code_table
	call Init_Memory_Table

	; ------------------- ;
	; Init Module manager ;
	; ------------------- ;
	mov eax, $21000 	; Module
	Set_Active_Module eax
	Init_module eax

	mov [gs:ebp], dword 2
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], dword $20000
	invoke IModule, IModule.Register_Module

	; ------------------ ;
	; Init other modules ;
	; ------------------ ;
	mov eax, $22000 	; Video
	Set_Active_Module eax
	Init_module eax

	movzx eax, word [fs:$19000]
	Write_register eax
	movzx eax, word [fs:$19002]
	Write_register eax
	cli
	hlt

	mov eax, $23000 	; Interrupt
	Set_Active_Module eax
	Init_module eax

	mov eax, $24000 	; Thread
	Set_Active_Module eax
	Init_module eax

	;mov eax, $24000         ; System2
	;Set_Active_Module eax
	;Init_module eax

	Set_Active_Module $7000
	mov [gs:ebp], dword 1
	mov [gs:ebp + 4], dword 0
	mov [gs:ebp + 8], dword 0
	mov [gs:ebp + 12], dword 0
	mov [gs:ebp + 16], dword $7000
	invoke IModule, IModule.Register_Module

	; ------------------------------- ;
	;          Start thread           ;
	; ------------------------------- ;
	mov [gs:ebp], dword Main_thread
	mov [gs:ebp + 4], dword $7000
	mov [gs:ebp + 8], dword $1000
	mov [gs:ebp + 12], dword $1000
	invoke IThread, IThread.New_Thread

	mov eax, [ss:_Result]
	mov [gs:ebp], eax
	invoke IThread, IThread.Start

	invoke IThread, IThread.Enable_context_switch

	ret

Init_Code_table:
	mov [gs:ebp], dword $1FFFFF
	invoke IMemory, IMemory.Create_Code_Region

	mov [gs:ebp], dword $4000
	mov [gs:ebp + 4], dword $6FFF
	mov [gs:ebp + 8], dword 4
	invoke IMemory, IMemory.Mark_Code

	mov [gs:ebp], dword $7000
	mov [gs:ebp + 4], dword $9FFF
	mov [gs:ebp + 8], dword 1
	invoke IMemory, IMemory.Mark_Code

	mov [gs:ebp], dword $A000
	mov [gs:ebp + 4], dword $FFFF
	mov [gs:ebp + 8], dword 4
	invoke IMemory, IMemory.Mark_Code

	mov [gs:ebp], dword $10000
	mov [gs:ebp + 4], dword $1FFFF
	mov [gs:ebp + 8], dword 3
	invoke IMemory, IMemory.Mark_Code

	mov [gs:ebp], dword $20000
	mov [gs:ebp + 4], dword $20FFF
	mov [gs:ebp + 8], dword 1
	invoke IMemory, IMemory.Mark_Code

	mov [gs:ebp], dword $21000
	mov [gs:ebp + 4], dword $21FFF
	mov [gs:ebp + 8], dword 1
	invoke IMemory, IMemory.Mark_Code

	mov [gs:ebp], dword $22000
	mov [gs:ebp + 4], dword $22FFF
	mov [gs:ebp + 8], dword 1
	invoke IMemory, IMemory.Mark_Code

	mov [gs:ebp], dword $23000
	mov [gs:ebp + 4], dword $23FFF
	mov [gs:ebp + 8], dword 1
	invoke IMemory, IMemory.Mark_Code

	mov [gs:ebp], dword $24000
	mov [gs:ebp + 4], dword $24FFF
	mov [gs:ebp + 8], dword 1
	invoke IMemory, IMemory.Mark_Code

	mov [gs:ebp], dword $25000
	mov [gs:ebp + 4], dword $25FFF
	mov [gs:ebp + 8], dword 1
	invoke IMemory, IMemory.Mark_Code

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

	invoke ICoreVideo, ICoreVideo.New_Line

	add ecx, 12
	cmp [fs:4 + ecx + 4], dword 0
	jne .Loop
	invoke ICoreVideo, ICoreVideo.New_Line
	ret

Init_Memory_Table:
	mov [gs:ebp], dword $200000
	mov [gs:ebp + 4], dword $3FFFFF
	invoke IMemory, IMemory.Create_Region

	ret

Print_Memory_Table:
	xor ecx, ecx

	.Loop:

	mov eax, [fs:$A004 + ecx]
	Write_register eax

	mov eax, [fs:$A004 + ecx + 4]
	Write_register eax

	mov eax, [fs:$A004 + ecx + 8]
	Write_register eax

	invoke ICoreVideo, ICoreVideo.New_Line

	add ecx, 12
	cmp [fs:$A004 + ecx + 4], dword 0
	jne .Loop
	invoke ICoreVideo, ICoreVideo.New_Line
	ret