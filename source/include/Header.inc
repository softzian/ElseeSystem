True = 1
False = 0

macro invoke x
{
	call dword [cs:x]
}

macro print x
{
	push x
	push $FFFE0000
	mov [esp - 2], word 8
	sub esp, 2
	invoke IUtility.Cardinal_to_HexStr
	xor ecx, ecx
	@@:
		mov al, [$FFFE0002 + ecx]
		mov [fs:$B8000 + ecx * 2], al
		mov [fs:$B8000 + ecx * 2 + 1], byte 1111b
		inc ecx
		cmp ecx, 8
		jb @b
	hlt
}

macro print2 x
{
	push x
	invoke IUtility.Write_Cardinal_Hex
	mov [esp - 1], byte ' '
	dec esp
	invoke IUtility.Write_Char
}

IVideo = $100800
IVideo.Write_Telex = IVideo + 4
IVideo.Clear_Screen = IVideo + 8
IVideo.Set_Cursor = IVideo + 12
IVideo.Get_Cursor = IVideo + 16
IVideo.New_Line = IVideo + 20
IVideo.Move_Cursor = IVideo + 24
IVideo.Write = IVideo + 28
IVideo.Switch_Window = IVideo + 32
IVideo.Blit_text = IVideo + 36

ISystem = $100000
ISystem.Create_Region = ISystem + 4
ISystem.Allocate = ISystem + 8
ISystem.Deallocate = ISystem + 12
ISystem.Mark_Memory = ISystem + 16
ISystem.Create_Code_Region = ISystem + 20
ISystem.Allocate_Code = ISystem + 24
ISystem.Deallocate_Code = ISystem + 28
ISystem.Mark_Code = ISystem + 32
ISystem.Set_Active_Module = ISystem + 36
ISystem.Get_Active_Module = ISystem + 40
ISystem.Create_Message_Queue = ISystem + 44
ISystem.Send_Message = ISystem + 48
ISystem.Get_Message = ISystem + 52
ISystem.Copy_code_to_data = ISystem + 56

IKeyboard = $100200
IKeyboard.Set_target_queue = IKeyboard + 4

IUtility = $100A00
IUtility.Cardinal_to_HexStr = IUtility + 4
IUtility.Write_Cardinal_Hex = IUtility + 8
IUtility.Write_Char = IUtility + 12
IUtility.Write_String = IUtility + 16
IUtility.HexStr_to_Cardinal = IUtility + 44
IUtility.HexChar_to_Byte = IUtility + 48

IInterrupt = $100400
IInterrupt.Install_ISR = IInterrupt + 4
IInterrupt.Init_PIC = IInterrupt + 8
IInterrupt.Mask_all_IRQ = IInterrupt + 12
IInterrupt.Enable_IRQ = IInterrupt + 16
IInterrupt.Send_EOI = IInterrupt + 20

IThread = $100600
IThread.New_Thread = IThread + 4
IThread.Start = IThread + 8
IThread.Yield = IThread + 12

IConvert = $100C00
IConvert.UTF16_to_raw_UTF32 = IConvert + 4

IConsole = $100E00
IConsole.Alloc_console = IConsole + 4
IConsole.Write_console = IConsole + 8
IConsole.Read_console = IConsole + 12
IConsole.Switch_console = IConsole + 16
