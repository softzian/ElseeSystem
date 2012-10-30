True = 1
False = 0

_Result = $FFFFFFF0
_ModuleIdx = $FFFFFFFC
_ThreadIdx = $FFFFFFE8
_LDT = $20000

macro invoke x
{
	call dword [cs:x]
}

macro Write_register x
{
	mov [gs:ebp], x
	mov [gs:ebp + 4], dword $FFFF0000
	add [gs:0], dword 8
	push eax
	call Function_Cardinal_to_HexStr_32

	mov [gs:ebp], dword $FFFF0000
	mov [gs:ebp + 4], word 9
	add [gs:0], dword 6
	invoke IVideo.Write_Telex

	pop eax
}

IVideo = $100800
IVideo.Write_Telex = IVideo + 4
IVideo.Clear_Screen = IVideo + 8
IVideo.New_Line = IVideo + 12
IVideo.Blit_text = IVideo + 16

ISystem = $100000
ISystem.Create_Region = ISystem + 4
ISystem.Allocate = ISystem + 8
ISystem.Deallocate = ISystem + 12
ISystem.Mark_Memory = ISystem + 16

ISystem.Create_Code_Region = ISystem + 20
ISystem.Allocate_Code = ISystem + 24
ISystem.Deallocate_Code = ISystem + 28
ISystem.Mark_Code = ISystem + 32

ISystem.Create_Message_Queue = ISystem + 36
ISystem.Send_Message = ISystem + 40
ISystem.Get_Message = ISystem + 44

ISystem.Copy_code_to_data = ISystem + 48

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
IThread.Block_self = IThread + 16

IConvert = $100C00
IConvert.UTF16_to_raw_UTF32 = IConvert + 4

IConsole = $100E00
IConsole.Alloc_console = IConsole + 4
IConsole.Write_console = IConsole + 8
IConsole.Read_console = IConsole + 12
IConsole.Switch_console = IConsole + 16
