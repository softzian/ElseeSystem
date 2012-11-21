True = 1
False = 0

_Result = $FFFFFFF0
_ModuleIdx = $FFFFFFEC
_ThreadIdx = $FFFFFFE8
_LDT = $20000

macro invoke x
{
	call dword [cs:x]
}

macro Write_register x
{
	push eax
	mov [gs:ebp], x
	mov [gs:ebp + 4], dword $FFFF0000
	call Function_Cardinal_to_HexStr_32

	mov [gs:ebp], dword $FFFF0000
	mov [gs:ebp + 4], word 9
	invoke IVideo.Write_Telex

	pop eax
}

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

IVideo = $100800
IVideo.Write_Telex = IVideo + 4
IVideo.Clear_Screen = IVideo + 8
IVideo.New_Line = IVideo + 12
IVideo.Write_text_line = IVideo + 16
IVideo.Write_text_char = IVideo + 20
IVideo.Scroll_screen = IVideo + 24
IVideo.Get_display_size = IVideo + 28
IVideo.Scroll_rect = IVideo + 32

IData = $100A00
IData.Create_table = IData + 4

IData.Modify_table = IData + 8
IData.Finish_modify_table = IData + 12
IData.Access_table = IData + 16
IData.Finish_access_table = IData + 20

IData.Add_table_entry = IData + 24
IData.Delete_table_entry = IData + 28
IData.Get_table_entry = IData + 32

IData.Delete_table = IData + 36

IConvert = $100C00
IConvert.UTF16_to_raw_UTF32 = IConvert + 4

IConsole = $100E00
IConsole.Alloc_console = IConsole + 4
IConsole.Write_char = IConsole + 8
IConsole.Read_char = IConsole + 12
IConsole.Switch_console = IConsole + 16
IConsole.Lock_console = IConsole + 20
IConsole.Release_console = IConsole + 24
