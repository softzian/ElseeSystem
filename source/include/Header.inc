True = 1
False = 0

_Result = $FFFFFFF0
_ModuleIdx = $FFFFFFEC
_ThreadIdx = $FFFFFFE8
_LDT = $20000

HANDLE_VIDEO_CONTEXT = 1

macro invoke i, x
{
	mov eax, [fs:i]
	call dword [fs:eax + 4 + x]
}

macro Init_module x
{
	call dword [fs:x]
}

macro Write_register x
{
	push eax
	mov [gs:ebp], x
	mov [gs:ebp + 4], dword $100000
	call dword [fs:$4014] ; call Function_Cardinal_to_HexStr_32

	mov [gs:ebp], dword $100000
	mov [gs:ebp + 4], word 9
	invoke IVideo, IVideo.Write_Telex

	pop eax
}

macro Write_reg_byte x
{
	push eax
	mov [gs:ebp], x
	mov [gs:ebp + 4], dword $FFFF0000
	call dword [fs:$4014] ; call Function_Cardinal_to_HexStr_32

	mov [gs:ebp], dword $FFFF0006
	mov [gs:ebp + 4], word 3
	invoke IVideo.Write_Telex

	pop eax
}

ISystem = $100000
ISystem.Create_Region = 4
ISystem.Allocate = 8
ISystem.Deallocate = 12
ISystem.Mark_Memory = 16
;
ISystem.Create_Code_Region = 20
ISystem.Allocate_Code = 24
ISystem.Deallocate_Code = 28
ISystem.Mark_Code = 32
;
ISystem.Create_Message_Queue = 36
ISystem.Send_Message = 40
ISystem.Get_Message = 44
;
ISystem.Copy_code_to_data = 48
ISystem.Copy_data_to_code = 52
;
ISystem.Register_Module = 56
ISystem.Set_module_thread_table = 60
ISystem.Get_module_thread_table = 64
ISystem.Modify_GDT_entry = 68
ISystem.Save_GDT_entry = 72
ISystem.Load_GDT_entry = 76

IInterrupt = $100004
IInterrupt.Install_ISR = 4
IInterrupt.Init_PIC = 8
IInterrupt.Mask_all_IRQ = 12
IInterrupt.Enable_IRQ = 16
IInterrupt.Send_EOI = 20

IThread = $100008
IThread.New_Thread = 4
IThread.Start = 8
IThread.Yield = 12
IThread.Block_self = 16

IKeyboard = $100200
IKeyboard.Set_target_queue = IKeyboard + 4
IKeyboard.Shift_character = IKeyboard + 8

IVideo = $10000C
IVideo.Write_Telex = 4
IVideo.Clear_Screen = 8
IVideo.New_Line = 12
IVideo.Alloc_context = 16
;IVideo.Write_text_char = IVideo + 20
;IVideo.Scroll_screen = IVideo + 24
IVideo.Lock_context = 28
IVideo.Release_context = 32
IVideo.Switch_context = 36
IVideo.Write_text_char = 44
IVideo.Write_text_line = 48
IVideo.Clear_text_screen = 52
IVideo.Set_text_cursor = 56

IHandle = $100A00
IHandle.Create_handle = IHandle + 4
IHandle.Delete_handle = IHandle + 8
IHandle.Request_handle = IHandle + 12
IHandle.Resolve_handle = IHandle + 16
IHandle.Release_handle = IHandle + 20

INetwork = $100C00
INetwork.Add_adapter = INetwork + 4
INetwork.Transmit = INetwork + 8

IConsole = $100E00
IConsole.Alloc_console = IConsole + 4
IConsole.Write_char = IConsole + 8
IConsole.Read_char = IConsole + 12
IConsole.Switch_console = IConsole + 16
IConsole.Lock_console = IConsole + 20
IConsole.Release_console = IConsole + 24

IData = $101000
IData.Create_table = IData + 4
IData.Add_table_entry = IData + 8
IData.Delete_table_table = IData + 12
IData.Read_table_entry = IData + 16
IData.Finish_read_table_entry = IData + 20
IData.Modify_table_entry = IData + 24
IData.Finish_modify_table_entry = IData + 28
IData.Get_table_entry_address = IData + 32
IData.Delete_table = IData + 36