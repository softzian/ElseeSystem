True = 1
False = 0

_Result = $FFFFFFF0
_ModuleIdx = $FFFFFFEC
_ThreadIdx = $FFFFFFE8

HANDLE_VIDEO_CONTEXT = 1

macro invoke i, x
{
	mov eax, [fs:i]
	call dword [fs:eax + 9 + x]
}

macro invoke2 i, x
{
	call dword [fs:i + 9 + x]
}

macro invoke3 i, x
{
	mov eax, i
	call dword [fs:eax + 9 + x]
}

macro Init_module x
{
	call x
}

macro Write_register x
{
	push eax
	push ds

	push dword $7 * 8
	pop ds

	mov [gs:ebp], x
	mov [gs:ebp + 4], dword $0
	call dword [fs:$4014] ; call Function_Cardinal_to_HexStr_32

	mov [gs:ebp], dword $0
	mov [gs:ebp + 4], word 9
	invoke IVideo, IVideo.Write_Telex

	pop ds
	pop eax
}

macro Write_stack
{
	pop dword [gs:ebp]
	push eax
	push ds

	push dword $7 * 8
	pop ds

	mov [gs:ebp + 4], dword $0
	call dword [fs:$4014] ; call Function_Cardinal_to_HexStr_32

	mov [gs:ebp], dword $0
	mov [gs:ebp + 4], word 9
	invoke IVideo, IVideo.Write_Telex

	pop ds
	pop eax
}

macro Write_reg_byte x
{
	push eax
	mov [gs:ebp], x
	mov [gs:ebp + 4], dword $0
	call dword [fs:$4014] ; call Function_Cardinal_to_HexStr_32

	mov [gs:ebp], dword $6
	mov [gs:ebp + 4], word 3
	invoke IVideo, IVideo.Write_Telex

	pop eax
}

ISystem = $100000
ISystem.Create_Region = 0
ISystem.Allocate = 4
ISystem.Deallocate = 8
ISystem.Mark_Memory = 12
;
ISystem.Create_Code_Region = 16
ISystem.Allocate_Code = 20
ISystem.Deallocate_Code = 24
ISystem.Mark_Code = 28
;
ISystem.Copy_code_to_data = 32
ISystem.Copy_data_to_code = 36
;
ISystem.Register_Module = 40
ISystem.Set_module_thread_table = 44
ISystem.Get_module_thread_table = 48
ISystem.Modify_GDT_entry = 52
ISystem.Save_GDT_entry = 56
ISystem.Load_GDT_entry = 60
ISystem.Add_address_space = 64
ISystem.Get_address_space = 68
ISystem.Set_DS_space = 72
ISystem.Set_ES_space = 76
ISystem.Find_module = 80
;
ISystem.Install_ISR = 84

IInterrupt = $100004
IInterrupt.Install_IRQ_handler = 0
IInterrupt.Install_IRQ_direct_handler = 4
IInterrupt.Mask_all_IRQ = 8
IInterrupt.Enable_IRQ = 12
IInterrupt.Disable_IRQ = 16
IInterrupt.Send_EOI = 20

IThread = $100008
IThread.New_Thread = 0
IThread.Start = 4
IThread.Yield = 8
IThread.Block_self = 12
IThread.Block = 16

IVideo = $10000C
IVideo.Write_Telex = 0
IVideo.Clear_Screen = 4
IVideo.New_Line = 8
IVideo.Alloc_context = 12
;IVideo.Write_text_char = IVideo + 16
;IVideo.Scroll_screen = IVideo + 20
IVideo.Lock_context = 24
IVideo.Release_context = 28
IVideo.Switch_context = 32
IVideo.Write_text_char = 40
IVideo.Write_text_line = 44
IVideo.Clear_text_screen = 48
IVideo.Set_text_cursor = 52

IKeyboard = $100010
IKeyboard.Set_target_queue = IKeyboard + 0
IKeyboard.Shift_character = IKeyboard + 4

IHandle = $100A00
IHandle.Create_handle = IHandle + 4
IHandle.Delete_handle = IHandle + 8
IHandle.Request_handle = IHandle + 12
IHandle.Resolve_handle = IHandle + 16
IHandle.Release_handle = IHandle + 20

INetwork = $100018
INetwork.Add_adapter = 0
INetwork.Transmit = 4

IConsole = $100E00
IConsole.Alloc_console = IConsole + 4
IConsole.Write_char = IConsole + 8
IConsole.Read_char = IConsole + 12
IConsole.Switch_console = IConsole + 16
IConsole.Lock_console = IConsole + 20
IConsole.Release_console = IConsole + 24

IData = $100014
IData.Create_table = 0
IData.Add_table_entry = 4
IData.Delete_table_table = 8
IData.Get_table_entry = 12