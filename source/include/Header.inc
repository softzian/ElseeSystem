True = 1
False = 0

_Result = $FFFFFFF0
_ModuleIdx = $FFFFFFEC
_ThreadIdx = $FFFFFFE8

HANDLE_VIDEO_CONTEXT = 1

macro invoke i, x
{
	mov eax, [fs:i + 4]
	call dword [fs:eax + x]
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

	push dword $8 * 8
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

macro Write_reg_byte x
{
	push eax
	push ds

	push dword $8 * 8
	pop ds

	mov [gs:ebp], x
	mov [gs:ebp + 4], dword $0
	call dword [fs:$4014] ; call Function_Cardinal_to_HexStr_32

	mov [gs:ebp], dword $6
	mov [gs:ebp + 4], word 3
	invoke IVideo, IVideo.Write_Telex

	pop ds
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
ISystem.Register_Module = 32
ISystem.Set_module_thread_table = 36
ISystem.Get_module_thread_table = 40
ISystem.Modify_GDT_entry = 44
ISystem.Save_GDT_entry = 48
ISystem.Load_GDT_entry = 52
;
ISystem.Add_address_space = 56
ISystem.Set_DS_space = 60
ISystem.Set_ES_space = 64
ISystem.Find_module = 68
;
ISystem.Install_ISR = 72

IInterrupt = $100008
IInterrupt.Install_IRQ_handler = 0
IInterrupt.Install_IRQ_direct_handler = 4
IInterrupt.Mask_all_IRQ = 8
IInterrupt.Enable_IRQ = 12
IInterrupt.Disable_IRQ = 16
IInterrupt.Send_EOI = 20

IThread = $100010
IThread.New_Thread = 0
IThread.Start = 4
IThread.Yield = 8
IThread.Block_self = 12
IThread.Block = 16
IThread.Disable_context_switch = 20
IThread.Enable_context_switch = 24

IVideo = $100018
IVideo.Write_Telex = 0
IVideo.Clear_Screen = 4
IVideo.New_Line = 8

ISystem2 = $100020
ISystem2.Allocate = 0
ISystem2.Deallocate = 4
ISystem2.Allocate_Code = 8
ISystem2.Deallocate_Code = 12
ISystem2.Register_Module = 16
ISystem2.New_Thread = 20
ISystem2.Add_address_space = 24

IData = $100028
IData.Create_table = 0
IData.Add_table_entry = 4
IData.Delete_table_table = 8
IData.Get_table_entry = 12

INetwork = $100030
INetwork.Add_adapter = 0
INetwork.Transmit = 4
INetwork.Add_receiver = 8
INetwork.Receive_packet = 12

IKeyboard = $100010
IKeyboard.Set_target_queue = IKeyboard + 0
IKeyboard.Shift_character = IKeyboard + 4

IHandle = $100A00
IHandle.Create_handle = IHandle + 4
IHandle.Delete_handle = IHandle + 8
IHandle.Request_handle = IHandle + 12
IHandle.Resolve_handle = IHandle + 16
IHandle.Release_handle = IHandle + 20

IConsole = $100E00
IConsole.Alloc_console = IConsole + 4
IConsole.Write_char = IConsole + 8
IConsole.Read_char = IConsole + 12
IConsole.Switch_console = IConsole + 16
IConsole.Lock_console = IConsole + 20
IConsole.Release_console = IConsole + 24
