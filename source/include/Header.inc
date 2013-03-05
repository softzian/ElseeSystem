_Result = $FFFFFFF0
_ModuleIdx = $FFFFFFEC
_ThreadIdx = $FFFFFFE8

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

macro Write_register x
{
	push eax
	push ds

	push dword $4 * 8
	pop ds

	mov [gs:ebp], x
	mov [gs:ebp + 4], dword $0
	call dword [fs:$4014] ; call Function_Cardinal_to_HexStr_32

	mov [gs:ebp], dword $0
	mov [gs:ebp + 4], word 9
	invoke ICoreVideo, ICoreVideo.Write_Telex

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
	invoke ICoreVideo, ICoreVideo.Write_Telex

	pop ds
	pop eax
}

macro Get_DS_space
{
	mov eax, [fs:$4000 + 24]
}

macro Get_ES_space
{
	mov eax, [fs:$4000 + 28]
}

macro Init_module x
{
	call x
}

macro Set_Active_Module x
{
	mov [ss:_ModuleIdx], dword x
}

IMemory = $100000
IMemory.Create_Region = 0
IMemory.Allocate = 4
IMemory.Deallocate = 8
IMemory.Mark_Memory = 12
IMemory.Create_Code_Region = 16
IMemory.Allocate_Code = 20
IMemory.Deallocate_Code = 24
IMemory.Mark_Code = 28

IModule = $100008
IModule.Register_Module = 0
IModule.Find_module = 4
IModule.Set_module_thread_table = 8
IModule.Get_module_thread_table = 12
IModule.Save_system_state = 16
IModule.Load_system_state = 20
IModule.Add_address_space = 24
IModule.Set_DS_space = 28
IModule.Set_ES_space = 32

IInterrupt = $100010
IInterrupt.Install_ISR = 0
IInterrupt.Install_IRQ_handler = 4
IInterrupt.Install_IRQ_direct_handler = 8
IInterrupt.Mask_all_IRQ = 12
IInterrupt.Enable_IRQ = 16
IInterrupt.Disable_IRQ = 20
IInterrupt.Send_EOI = 24

IThread = $100018
IThread.New_Thread = 0
IThread.Start = 4
IThread.Yield = 8
IThread.Block_self = 12
IThread.Block = 16
IThread.Disable_context_switch = 20
IThread.Enable_context_switch = 24

ICoreVideo = $100020
ICoreVideo.Write_Telex = 0
ICoreVideo.Clear_Screen = 4
ICoreVideo.New_Line = 8

ICore = $100028
ICore.Allocate = 0
ICore.Deallocate = 4
ICore.Allocate_code = 8
ICore.Deallocate_code = 12
ICore.Register_module = 16
ICore.New_thread = 20
ICore.Add_address_space = 24
