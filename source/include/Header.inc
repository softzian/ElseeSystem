; System data
System_data = $10000
Total_RAM = 0
Free_RAM = 8
First_free_phy_page = 16
Page_fault_count = 24
Error_handler = 32
; End System data

macro invoke i*, x*
{
	mov rax, [dword i + 8]
	call qword [rax + i#.#x]
}

macro Save_all_registers
{
	push rbp
	push rax
	push rbx
	push rcx
	push rdx
	push rsi
	push rdi
	push r8
	push r9
	push r10
	push r11
	push r12
	push r13
	push r14
	push r15
}

macro Load_all_registers
{
	pop r15
	pop r14
	pop r13
	pop r12
	pop r11
	pop r10
	pop r9
	pop r8
	pop rdi
	pop rsi
	pop rdx
	pop rcx
	pop rbx
	pop rax
	pop rbp
}

macro Error_return
{
	cli
	hlt
	jmp .Return
}

macro Raise_error x, func_id, mod_id
{
	.Error#x#:
	mov rax, x
	mov rbx, func_id
	mov rcx, [mod_id]
	mov rdx, [mod_id + 8]
	jmp qword [dword System_data + Error_handler]
}

macro Debug_write_line x
{
	push rax
	push r11
	push r12
	push r13
	push r15

	push x
	push 0
	invoke IException, Card64_to_hex

	push 0
	push 16
	invoke IException, Write_line

	pop r15
	pop r13
	pop r12
	pop r11
	pop rax
}

MASK_12_LOW_BITS = $FFFFFFFFFFFFF000

IException = $100000
IException.Install_ISR = 0
IException.Write = 8
IException.Write_line = 16
IException.Card64_to_hex = 24
IException.Card64_to_decimal = 32

IMemory = $100010
IMemory.Allocate = 0
IMemory.Free = 8
IMemory.Allocate_physical_page = 16
IMemory.Free_physical_page = 24
IMemory.Map_one_page = 32
IMemory.Trace_page_table = 40

IModule = $100020
IModule.Register_module = 0
IModule.Find_module = 8
IModule.New_address_space = 16
IModule.Switch_address_space = 24
IModule.Get_thread_table = 32
IModule.Set_thread_table = 40

IPIC = $100030
IPIC.Install_IRQ_handler = 0
IPIC.Mask_all_IRQ = 8
IPIC.Enable_IRQ = 16
IPIC.Disable_IRQ = 24
IPIC.Send_EOI = 32

IThread = $100040
IThread.New_thread = 0
IThread.Start_thread = 8
IThread.Enable_threading = 16
IThread.Disable_threading = 24