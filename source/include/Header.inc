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