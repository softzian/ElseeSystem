macro invoke i*, x*
{
	mov rax, [i + 8]
	call qword [rax + i#.#x]
}

IException = $100000
IException.Install_ISR = 0
IException.Write = 8
