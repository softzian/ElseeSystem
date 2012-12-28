; Loader3.asm - Early stage module loader v3 - PXE Boot
; Written in 2012 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

include 'include\Header.inc'
include 'include\Errcode.inc'

org $7C00

use16

PXE_Loader:
	call Procedure_PXE_Start

	push Var.System_Module
	push dword $10000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Video_Module
	push dword $11000
	call Function_Download_File
	test ax, ax
	jnz Abort

	;push Var.Utility_Module
	;push dword $12000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

	push Var.Interrupt_Module
	push dword $13000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Thread_Module
	push dword $14000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Keyboard_Module
	push dword $15000
	call Function_Download_File
	test ax, ax
	jnz Abort

	;push Var.Console_Module
	;push dword $16000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

	;push Var.Convert_Module
	;push dword $17000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

	;push Var.Handle_Module
	;push dword $18000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

	;push Var.Editor_Module
	;push dword $19000
	;call Function_Download_File
	;test ax, ax
	;jnz Abort

	push Var.Data_Module
	push dword $30000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.Network_Module
	push dword $31000
	call Function_Download_File
	test ax, ax
	jnz Abort

	push Var.RTL8139_Module
	push dword $32000
	call Function_Download_File
	test ax, ax
	jnz Abort

	call Procedure_PXE_Finish

Switch_to_Protected_Mode:
	cli
	call enable_A20
	xor ax, ax
	mov ds, ax
	lgdt [ds:GDT_Desc]
	mov eax, cr0
	or eax, 1
	mov cr0, eax
	jmp $8:Begin

include 'Loader3_p1.inc'
include 'Loader3_p2.inc'

Var:
	.Interrupt_Module db 9,0,'8259A.bin'
	.System_Module db 10,0,'System.bin'
	.Video_Module db 7,0,'VGA.bin'
	.Utility_Module db 11,0,'Utility.bin'
	.Thread_Module db 10,0,'Thread.bin'
	.Keyboard_Module db 8,0,'8042.bin'
	.Console_Module db 11,0,'Console.bin'
	.Convert_Module db 11,0,'Convert.bin'
	.Handle_Module db 10,0,'Handle.bin'
	.Editor_Module db 10,0,'Editor.bin'
	.Data_Module db 8,0,'Data.bin'
	.Network_Module db 11,0,'Network.bin'
	.RTL8139_Module db 11,0,'RTL8139.bin'

	.Text db 7,0,'Success'
	.Text2 db 'Hello World'
	.UTF32_Str dd 'H','e','l','l','o',' ','W','o','r','l','d'
	.Console dd 0
	.Console2 dd 0

Halt:
	hlt
	jmp Halt

Abort:
	call Print_Failure
	jmp Halt

enable_A20:
	call .a20wait
	mov al, $AD
	out $64, al

	call .a20wait
	mov al, $D0
	out $64, al

	call .a20wait2
	in al, $60
	push ax

	call .a20wait
	mov al, $D1
	out $64, al

	call .a20wait
	pop ax
	or al, 2
	out $60, al

	call .a20wait
	mov al, $AE
	out $64, al

	call .a20wait
	ret

.a20wait:
	in al, $64
	test al, 2
	jnz .a20wait
	ret
.a20wait2:
	in al, $64
	test al, 1
	jz .a20wait2
	ret

GDT:
	.gdt_null:	; 0
		dq 0
	.gdt_code:	; 1 - CS
		dw $01FF
		dw 0
		db 0
		db 10011010b
		db 11000000b
		db 0
	.gdt_gdata:	; 2 - FS
		dw $FFFF
		dw 0
		db 0
		db 10010010b
		db 11001111b
		db 0
	.gdt_data:	; 3 - DS
		dw $1
		dw $4000
		db $20
		db 10010010b
		db 11000000b
		db 0
	.gdt_stack1:	; 4 - SS
		dw $FFFE
		dw $5000
		db 2
		db 10010110b
		db 11001111b
		db 0
	.gdt_stack2:	; 5 - GS
		dw 0
		dw $0000
		db 2
		db 10010010b
		db 11000000b
		db 0
	.gdt_data2:	 ; 6 - ES
		dw $1
		dw $4000
		db $20
		db 10010010b
		db 11000000b
		db 0
	.gdt_data3:	 ; 7 - DS2
		dw $0000
		dw $F000
		db $0
		db 10010010b
		db 11000000b
		db 0
	.gdt_stack3:	; 8 - GS2
		dw 0
		dw $E000
		db 0
		db 10010010b
		db 11000000b
		db 0
GDT_end:
GDT_Desc:
	dw GDT_end - GDT
	dd GDT

use32

Begin:
	mov ax, 2 * 8
	mov fs, ax

	mov ax, 3 * 8
	mov ds, ax
	mov es, ax

	mov ax, 4 * 8
	mov ss, ax
	mov esp, $FFFFFFE0

	mov ax, 5 * 8
	mov gs, ax
	mov ebp, 16

	mov [fs:$4000], dword 0
	mov [fs:$4004], dword GDT
	mov [fs:$4008], dword 0
	mov [fs:$400C], dword 0
	mov [fs:$4010], dword 0
	mov [fs:$4014], dword Function_Cardinal_to_HexStr_32

	call Init_base_system

	jmp Halt32

Main_thread:
	mov [gs:ebp], dword $300000
	mov [gs:ebp + 4], dword 0
	invoke ISystem, ISystem.Add_address_space

	mov ecx, $7000
	xor edx, edx
	invoke ISystem, ISystem.Set_address_space

	mov [ds:0], dword $19932012
	mov eax, [fs:$300000]
	Write_register eax
	cli

;        mov [gs:ebp], dword Packet
;        mov [gs:ebp + 4], dword $FFFF1000
;        mov [gs:ebp + 8], dword 11
;        invoke ISystem.Copy_code_to_data
;
;        mov [gs:ebp], dword 1
;        mov [gs:ebp + 4], dword $FFFFFFFF
;        mov [gs:ebp + 8], word $FFFF
;        mov [gs:ebp + 10], dword $FFFF1000
;        mov [gs:ebp + 14], word 11
;        mov [gs:ebp + 16], word $9
;        invoke INetwork.Transmit
	;jmp dword $19000

Halt32:
	hlt
	jmp Halt32

Packet:
	db 'Hello World'

Function_Cardinal_to_HexStr_32:
	.Num equ dword [gs:ebp - 8]
	.HexStr equ dword [gs:ebp - 4]

	push ebp
	add ebp, 8
	push ebx
	push ecx
	push edx
	push edi

	mov edx, .Num
	xor ebx, ebx
	mov edi, .HexStr

	mov cl, 7
	.Loop:
	mov eax, edx
	shl cl, 2
	shr eax, cl
	shr cl, 2
	and al, $F

	cmp al, $A
	jae .j1
	add al, '0' - 0
	jmp .j2
	.j1: add al, 'A' - $A
	.j2: inc ebx

	mov [ds:edi + ebx - 1], al

	.Continue_loop:
	dec cl
	jns .Loop

	.Return:
	xor eax, eax
	pop edi
	pop edx
	pop ecx
	pop ebx

	pop ebp
	ret

	restore .Num
	restore .HexStr

include 'Loader3_p3.inc'

