; Loader2.asm - Early stage module loader v3 - PXE Boot
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
include 'include\Pxe.inc'

org $7C00

use16

PXE_Loader:
	mov bp, sp

	; Checksum PXENV+ struct
	mov di, bx
	cmp dword [es:di], 'PXEN'
	jne Abort
	cmp word [es:di+4], 'V+'
	jne Abort

	mov cl, [es:di+8]
	xor ch, ch
	xor al, al
	.Loop:
	add al, [es:di]
	inc di
	loop .Loop
	test al, al
	jnz Abort

	cmp [es:bx+6], word $0201
	jb Abort

	; Checksum PXE struct
	les bx, [es:bx+$28]
	mov di, bx
	cmp dword [es:di], '!PXE'
	jne Abort

	mov cl, [es:di+4]
	xor ch, ch
	xor al, al
	.Loop2:
	add al, [es:di]
	inc di
	loop .Loop2
	test al, al
	jnz Abort

	xor ax, ax
	mov ds, ax
	mov eax, [es:bx+$10]
	mov [Var.EntryPointSP], eax

	; Get cached info
	push ds
	push t_PXENV_GET_CACHED_INFO
	push PXENV_GET_CACHED_INFO
	call dword [Var.EntryPointSP]
	add sp, 6

	test ax, ax
	jne Abort

	mov ax, word [t_PXENV_GET_CACHED_INFO.Buffer_Off]
	mov word [Var.Bootph], ax
	mov ax, word [t_PXENV_GET_CACHED_INFO.Buffer_Seg]
	mov word [Var.Bootph+2], ax

	les bx, [Var.Bootph]
	mov eax, [es:bx+20]
	mov [Var.SIP], eax

	;push Var.Interrupt_Module
	;push dword $10000
	;call Function_Download_File

	push Var.Memory_Module
	push dword $11000
	call Function_Download_File

	test ax, ax
	jnz Abort

	push Var.Video_Module
	push dword $12000
	call Function_Download_File

	test ax, ax
	jnz Abort

	push Var.Utility_Module
	push dword $13000
	call Function_Download_File

	test ax, ax
	jnz Abort

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

Function_Download_File: ; (var Filename : String; Addr : Address32)
	.Filename equ word [bp+8]
	.Addr equ dword [bp+4]

	push bp
	mov bp, sp
	push cx
	push ds
	push si
	push es
	push di

	; Set t_PXENV_TFTP_READ_FILE.FileName to Filename
	xor ax, ax
	mov ds, ax
	mov es, ax

	mov si, .Filename
	mov cx, [ds:si]
	cmp cx, 127
	ja .Error1     ; Filename too long

	add si, 2
	mov di, t_PXENV_TFTP_READ_FILE.FileName

	rep movsb
	mov [es:di], byte 0

	; Set t_PXENV_TFTP_READ_FILE.Buffer to Addr
	mov eax, .Addr
	mov [t_PXENV_TFTP_READ_FILE.Buffer], eax
	; BufferSize is fixed as 4096 bytes
	mov [t_PXENV_TFTP_READ_FILE.BufferSize], $1000
	mov eax, [Var.SIP]
	mov [t_PXENV_TFTP_READ_FILE.ServerIPAddress], eax

	push ds
	push t_PXENV_TFTP_READ_FILE
	push PXENV_TFTP_READ_FILE
	call [Var.EntryPointSP]

	.Return:
	pop di
	pop es
	pop si
	pop ds
	pop cx
	leave
	ret 6
	.Error1:
	mov ax, -1 ; Filename too long
	jmp .Return

	restore .Filename
	restore .Offset

include 'Loader3_p1.inc'

Function_GetAddr:
	mov bp, sp
	mov bx, [bp]
	ret

Var:
	.Text1 db 19,0,'Address to bootph: '
	.Memory_Module db 10,0,'Memory.bin'
	.Video_Module db 7,0,'VGA.bin'
	.Utility_Module db 11,0,'Utility.bin'
	.EntryPointSP dd 0
	.Bootph dd 0
	.SIP dd 0

t_PXENV_GET_CACHED_INFO:
	.Status dw 0
	.PacketType dw PXENV_PACKET_TYPE_DHCP_ACK
	.BufferSize dw 0
	.Buffer_Off dw 0
	.Buffer_Seg dw 0
	.BufferLimit dw 0
t_PXENV_TFTP_READ_FILE:
	.Status dw 0
	.FileName db 128 dup 0
	.BufferSize dd 0
	.Buffer dd 0
	.ServerIPAddress dd 0
	.GatewayIPAddress dd 0
	.McastIPAddress dd 0
	.TFTPClntPort dw 69
	.TFTPSrvPort dw 69
	.TFTPOpenTimeOut dw 0
	.TFTPReopenDelay dw 10

Halt:
	hlt
	jmp Halt

Abort:
	xor ax, ax
	mov sp, bp
	retf

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
	gdt_null:
		dq 0
	gdt_code:
		dw $FFFF
		dw 0
		db 0
		db 10011010b
		db 11001111b
		db 0
	gdt_data:
		dw $FFFF
		dw 0
		db 0
		db 10010010b
		db 11001111b
		db 0
	gdt_end:
GDT_Desc:
	dw gdt_end - GDT
	dd GDT


use32

Begin:
	mov ax, $10
	mov ds, ax
	mov ss, ax
	mov es, ax
	mov esp, $400000

	mov eax, $11000
	call eax
	mov eax, $12000
	call eax
	mov eax, $13000
	call eax

	call dword [IVideo.Clear_Screen]

	push 0
	push $3FFFFF
	push 0
	call dword [IMemory.Create_Region]

	push $10000
	push 0
	push $3000
	call dword [IMemory.Allocate]

	call Print_Memory_Table

	push $0
	push $11000
	push $11FFF
	push $1
	call dword [IMemory.Mark_Memory]

	call Print_Memory_Table

	push $0
	push $12000
	push $12FFF
	push $2
	call dword [IMemory.Mark_Memory]

	call Print_Memory_Table

	hlt

Print_Memory_Table:
	xor ecx, ecx

	.Loop:
	push dword [4+ecx]
	call dword [IUtility.Write_Cardinal_Hex]
	mov [esp-1], byte ' '
	dec esp
	call dword [IUtility.Write_Char]

	push dword [4+ecx+4]
	call dword [IUtility.Write_Cardinal_Hex]
	mov [esp-1], byte ' '
	dec esp
	call dword [IUtility.Write_Char]

	push dword [4+ecx+8]
	call dword [IUtility.Write_Cardinal_Hex]

	call dword [IVideo.New_Line]

	add ecx, 12
	cmp [4+ecx+4], dword 0
	jne .Loop
	call dword [IVideo.New_Line]
	ret

Function_Load_Module:
	.Offset equ dword [ebp+12]
	.Filename equ dword [ebp+8]

	enter 0, 0

	xor eax, eax

	.Return:
	leave
	ret 8
	restore .Offset
	restore .Filename