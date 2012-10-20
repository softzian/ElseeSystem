use16

include 'include/PXE.inc'

Procedure_PXE_Start:
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
	mov [PXE_Var.EntryPointSP], eax

	; Get cached info
	push ds
	push t_PXENV_GET_CACHED_INFO
	push PXENV_GET_CACHED_INFO
	call dword [PXE_Var.EntryPointSP]
	add sp, 6

	test ax, ax
	jne Abort

	mov ax, word [t_PXENV_GET_CACHED_INFO.Buffer_Off]
	mov word [PXE_Var.Bootph], ax
	mov ax, word [t_PXENV_GET_CACHED_INFO.Buffer_Seg]
	mov word [PXE_Var.Bootph+2], ax

	les bx, [PXE_Var.Bootph]
	mov eax, [es:bx+20]
	mov [PXE_Var.SIP], eax

	ret

Procedure_PXE_Finish:
	xor ax, ax
	mov ds, ax
	push ds
	push t_PXENV_UNDI_SHUTDOWN
	push PXENV_UNDI_SHUTDOWN
	call dword [PXE_Var.EntryPointSP]
	add sp, 6

	xor ax, ax
	mov ds, ax
	push ds
	push t_PXENV_STOP_BASE
	push PXENV_STOP_BASE
	call dword [PXE_Var.EntryPointSP]
	add sp, 6

	xor ax, ax
	mov ds, ax
	push ds
	push t_PXENV_STOP_UNDI
	push PXENV_STOP_UNDI
	call dword [PXE_Var.EntryPointSP]
	add sp, 6

	ret

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
	mov eax, [PXE_Var.SIP]
	mov [t_PXENV_TFTP_READ_FILE.ServerIPAddress], eax

	push ds
	push t_PXENV_TFTP_READ_FILE
	push PXENV_TFTP_READ_FILE
	call [PXE_Var.EntryPointSP]

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
t_PXENV_STOP_BASE:
t_PXENV_STOP_UNDI:
t_PXENV_UNLOAD_STACK:
t_PXENV_UNDI_SHUTDOWN:
	.Status dw 0
	.reserved db 10 dup 0

PXE_Var:
	.EntryPointSP dd 0
	.Bootph dd 0
	.SIP dd 0