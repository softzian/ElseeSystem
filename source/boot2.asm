; Boot2.asm - FAT16 Boot sector v2
; Written in 2012 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

org $7C00

use16
	jmp Begin
	nop

FAT16_BPB:
	.OEM db "MSWIN4.1"
	.BytsPerSec dw 512
	.SecPerClus db 4
	.RsvdSecCnt dw 1
	.NumFATs db 2
	.RootEntCnt dw 512
	.TotSec16 dw 40257
	.Media db $F8
	.FATSz16 dw 40
	.SecPerTrk dw 63
	.NumHeads dw 16
	.HiddSec dd 63
	.TotSec32 dd 0

	.DrvNum db $80
	.Reserved1 db 0
	.BootSig db $29
	.VolID dd $FF00FF00
	.VolLab db "NO NAME    "
	.FilSysType db "FAT16   "

Begin:
	cli
	xor ax, ax
	mov ds, ax
	mov es, ax
	mov ax, $7000
	mov ss, ax
	mov sp, $FFFF

	; Check MBR to find boot partition
	mov bx, $7BE
	mov cx, 4
	.loop1:
	cmp byte [bx], $80
	je .Found_Boot_Partition
	add bx, 16
	loop .loop1
	jmp Halt

	.Found_Boot_Partition:
	mov ax, word [bx+8]
	mov word [Var.FirstLBA], ax
	mov dx, word [bx+10]
	mov word [Var.FirstLBA+2], dx

	; Load FAT and Root dir to Memory [$1000:$0000]
	add ax, [FAT16_BPB.RsvdSecCnt]
	adc dx, 0
	mov cl, [FAT16_BPB.NumFATs]
	dec cx
	.loop2:
	add ax, [FAT16_BPB.FATSz16]
	adc dx, 0
	loop .loop2
	call Function_LBA_to_CHS

	mov ax, [FAT16_BPB.RootEntCnt]
	shr ax, 4
	add ax, [FAT16_BPB.FATSz16]
	mov ah, 2

	mov dl, $80
	mov bx, $1000
	mov es, bx
	xor bx, bx
	int $13

	mov bx, $7E00
	mov si, Var.LoaderFilename
	call Procedure_Load_File
	jmp 0:$7E00

Function_LBA_to_CHS:	; Take LBA in DX AX, convert to CHS in CH DH CL
	cmp dx, $FC
	jnb Halt

	mov bx, 256*63
	div bx
	mov ch, al
	mov cl, ah
	mov ax, dx

	mov bl, 63
	div bl
	mov dh, al

	inc ah
	cmp ah, 63
	jna .j1
	sub ah, 63
	add dh, 1
	adc ch, 0
	adc cl, 0

	.j1:
	shl cl, 6
	add cl, ah
	ret

Function_Cluster_to_LBA:
	push bp
	mov bp, sp
	cmp ax, 2
	jb Halt

	sub ax, 2
	xor bx, bx
	mov bl, [FAT16_BPB.SecPerClus]
	mul bx

	add ax, word [Var.FirstLBA]
	adc dx, word [Var.FirstLBA+2]

	add ax, [FAT16_BPB.RsvdSecCnt]
	adc dx, 0
	push ax
	push dx

	mov ax, [FAT16_BPB.FATSz16]
	mov bl, [FAT16_BPB.NumFATs]
	mul bx
	add [bp-2], ax
	adc [bp-4], dx

	mov ax, [FAT16_BPB.RootEntCnt]
	shr ax, 4
	add [bp-2], ax
	adc [bp-4], dx

	pop dx
	pop ax
	pop bp
	ret

Procedure_Load_File:
	; [bp-2] is Dest offset(bx)
	; [bp-4] is Filename(si)
	; [bp-6] is Root dir limit

	push bp
	mov bp, sp
	push bx
	push si

	; Calculate Root directory offset in memory
	mov bx, $1000
	mov es, bx
	xor bx, bx
	mov ax, [FAT16_BPB.FATSz16]
	shl ax, 9
	add bx, ax

	; Calculate Root dir end
	mov ax, [FAT16_BPB.RootEntCnt]
	shl ax, 5
	add ax, bx
	push ax

	.loop2: ; Find file loop
	mov di, bx
	mov cx, 11
	repe cmpsb
	je .Found
	cmp bx, [bp-6]
	jnb Halt
	add bx, 32
	mov si, [bp-4]
	jmp .loop2

	.Found:
	mov ax, [es:bx+26]
	.loop3:
	push ax
	call Function_Cluster_to_LBA
	call Function_LBA_to_CHS
	mov ah, 2
	mov al, [FAT16_BPB.SecPerClus]
	mov dl, $80
	xor bx, bx
	mov es, bx
	mov bx, [bp-2]
	int $13

	mov bx, $1000
	mov es, bx
	pop bx
	shl bx, 1
	mov ax, [es:bx]
	cmp ax, $FFFF
	je .Done
	cmp ax, $FFF7
	je Halt
	xor bx, bx
	mov bl, [FAT16_BPB.SecPerClus]
	shl bx, 9
	add [bp-2], bx
	jmp .loop3

	.Done:
	mov sp, bp
	pop bp
	ret

Halt:
	hlt

Var:
	.FirstLBA dd 0
	.LoaderFilename db "LOADER  BIN"

repeat 510-($-$$)
	db 0
end repeat
	db $55
	db $AA