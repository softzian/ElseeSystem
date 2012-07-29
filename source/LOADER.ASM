; Loader.asm - Early stage module loader
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

org $800

use16

Switch_to_Protected_Mode:
	call enable_A20
	lgdt [GDT_Desc]
	mov eax, cr0
	or eax, 1
	mov cr0, eax
	jmp $8:Begin

use32

Begin:
	mov ax, $10
	mov ds, ax
	mov ss, ax
	mov es, ax
	mov esp, $400000

	push $200000
	push Var.IInterrupt_Modulename
	call Function_Load_Module
	sti

	push $201000
	push Var.IVideo_Modulename
	call Function_Load_Module

	push $202000
	push Var.IKeyboard_Modulename
	call Function_Load_Module

	push $203000
	push Var.ISysUtils_Modulename
	call Function_Load_Module

	call dword [IVideo.Clear_Display]

	push Var.Text+2
	sub esp, 2
	mov ax, word [Var.Text]
	mov [esp], ax
	call dword [IVideo.Write_Telex]

	push Var.Buff+2
	sub esp, 2
	mov word [esp], 255
	push Var.Buff
	call dword [IKeyboard.Read]

	push Var.Buff+2
	sub esp, 2
	mov ax, word [Var.Buff]
	mov [esp], ax
	call dword [IVideo.Write_Telex]

	.loop:
	hlt
	jmp .loop

Function_Load_Module:
	.Offset equ dword [ebp+12]
	.Filename equ dword [ebp+8]

	enter 0, 0

	push .Offset
	push .Filename
	call Procedure_Load_File_in_Root

	mov eax, .Offset
	call eax

	leave
	ret 8
	restore .Offset
	restore .Filename

Function_Read_Sectors:
	.LBA equ dword [ebp+13]
	.Offset equ dword [ebp+9]
	.Sector_Count equ byte [ebp+8]

	enter 0, 0
	push ebx
	push ecx
	push edx
	push edi

	; Check Status Register
	mov dx, $1F7
	in al, dx
	test al, 10001000b
	jnz .Busy

	; Set Drive/Head Register, LBA Mode
	dec edx
	mov ebx, .LBA
	mov eax, ebx
	shr eax, 24
	or al, 11100000b
	and al, 11101111b
	out dx, al

	; Set Cylinder High Register
	dec edx
	mov eax, ebx
	shr eax, 16
	out dx, al

	; Set Cylinder Low Register
	dec edx
	mov eax, ebx
	shr eax, 8
	out dx, al

	; Set Sector Register
	dec edx
	mov eax, ebx
	out dx, al

	; Set Sector Count Register
	dec edx
	mov al, .Sector_Count
	out dx, al

	; Send command
	add dl, 5
	mov al, $20
	out dx, al

	.Loop_until_not_Busy:
	in al, dx
	test al, 10000000b
	jnz .Loop_until_not_Busy

	test al, 00001000b
	jz .Error

	.Transfer:
	mov edi, .Offset
	sub dl, 7
	mov ecx, 256

	cld
	rep insw

	.Check_result:
	mov .Offset, edi
	add dl, 7
	in al, dx
	test al, 10001000b
	jnz .Loop_until_not_Busy

	.Success:
	xor eax, eax
	.Return:
	pop edi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 9

	.Error:
	mov eax, $000000FF
	hlt
	.Busy:
	mov eax, $000000FE
	hlt

	restore .Buff
	restore .LBA
	restore .Sector_Count

Procedure_Load_File_in_Root:
	.Offset equ dword [ebp+12]
	.Filename equ dword [ebp+8]
	.Root_Dir_Limit equ dword [ebp-4]
	FAT16_BPB.SecPerClus equ byte [$7C00+13]
	FAT16_BPB.RootEntCnt equ word [$7C00+17]
	FAT16_BPB.FATSz16 equ word [$7C00+22]

	enter 0, 4
	push ebx
	push ecx
	push edi
	push esi

	; Load Root Dir offset to EBX
	mov ebx, $10000
	xor eax, eax
	mov ax, FAT16_BPB.FATSz16
	shl eax, 9
	add ebx, eax

	; Calculate Root dir Limit
	xor eax, eax
	mov ax, FAT16_BPB.RootEntCnt
	shl eax, 5
	add eax, ebx
	mov .Root_Dir_Limit, eax

	mov esi, .Filename
	.loop2: ; Find file loop
	mov edi, ebx
	mov ecx, 11
	repe cmpsb
	je .Found
	cmp ebx, .Root_Dir_Limit
	jnb Halt
	add ebx, 32
	mov esi, .Filename
	jmp .loop2

	.Found:
	mov ax, [ebx+26]
	.loop3:
	push ax        ; Save cluster index to Stack
	call Function_Cluster_to_LBA
	push eax
	push .Offset
	mov al, FAT16_BPB.SecPerClus
	dec esp
	mov [esp], al
	call Function_Read_Sectors

	xor ebx, ebx
	pop bx	; Pop cluster index to BX
	shl ebx, 1
	mov ax, [$10000+ebx]
	cmp ax, $FFFF
	je .Done
	cmp ax, $FFF7
	je Halt
	xor ebx, ebx
	mov bl, FAT16_BPB.SecPerClus
	shl ebx, 9
	add .Offset, ebx
	jmp .loop3

	.Done:
	pop esi
	pop edi
	pop ecx
	pop ebx
	leave
	ret 8

	restore .Offset
	restore .Filename
	restore .Root_Dir_Limit

Function_Cluster_to_LBA:	; Input: AX is cluster index
	.FirstLBA = $7E00
	FAT16_BPB.RsvdSecCnt equ word [$7C00+14]
	FAT16_BPB.NumFATs equ byte [$7C00+16]

	cmp ax, 2
	jb Halt

	enter 0, 0
	push ebx
	push edx

	; eax = (Cluster-2) x SecPerClus
	push ax
	xor eax, eax
	pop ax
	sub eax, 2
	xor ebx, ebx
	mov bl, FAT16_BPB.SecPerClus
	mul ebx

	add eax, dword [.FirstLBA]

	mov bx, FAT16_BPB.RsvdSecCnt
	add eax, ebx
	push eax

	xor eax, eax
	xor ebx, ebx
	mov ax, FAT16_BPB.FATSz16
	mov bl, FAT16_BPB.NumFATs
	mul ebx
	add [esp], eax

	xor eax, eax
	mov ax, FAT16_BPB.RootEntCnt
	shr eax, 4	; RootDirSecCnt = RootEntCnt / 32 / 512 = RootEntCnt / 16
	add [esp], eax

	pop eax

	pop edx
	pop ebx
	leave
	ret

Halt:
	hlt

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
	dw gdt_end - GDT - 1
	dd GDT

Var:
	.Filename db 'LOADER  ASM'
	.Text db 14,0,'Input string: '
	.Buff db 0,0, 255 dup 0

	.IInterrupt_Modulename db '8259A   BIN'
	.IVideo_Modulename db 'VGA     BIN'
	.IKeyboard_Modulename db '8042    BIN'
	.ISysUtils_Modulename db 'SYSUTILSBIN'

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