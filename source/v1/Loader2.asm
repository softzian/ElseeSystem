; Loader2.asm - Early stage module loader v2
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

org $7E00

use16

Switch_to_Protected_Mode:
	call enable_A20
	lgdt [GDT_Desc]
	mov eax, cr0
	or eax, 1
	mov cr0, eax
	jmp $8:Begin

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

use32

Begin:
	mov ax, $10
	mov ds, ax
	mov ss, ax
	mov es, ax
	mov esp, $400000

	push 0
	push Var.IMemory_Modulename
	call Function_Load_Module

	push $10000
	push $3FFFFF
	push 0
	call dword [IMemory.Create_Region]

	push $200000
	push $10000
	push $400000
	call dword [IMemory.Allocate]

	push dword [$200000]
	push Var.IInterrupt_Modulename
	call Function_Load_Module

	push $200004
	push $10000
	push $1000
	call dword [IMemory.Allocate]

	push dword [$200004]
	push $10000
	call dword [IMemory.Deallocate]

	hlt

Function_Load_Module:
	.Offset equ dword [ebp+12]
	.Filename equ dword [ebp+8]

	enter 0, 0

	push .Offset
	push .Filename
	call Function_Load_File_in_Root

	test eax, eax
	jne .Return

	mov eax, .Offset
	call eax

	xor eax, eax

	.Return:
	leave
	ret 8
	restore .Offset
	restore .Filename

Function_Read_Sectors:
	.LBA equ dword [ebp+13] ; LBA : Card32
	.Offset equ dword [ebp+9]	; Offset : Address
	.Sector_Count equ byte [ebp+8]	; Sector_Count : Card8

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


; ----------------- Function Load_File_in_Root Section -------------------

Function_Load_File_in_Root:
	.Offset equ dword [ebp+12]	; Offset : Address
	.Filename equ dword [ebp+8]	; var Filename : Array[11] of Char
	; Local Var:
	.SecPerClus equ byte [ebp-1]
	.LBA_FAT equ dword [ebp-5]
	.LBA_Clus2 equ dword [ebp-9]
	.ClusIdx equ word [ebp-2]

	push ebp
	mov ebp, esp
	sub esp, 9

	push ebx
	push ecx
	push edx
	push esi
	push edi

	; Read MBR
	push 0
	push ATA_Buff
	mov [esp-1], byte 1
	dec esp
	call Function_Read_Sectors

	; Find bootable partition
	xor eax, eax
	.Repeat1:
	cmp [ATA_Buff+$1BE+eax], byte $80
	je .End_Repeat1
	cmp al, 48
	je .Error1	; No boot partition
	add al, 16
	jmp .Repeat1
	.End_Repeat1:

	; Load boot sector
	mov eax, [ATA_Buff+$1BE+eax+8]
	mov .LBA_FAT, eax
	push eax
	push ATA_Buff
	mov [esp-1], byte 1
	dec esp
	call Function_Read_Sectors

	mov al, [ATA_Buff+13]
	mov .SecPerClus, al

	; Calculate LBA_FAT, LBA_FAT = LBA of partition + RsvdSecCnt
	xor eax, eax
	mov ax, [ATA_Buff+14]  ; RsvdSecCnt
	add .LBA_FAT, eax

	; Calculate LBA_Root, LBA_Root = LBA_FAT + FATSz16 * NumFATs
	xor eax, eax
	mov ax, [ATA_Buff+22]	; FATSz16
	mov dl, [ATA_Buff+16]	; NumFATs
	xor dh, dh
	mul dx
	add eax, .LBA_FAT

	; Calculate LBA_Clus2, LBA_Clus2 = LBA_Root + RootEntCnt * 32 div 512
	xor edx, edx
	mov dx, [ATA_Buff+17]	; RootEntCnt
	shr dx, 4
	add edx, eax
	mov .LBA_Clus2, edx

	jmp .Find_File_in_Root
	.Found:

	mov esi, .LBA_Clus2
	mov ebx, .LBA_FAT
	xor ch, ch
	mov cl, .SecPerClus
	mov edi, .Offset

	; Get Cluster Index in Root Dir Entry
	mov ax, [ATA_Buff+eax+26]

	test ax, ax
	jz .Error3	; Empty file

	; Translate Cluster Index to LBA
	.Clus_to_LBA:
	mov .ClusIdx, ax
	sub ax, 2
	mul cx
	shl edx, 16
	mov dx, ax
	add edx, esi

	push edx
	push edi
	mov [esp-1], cl
	dec esp
	call Function_Read_Sectors

	; Get FAT[ClusIdx] value
	xor eax, eax
	mov ax, .ClusIdx
	mov ch, al
	shr ax, 8
	add eax, ebx

	push eax
	push ATA_Buff
	mov [esp-1], byte 1
	dec esp
	call Function_Read_Sectors

	xor eax, eax
	mov al, ch
	xor ch, ch
	mov ax, [ATA_Buff+eax*2]

	; Check if EOF?
	cmp ax, $FFFF
	je .Done

	xor edx, edx
	mov dl, cl
	shl edx, 9
	add edi, edx
	jmp .Clus_to_LBA

	.Done:
	xor eax, eax
	.Return:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	leave
	ret 8

	.Error1:
	mov eax, UNSPECIFIC_ERROR
	jmp .Return
	.Error2:
	mov eax, FILE_NOT_FOUND
	jmp .Return
	.Error3:
	mov eax, EMPTY_FILE
	jmp .Return

.Find_File_in_Root:
	mov ebx, edx	; EDX still has the value of LBA_Clus2
	mov edx, eax	; EAX still has the value of LBA_Root

	.Repeat2:
	push edx
	push ATA_Buff
	mov [esp-1], byte 1
	dec esp
	call Function_Read_Sectors

		xor eax, eax
		.Repeat2_a:
		mov esi, .Filename
		lea edi, [ATA_Buff+eax]
		xor ecx, ecx
		mov cl, 11
		repe cmpsb
		je .Found
		add eax, 32
		cmp ax, 512
		jne .Repeat2_a

	inc edx
	cmp edx, ebx
	jne .Repeat2
	jmp .Error2	; File not found

	restore .ClusIdx
	restore .SecPerClus
	restore .LBA_FAT
	restore .LBA_Clus2
	restore .Filename
	restore .Offset

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
	.IMemory_Modulename db 'MEMORY  BIN'
	.IInterrupt_Modulename db '8259A   BIN'
	.IVideo_Modulename db 'VGA     BIN'
	.IKeyboard_Modulename db '8042    BIN'
	.IUtility_Modulename db 'UTILITY BIN'

ATA_Buff: