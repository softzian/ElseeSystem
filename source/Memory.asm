; Memory.asm - IMemory Basic Module
; Written in 2012 by Congdm
;
; To the extent possible under law, the author(s) have dedicated
; all copyright and related and neighboring rights to this software
; to the public domain worldwide. This software is distributed
; without any warranty.
; You should have received a copy of the CC0 Public Domain Dedication
; along with this software.
; If not, see http://creativecommons.org/publicdomain/zero/1.0/

include 'include/Header.inc'
include 'include/Errcode.inc'

use32

IMemory = $100000
; Function 1: Create_Region (Offset : Address; Limit : Cardinal; Type : Cardinal)
; Function 2: Allocate (var Ptr : Address; Region : Address; Size : Cardinal)
; Function 3: Deallocate (Ptr : Address; Region : Address)

Function_Init:
	push ebx
	push edi

	mov ebx, eax
	mov edi, IMemory
	mov [edi], eax
	lea eax, [ebx+Function_Create_Region]
	mov [edi+4], eax
	lea eax, [ebx+Function_Allocate]
	mov [edi+8], eax
	lea eax, [ebx+Function_Deallocate]
	mov [edi+12], eax

	xor eax, eax
	pop edi
	pop ebx
	ret

Function_Create_Region:  ; Function 1
	.Offset equ dword [ebp+16]
	.Limit equ dword [ebp+12]
	.Type equ dword [ebp+8]

	push ebp
	mov ebp, esp

	push ebx
	push ecx

	mov eax, .Type
	test eax, eax
	jnz .Error1

	mov eax, .Limit
	mov ebx, .Offset

	cmp eax, ebx
	jbe .Error2

	sub eax, ebx
	cmp eax, 24
	jb .Error2

	sub eax, 23
	mov ecx, eax

	mov [ebx], dword 0
	lea eax, [ebx+8]
	mov [ebx+4], eax
	xor eax, eax
	mov [ebx+8], eax
	mov [ebx+12], ecx
	mov [ebx+16], eax
	mov [ebx+20], eax

	.Return:
	pop ecx
	pop ebx
	leave
	ret 12
	.Error1:
	mov eax, UNSUPPORTED_FUNCTION
	jmp .Return
	.Error2:
	mov eax, NOT_LARGE_ENOUGH
	jmp .Return
	restore .Offset
	restore .Size
	restore .Type

Function_Allocate:    ; Function 2
	.Ptr equ dword [ebp+16] ; var Ptr : Address
	.Region equ dword [ebp+12] ; Region : Address
	.Size equ dword [ebp+8] ; Size : Cardinal

	push ebp
	mov ebp, esp

	push ebx
	push ecx
	push edx

	mov ecx, .Size

	mov ebx, .Region
	cmp [ebx], dword 0
	jne .Error1

	mov ebx, [ebx+4]
	test ebx, ebx
	jz .Error2

	mov edx, ebx
	.Repeat1:
	cmp [ebx], dword 0
	jne .Next1

	mov eax, [ebx+4]
	cmp eax, ecx
	je .Found_Equal
	jb .Next1

	cmp eax, [edx+4]
	jnb .Next1
	mov edx, ebx

	.Next1:
	mov eax, [ebx+12]
	test eax, eax
	jz .End_Repeat1

	mov ebx, eax
	jmp .Repeat1
	.End_Repeat1:

	mov ebx, edx
	mov eax, [ebx+4]
	cmp eax, ecx
	jb .Error2

	sub eax, ecx
	sub eax, 16
	jle .Found_Equal

	add edx, 16
	add edx, ecx
	mov [edx], dword 0
	mov [edx+4], eax
	mov [edx+8], ebx
	mov eax, [ebx+12]
	mov [edx+12], eax

	mov [ebx+12], edx
	mov [ebx+4], ecx

	.Found_Equal:
	mov edx, .Ptr
	lea eax, [ebx+16]
	mov [edx], eax

	mov [ebx], dword 1
	mov eax, .Region
	cmp [eax+4], ebx
	jne .Done

	.Repeat2:
	mov ebx, [ebx+12]
	test ebx, ebx
	jz .End_Repeat2

	cmp [ebx], dword 0
	jne .Repeat2
	.End_Repeat2:

	mov [eax+4], ebx

	.Done:
	xor eax, eax

	.Return:
	pop edx
	pop ecx
	pop ebx
	leave
	ret 12

	.Error1:
	mov eax, UNSUPPORTED_FUNCTION
	jmp .Return
	.Error2:
	mov eax, NO_FREE_MEMORY
	jmp .Return
	restore .Ptr
	restore .Region
	restore .Size

Function_Deallocate:	; Function 3
	.Ptr equ dword [ebp+12] ; Ptr : Address
	.Region equ dword [ebp+8] ; Region : Address

	mov eax, [esp+4]	; [esp+4] is Region
	cmp [eax], dword 0
	jne .Error1

	push ebp
	mov ebp, esp

	push ebx
	push ecx

	mov ebx, .Ptr
	sub ebx, 16
	mov [ebx], dword 0

	mov eax, [ebx+8]
	call .Merge_Free_Blocks
	mov eax, ebx
	mov ebx, [eax+12]
	call .Merge_Free_Blocks

	mov ebx, .Region
	cmp [ebx+4], dword 0
	je .Change
	cmp eax, [ebx+4]
	jae .Done
	.Change: mov [ebx+1], eax

	.Done:
	xor eax, eax
	.Return:
	pop ecx
	pop ebx
	leave
	ret 8
	.Error1:
	mov eax, UNSUPPORTED_FUNCTION
	ret 8

.Merge_Free_Blocks:
	test eax, eax
	jz .Exit
	test ebx, ebx
	jz .Exit
	cmp [eax], dword 0
	jnz .Exit
	cmp [ebx], dword 0
	jnz .Exit

	mov ecx, [ebx+12]
	mov [eax+12], ecx

	test ecx, ecx
	jz .j1
	mov [ecx+8], eax
	.j1:

	mov ecx, [ebx+4]
	add ecx, 16
	add [eax+4], ecx

	mov ebx, eax
	.Exit: ret

	restore .Ptr
	restore .Region