GDT:
	.gdt_null:	; 0 - Null
		dq 0
	.gdt_code32:	; 1 - Code 32
		dw $FFFF
		dw 0
		db 0
		db 10011010b
		db 11001111b
		db 0
	.gdt_gdata32:	; 2 - Data 32
		dw $FFFF
		dw 0
		db 0
		db 10010010b
		db 11001111b
		db 0
	.gdt_code64:	; 3 - Code 64
		dw $FFFF
		dw 0
		db 0
		db 10011010b
		db 10101111b
		db 0
	.gdt_gdata64:	; 4 - Data 64
		dw $FFFF
		dw 0
		db 0
		db 10010010b
		db 10101111b
		db 0
	.tss64: 	  ; 5, 6 - TSS
		dw 103
		dw TSS64
		db 0
		db 10001001b
		db 0
		db 0
		dd 0
		dd 0
	gdt_end:

GDT_Desc:
	dw gdt_end - GDT - 1
	dd GDT

TSS64:
	dd 0
	.RSP0 dq 0
	.RSP1 dq 0
	.RSP2 dq 0
	dd 0
	dd 0
	.IST1 dq $10000
	.IST2 dq $F000
	.IST3 dq $10000
	.IST4 dq $10000
	.IST5 dq $10000
	.IST6 dq $10000
	.IST7 dq $10000
	dd 0
	dd 0
	dw 0
	.IO_map dw 0