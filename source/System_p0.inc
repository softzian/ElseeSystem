; System_p0.inc - Part 0 (header) of System.asm
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

Const:
	SizeOf_Region_Header = 4
	SizeOf_Memory_Entry = 12
	SizeOf_Memory_Table = ($4000 - SizeOf_Region_Header)
	Region_Address = $FFFFFFFF - $4000 + 1

	SizeOf_Code_Header = 4
	SizeOf_Code_Entry = 12
	SizeOf_Code_Region = $4000
	SizeOf_Code_Table = (SizeOf_Code_Region - SizeOf_Code_Header)

Error_Code:
	REGION_SIZE_IS_NOT_LARGE_ENOUGH = -1
	NON_POSITIVE_SIZE = INVALID_SIZE
	MESSAGE_QUEUE_FULL = -2
	MESSAGE_QUEUE_EMPTY = -3