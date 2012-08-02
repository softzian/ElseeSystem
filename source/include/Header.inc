True = 1
False = 0

IInterrupt = $100200
IInterrupt.Install_ISR = IInterrupt + 4
IInterrupt.Init_PIC = IInterrupt + 8
IInterrupt.Mask_all_IRQ = IInterrupt + 12
IInterrupt.Enable_IRQ = IInterrupt + 16
IInterrupt.Send_EOI = IInterrupt + 20

IKeyboard = $100400
IKeyboard.Init_Keyboard = IKeyboard + 4
IKeyboard.Read = IKeyboard + 8
IKeyboard.Clear_Keyboard_Buffer = IKeyboard + 12

IVideo = $100600
IVideo.Write_Telex = IVideo + 4
IVideo.Clear_Display = IVideo + 8
IVideo.Set_Cursor = IVideo + 12
IVideo.Get_Cursor = IVideo + 16
IVideo.New_Line = IVideo + 20

IUtility = $100A00
IUtility.Unsigned_to_HexStr1 = IUtility + 4
IUtility.Write_Unsigned_Hex = IUtility + 8
IUtility.Write_Char = IUtility + 12
IUtility.Write_String1 = IUtility + 16
IUtility.Create_Ring_Buffer = IUtility + 20
IUtility.Ring_Buffer_Read = IUtility + 24
IUtility.Ring_Buffer_Write = IUtility + 28
IUtility.Clear_Ring_Buffer = IUtility + 32
IUtility.Read_String1 = IUtility + 36
IUtility.Read_Char = IUtility + 40
IUtility.HexStr1_to_Unsigned = IUtility + 44
IUtility.HexChar_to_Byte = IUtility + 48

Format_Hex = 1