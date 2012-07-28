IInterrupt = $100200
IInterrupt.Install_ISR = IInterrupt + 4
IInterrupt.Init_PIC = IInterrupt + 8
IInterrupt.Mask_all_IRQ = IInterrupt + 12
IInterrupt.Enable_IRQ = IInterrupt + 16
IInterrupt.Send_EOI = IInterrupt + 20

IVideo = $100600
IVideo.Write_Telex = IVideo + 4
IVideo.Clear_Display = IVideo + 8

ISysUtils = $100A00
ISysUtils.Byte_to_HexStr = ISysUtils + 4
ISysUtils.Write_Byte = ISysUtils + 8
ISysUtils.Write_Char = ISysUtils + 12

Format_Hex = 1