True = 1
False = 0

macro invoke x
{
	call dword [cs:x]
}

IVideo = $100800
IVideo.Write_Telex = IVideo + 4
IVideo.Clear_Screen = IVideo + 8
IVideo.Set_Cursor = IVideo + 12
IVideo.Get_Cursor = IVideo + 16
IVideo.New_Line = IVideo + 20
IVideo.Move_Cursor = IVideo + 24

ISystem = $100000
ISystem.Create_Region = ISystem + 4
ISystem.Allocate = ISystem + 8
ISystem.Deallocate = ISystem + 12
ISystem.Mark_Memory = ISystem + 16
ISystem.Create_Code_Region = ISystem + 20
ISystem.Allocate_Code = ISystem + 24
ISystem.Deallocate_Code = ISystem + 28
ISystem.Mark_Code = ISystem + 32
ISystem.Set_Active_Module = ISystem + 36
ISystem.Get_Active_Module = ISystem + 40

IUtility = $100A00
IUtility.Cardinal_to_HexStr = IUtility + 4
IUtility.Write_Cardinal_Hex = IUtility + 8
IUtility.Write_Char = IUtility + 12
IUtility.Write_String = IUtility + 16
IUtility.HexStr_to_Cardinal = IUtility + 44
IUtility.HexChar_to_Byte = IUtility + 48

IInterrupt = $100400
IInterrupt.Install_ISR = IInterrupt + 4
IInterrupt.Init_PIC = IInterrupt + 8
IInterrupt.Mask_all_IRQ = IInterrupt + 12
IInterrupt.Enable_IRQ = IInterrupt + 16
IInterrupt.Send_EOI = IInterrupt + 20

IThread = $100600
IThread.New_Thread = IThread + 4
IThread.Start = IThread + 8
IThread.Yield = IThread + 12
