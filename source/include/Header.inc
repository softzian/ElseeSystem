True = 1
False = 0

IVideo = $100600
IVideo.Write_Telex = IVideo + 4
IVideo.Clear_Screen = IVideo + 8
IVideo.Set_Cursor = IVideo + 12
IVideo.Get_Cursor = IVideo + 16
IVideo.New_Line = IVideo + 20
IVideo.Move_Cursor = IVideo + 24

IMemory = $100200
IMemory.Create_Region = IMemory + 4
IMemory.Allocate = IMemory + 8
IMemory.Deallocate = IMemory + 12
IMemory.Mark_Memory = IMemory + 16

IModule = $100000
IModule.Add_Module = IModule + 4
IModule.Set_Active_Module = IModule + 8
IModule.Get_Active_Module = IModule + 12
IModule.Set_Module_Table = IModule + 16
