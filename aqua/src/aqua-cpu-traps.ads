package Aqua.CPU.Traps is

   procedure Handle_Get_Property
     (CPU   : in out Aqua_CPU_Type'Class);

   procedure Handle_Set_Property
     (CPU   : in out Aqua_CPU_Type'Class);

   procedure Handle_Iterator_Start
     (CPU   : in out Aqua_CPU_Type'Class);

   procedure Handle_Iterator_Next
     (CPU   : in out Aqua_CPU_Type'Class);

end Aqua.CPU.Traps;