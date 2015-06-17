package body Aqua.Memory is

   --------------
   -- Get_Byte --
   --------------

   overriding function Get_Byte
     (Memory : Memory_Type;
      Addr   : Address)
      return Byte
   is
   begin
      return Memory.Mem (Addr);
   end Get_Byte;

   --------------
   -- Set_Byte --
   --------------

   overriding procedure Set_Byte
     (Memory : in out Memory_Type;
      Addr   : Address;
      Value  : Byte)
   is
   begin
      Memory.Mem (Addr) := Value;
   end Set_Byte;

end Aqua.Memory;
