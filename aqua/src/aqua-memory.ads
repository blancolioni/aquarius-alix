package Aqua.Memory is

   type Memory_Type is new Memory_Interface with private;

   overriding function Get_Byte (Memory : Memory_Type;
                                 Addr   : Address)
                                 return Byte;

   overriding procedure Set_Byte (Memory : in out Memory_Type;
                                  Addr   : Address;
                                  Value  : Byte);

private

   type Memory_Array is array (Address) of Byte;

   type Memory_Type is new Memory_Interface with
      record
         Mem : Memory_Array;
      end record;

end Aqua.Memory;
