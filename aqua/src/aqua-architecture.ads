package Aqua.Architecture is

   type Register_Index is mod 8;

   type Registers is array (Register_Index) of Word;

   type Addressing_Mode is (Register, Autoincrement, Autodecrement, Indexed);

   type Operand_Type is
      record
         Mode     : Addressing_Mode;
         Deferred : Boolean;
         Register : Register_Index;
      end record;

--     function Get_Register (Name : String) return Register_Index;

   function Get_Address
     (Operand : Operand_Type;
      R       : in out Registers;
      Memory  : in out Memory_Interface'Class)
      return Address;

   function Get_Bits
     (Operand : Operand_Type;
      Shift   : Bit_Index)
      return Word;

   function Get_Operand
     (Value : Word;
      Start : Bit_Index)
      return Operand_Type;

   procedure Read
     (Operand : Operand_Type;
      R      : in out Registers;
      Memory : in out Memory_Interface'Class;
      Value  :    out Word);

   procedure Write
     (Operand : Operand_Type;
      R      : in out Registers;
      Memory : in out Memory_Interface'Class;
      Value  : Word);

   procedure Update
     (Operand : Operand_Type;
      R      : in out Registers;
      Memory : in out Memory_Interface'Class;
      Fn     : not null access
        function (X : Word) return Word);

end Aqua.Architecture;
