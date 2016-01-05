package Aqua.Architecture is

   type Register_Index is mod 256;

   type Registers is array (Register_Index) of Word;

   type Addressing_Mode is (Register, Autoincrement, Autodecrement, Indexed);

   R_PC : constant Register_Index := 255;
   R_SP : constant Register_Index := 254;
   R_FP : constant Register_Index := 253;
   R_OP : constant Register_Index := 252;
   R_PV : constant Register_Index := 251;

   type Condition_Code is
     (Always, EQ, LT, LE, MI, LOS, VS, CS);

   type Aqua_Instruction is
     (A_Halt, A_Rts,
      A_Clr, A_Dec, A_Inc, A_Jmp, A_Jsr, A_Neg, A_Not, A_Tst,
      A_Add, A_Bic, A_Bis, A_Bit, A_Cmp, A_Div, A_Mov, A_Mul, A_Sub,
      A_Nop, A_Br, A_Bne, A_Beq, A_Bge, A_Blt, A_Bgt, A_Ble, A_Bpl, A_Bmi,
      A_Bhi, A_Blos, A_Bvc, A_Bvs, A_Bcc, A_Bcs,
      A_Trap,
      A_Get_Property, A_Set_Property);

   subtype No_Operand_Instruction is Aqua_Instruction range A_Halt .. A_Nop;
   subtype Single_Operand_Instruction is Aqua_Instruction range A_Clr .. A_Tst;
   subtype Double_Operand_Instruction is Aqua_Instruction range A_Add .. A_Sub;
   subtype Branch_Instruction is Aqua_Instruction range A_Nop .. A_Bcs;
   subtype Property_Instruction is Aqua_Instruction range
     A_Get_Property .. A_Set_Property;

   type Operand_Type is
      record
         Mode     : Addressing_Mode;
         Deferred : Boolean;
         Register : Register_Index;
      end record;

   function Get_Instruction
     (Instruction : Word)
      return Aqua_Instruction
   is (Aqua_Instruction'Val (Instruction / 2 ** 24));

   function Get_Source_Operand
     (Instruction : Word)
      return Operand_Type;

   function Get_Destination_Operand
     (Instruction : Word)
      return Operand_Type;

   function Encode
     (Instruction : No_Operand_Instruction)
      return Word;

   function Encode
     (Instruction : Single_Operand_Instruction;
      Operand     : Operand_Type)
      return Word;

   function Encode
     (Instruction : Double_Operand_Instruction;
      Src, Dst    : Operand_Type)
      return Word;

   function Encode_Branch
     (Instruction : Branch_Instruction;
      Offset      : Word)
      return Word;

   function Encode_Trap
     (Trap        : Natural)
      return Word;

   function Encode_Property
     (Instruction   : Property_Instruction;
      Property_Name : Word)
      return Word;

   function Get_Address
     (Operand : Operand_Type;
      R       : in out Registers;
      Memory  : in out Memory_Interface'Class)
      return Address;

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
