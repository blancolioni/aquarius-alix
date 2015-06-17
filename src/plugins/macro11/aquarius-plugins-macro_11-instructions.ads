package Aquarius.Plugins.Macro_11.Instructions is

   type Instruction_Class is (Double_Operand, Single_Operand, Zero_Operand,
                              Branch, Jump, Trap);

   type Instruction_Info is
      record
         Class  : Instruction_Class;
         Base   : Word;
         Mode_1 : Bit_Index;
         Mode_2 : Bit_Index;
         Cond   : Bit_Index;
         Reg    : Bit_Index;
      end record;

   function Get_Instruction_Info
     (Mnemonic : String)
      return Instruction_Info;

end Aquarius.Plugins.Macro_11.Instructions;
