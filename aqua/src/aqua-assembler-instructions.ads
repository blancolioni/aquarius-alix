with Aqua.Architecture;

package Aqua.Assembler.Instructions is

   function Create_Instruction_Word
     (Mnemonic : String;
      Src      : Aqua.Architecture.Operand_Type;
      Dst      : Aqua.Architecture.Operand_Type)
      return Word;

   function Create_Instruction_Word
     (Mnemonic : String;
      Dst      : Aqua.Architecture.Operand_Type)
      return Word;

   function Create_Instruction_Word
     (Mnemonic : String)
      return Word;

   function Create_Branch_Instruction_Word
     (Mnemonic : String;
      Dst      : Word)
      return Word;

   function Create_Property_Instruction_Word
     (Mnemonic      : String;
      Property_Name : Word)
      return Word;

   function Create_Trap_Instruction_Word
     (Trap : Natural)
      return Word;

end Aqua.Assembler.Instructions;
