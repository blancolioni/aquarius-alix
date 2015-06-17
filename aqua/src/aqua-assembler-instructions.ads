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

   function Create_Branch_Instruction_Word
     (Mnemonic : String;
      Dst      : Word)
      return Word;

   function Create_Jump_Instruction
      (Dst      : Aqua.Architecture.Operand_Type)
      return Word;

   function Create_Jsr_Instruction
     (Register : Aqua.Architecture.Register_Index;
      Dst      : Aqua.Architecture.Operand_Type)
      return Word;

   function Create_Return_Instruction_Word
     (R : Aqua.Architecture.Register_Index)
      return Word;

   function Create_Trap_Instruction_Word
     (Trap : Natural)
      return Word;

end Aqua.Assembler.Instructions;
