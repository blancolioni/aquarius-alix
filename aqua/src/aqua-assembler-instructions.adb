package body Aqua.Assembler.Instructions is

   ------------------------------------
   -- Create_Branch_Instruction_Word --
   ------------------------------------

   function Create_Branch_Instruction_Word
     (Mnemonic : String;
      Dst      : Word)
      return Word
   is
   begin
      return Aqua.Architecture.Encode_Branch
        (Aqua.Architecture.Branch_Instruction'Value ("A_" & Mnemonic),
         Dst);
   end Create_Branch_Instruction_Word;

   -----------------------------
   -- Create_Instruction_Word --
   -----------------------------

   function Create_Instruction_Word
     (Mnemonic : String;
      Src      : Aqua.Architecture.Operand_Type;
      Dst      : Aqua.Architecture.Operand_Type)
      return Word
   is
      use Aqua.Architecture;
   begin
      return Encode (Double_Operand_Instruction'Value ("A_" & Mnemonic),
                     Src, Dst);
   end Create_Instruction_Word;

   -----------------------------
   -- Create_Instruction_Word --
   -----------------------------

   function Create_Instruction_Word
     (Mnemonic : String;
      Dst      : Aqua.Architecture.Operand_Type)
      return Word
   is
      use Aqua.Architecture;
   begin
      return Encode
        (Single_Operand_Instruction'Value ("A_" & Mnemonic),
         Dst);
   end Create_Instruction_Word;

   -----------------------------
   -- Create_Instruction_Word --
   -----------------------------

   function Create_Instruction_Word
     (Mnemonic : String)
      return Word
   is
      use Aqua.Architecture;
   begin
      return Encode
        (No_Operand_Instruction'Value ("A_" & Mnemonic));
   end Create_Instruction_Word;

   function Create_Property_Instruction_Word
     (Mnemonic      : String;
      Property_Name : Word)
      return Word
   is
      use Aqua.Architecture;
   begin
      return Encode_Property
        (Property_Instruction'Value ("A_" & Mnemonic),
         Property_Name);
   end Create_Property_Instruction_Word;

   ----------------------------------
   -- Create_Trap_Instruction_Word --
   ----------------------------------

   function Create_Trap_Instruction_Word
     (Trap : Natural)
      return Word
   is
   begin
      return Aqua.Architecture.Encode_Trap (Trap);
   end Create_Trap_Instruction_Word;

end Aqua.Assembler.Instructions;
