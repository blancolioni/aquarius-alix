with Ada.Containers.Hashed_Maps;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Strings.Fixed.Equal_Case_Insensitive;

package body Aqua.Assembler.Instructions is

   subtype Mnemonic_String is String (1 .. 5);

   Got_Instructions : Boolean := False;

   type Instruction_Info is
      record
         Base      : Word;
         Operands  : Natural;
         Src_Shift : Bit_Index;
         Dst_Shift : Bit_Index;
      end record;

   package Info_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Mnemonic_String,
        Element_Type    => Instruction_Info,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   Info_Map : Info_Maps.Map;

   procedure Check_Instructions;

   function Get_Instruction_Info
     (Mnemonic : String)
      return Instruction_Info;

   ------------------------
   -- Check_Instructions --
   ------------------------

   procedure Check_Instructions is

      procedure Branch (CC   : String;
                        Base : Word);

      procedure Double (Mnemonic : String;
                        Op_Code  : Word);

      procedure Single (Mnemonic     : String;
                        Op_Code      : Word;
                        Byte_Version : Boolean := True);

      ------------
      -- Branch --
      ------------

      procedure Branch (CC   : String;
                        Base : Word)
      is
         Info : constant Instruction_Info :=
                  (Base      => Base * 64,
                   Operands  => 1,
                   Src_Shift => 0,
                   Dst_Shift => 0);
         Key : Mnemonic_String := (others => ' ');
      begin
         Key (CC'Range) := CC;
         Info_Map.Insert (Key, Info);
      end Branch;

      ------------
      -- Double --
      ------------

      procedure Double (Mnemonic : String;
                        Op_Code  : Word)
      is
         Key : Mnemonic_String := (others => ' ');
         Base : constant Word := Op_Code * 2 ** 12;
         Src_Shift : constant Bit_Index := 6;
         Dst_Shift : constant Bit_Index := 0;
         Info : Instruction_Info :=
                  (Base      => Base,
                   Operands  => 2,
                   Src_Shift => Src_Shift,
                   Dst_Shift => Dst_Shift);
      begin
         Key (Mnemonic'Range) := Mnemonic;

         Info_Map.Insert (Key, Info);
         Info.Base := Info.Base + 2 ** 15;
         if Mnemonic = "add" then
            Key (1 .. 3) := "sub";
         else
            Key (4) := 'b';
         end if;
         Info_Map.Insert (Key, Info);
      end Double;

      ------------
      -- Single --
      ------------

      procedure Single (Mnemonic     : String;
                        Op_Code      : Word;
                        Byte_Version : Boolean := True)
      is
         Key : Mnemonic_String := (others => ' ');
         Base : constant Word := Op_Code * 2 ** 6;
         Info : Instruction_Info :=
                  (Base      => Base,
                   Operands  => 1,
                   Src_Shift => 0,
                   Dst_Shift => 0);
      begin
         Key (Mnemonic'Range) := Mnemonic;
         Info_Map.Insert (Key, Info);

         if Byte_Version then
            Info.Base := Info.Base + 2 ** 15;
            if Mnemonic = "add" then
               Key (1 .. 3) := "sub";
            else
               Key (4) := 'b';
            end if;
            Info_Map.Insert (Key, Info);
         end if;

      end Single;
   begin
      if Got_Instructions then
         return;
      end if;

      Double ("mov", 1);
      Double ("cmp", 2);
      Double ("add", 6);

      Single ("clr", 8#0050#);
      Single ("com", 8#0051#);
      Single ("inc", 8#0052#);
      Single ("dec", 8#0053#);
      Single ("neg", 8#0054#);
      Single ("adc", 8#0055#);
      Single ("sbc", 8#0056#);
      Single ("tst", 8#0057#);

      Branch ("br",  8#0004#);
      Branch ("bne", 8#0010#);
      Branch ("beq", 8#0014#);
      Branch ("bge", 8#0020#);
      Branch ("blt", 8#0024#);
      Branch ("bgt", 8#0030#);
      Branch ("ble", 8#0034#);
      Branch ("bpl", 8#1000#);
      Branch ("bmi", 8#1004#);
      Branch ("bhi", 8#1010#);
      Branch ("blos", 8#1014#);

      Got_Instructions := True;
   end Check_Instructions;

   ------------------------------------
   -- Create_Branch_Instruction_Word --
   ------------------------------------

   function Create_Branch_Instruction_Word
     (Mnemonic : String;
      Dst      : Word)
      return Word
   is
   begin
      Check_Instructions;

      declare
         Info : constant Instruction_Info := Get_Instruction_Info (Mnemonic);
      begin
         return Info.Base + Dst;
      end;
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
   begin
      Check_Instructions;

      declare
         Info : constant Instruction_Info := Get_Instruction_Info (Mnemonic);
      begin
         return Info.Base
           + Aqua.Architecture.Get_Bits (Src, Info.Src_Shift)
           + Aqua.Architecture.Get_Bits (Dst, Info.Dst_Shift);
      end;
   end Create_Instruction_Word;

   -----------------------------
   -- Create_Instruction_Word --
   -----------------------------

   function Create_Instruction_Word
     (Mnemonic : String;
      Dst      : Aqua.Architecture.Operand_Type)
      return Word
   is
   begin

      Check_Instructions;

      declare
         Info : constant Instruction_Info := Get_Instruction_Info (Mnemonic);
      begin
         return Info.Base
           + Aqua.Architecture.Get_Bits (Dst, Info.Dst_Shift);
      end;

   end Create_Instruction_Word;

   ----------------------------
   -- Create_Jsr_Instruction --
   ----------------------------

   function Create_Jsr_Instruction
     (Register : Aqua.Architecture.Register_Index;
      Dst      : Aqua.Architecture.Operand_Type)
      return Word
   is
   begin
      return 8#004000# + Word (Register) *  2 ** 6
        + Aqua.Architecture.Get_Bits (Dst, 0);
   end Create_Jsr_Instruction;

   -----------------------------
   -- Create_Jump_Instruction --
   -----------------------------

   function Create_Jump_Instruction
      (Dst      : Aqua.Architecture.Operand_Type)
       return Word
   is
   begin
      return 8#000100# +
        Aqua.Architecture.Get_Bits (Dst, 0);
   end Create_Jump_Instruction;

   ------------------------------------
   -- Create_Return_Instruction_Word --
   ------------------------------------

   function Create_Return_Instruction_Word
     (R : Aqua.Architecture.Register_Index)
      return Word
   is
   begin
      return 8#000200# + Word (R);
   end Create_Return_Instruction_Word;

   ----------------------------------
   -- Create_Trap_Instruction_Word --
   ----------------------------------

   function Create_Trap_Instruction_Word
     (Trap : Natural)
      return Word
   is
   begin
      return 8#104000# + Word (Trap);
   end Create_Trap_Instruction_Word;

   --------------------------
   -- Get_Instruction_Info --
   --------------------------

   function Get_Instruction_Info
     (Mnemonic : String)
      return Instruction_Info
   is
      Key : Mnemonic_String := (others => ' ');
   begin
      Key (Mnemonic'Range) := Mnemonic;
      return Info_Map (Key);
   exception
      when Constraint_Error =>
         raise Constraint_Error
           with "Unknown (or unimplemented) instruction: "  & Mnemonic;
   end Get_Instruction_Info;

end Aqua.Assembler.Instructions;
