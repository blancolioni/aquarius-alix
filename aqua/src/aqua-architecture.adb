package body Aqua.Architecture is

   procedure Set_Instruction
     (W           : in out Word;
      Instruction : Aqua_Instruction);

   procedure Set_Operands
     (Instruction : in out Word;
      Source      : Operand_Type;
      Destination : Operand_Type);

   procedure Set_Operand
     (Instruction : in out Word;
      Destination : Operand_Type);

   ------------
   -- Encode --
   ------------

   function Encode
     (Instruction : No_Operand_Instruction)
      return Word
   is
      Result : Word := 0;
   begin
      Set_Instruction (Result, Instruction);
      return Result;
   end Encode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Instruction : Single_Operand_Instruction;
      Operand     : Operand_Type)
      return Word
   is
      Result : Word := 0;
   begin
      Set_Instruction (Result, Instruction);
      Set_Operand (Result, Operand);
      return Result;
   end Encode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Instruction : Double_Operand_Instruction;
      Src, Dst    : Operand_Type)
      return Word
   is
      Result : Word := 0;
   begin
      Set_Instruction (Result, Instruction);
      Set_Operands (Result, Src, Dst);
      return Result;
   end Encode;

   -------------------
   -- Encode_Branch --
   -------------------

   function Encode_Branch
     (Instruction : Branch_Instruction;
      Offset      : Word)
      return Word
   is
      Result : Word := Offset;
   begin
      Set_Instruction (Result, Instruction);
      return Result;
   end Encode_Branch;

   -------------------------
   -- Encode_Get_Property --
   -------------------------

   function Encode_Property
     (Instruction   : Property_Instruction;
      Property_Name : Word)
      return Word
   is
      Result : Word := Property_Name;
   begin
      Set_Instruction (Result, Instruction);
      return Result;
   end Encode_Property;

   -----------------
   -- Encode_Trap --
   -----------------

   function Encode_Trap
     (Trap        : Natural)
      return Word
   is
      Result : Word := Word (Trap);
   begin
      Set_Instruction (Result, A_Trap);
      return Result;
   end Encode_Trap;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address
     (Operand : Operand_Type;
      R       : in out Registers;
      Memory  : in out Memory_Interface'Class)
      return Address
   is
      Result : Address;
   begin
      case Operand.Mode is
         when Register =>
            if Operand.Deferred then
               return Get_Address (R (Operand.Register));
            else
               raise Program_Error
                 with "cannot get address of register mode";
            end if;
         when Autoincrement =>
            Result := Get_Address (R (Operand.Register));

            R (Operand.Register) := R (Operand.Register) + 4;
         when Autodecrement =>
            R (Operand.Register) := R (Operand.Register) - 4;
            Result := Get_Address (R (Operand.Register));
         when Indexed =>
            Result := Get_Address (R (Operand.Register));
            declare
               A : constant Word :=
                     Memory.Get_Word (Get_Address (R (R_PC)));
            begin
               if Is_Integer (A) then
                  declare
                     I : constant Aqua_Integer := Get_Integer (A);
                  begin
                     if I < 0 then
                        Result := Result - Address (abs I);
                     else
                        Result := Result + Address (I);
                     end if;
                  end;
               else
                  Result := Result + Get_Address (A);
               end if;
            end;
            R (R_PC) := R (R_PC) + 4;
      end case;

      if Operand.Deferred then
         Result := Get_Address (Memory.Get_Word (Result));
      end if;

      return Result;
   end Get_Address;

   -----------------------------
   -- Get_Destination_Operand --
   -----------------------------

   function Get_Destination_Operand
     (Instruction : Word)
      return Operand_Type
   is
   begin
      return Operand : Operand_Type do
         Operand.Register :=
           Register_Index (Get_Bits (Instruction, 15, 8));
         Operand.Deferred := Boolean'Val (Get_Bits (Instruction, 3, 1));
         Operand.Mode := Addressing_Mode'Val (Get_Bits (Instruction, 2, 3));
      end return;
   end Get_Destination_Operand;

   ------------------------
   -- Get_Source_Operand --
   ------------------------

   function Get_Source_Operand
     (Instruction : Word)
      return Operand_Type
   is
   begin
      return Operand : Operand_Type do
         Operand.Register :=
           Register_Index (Get_Bits (Instruction, 23, 8));
         Operand.Deferred := Boolean'Val (Get_Bits (Instruction, 7, 1));
         Operand.Mode := Addressing_Mode'Val (Get_Bits (Instruction, 6, 3));
      end return;
   end Get_Source_Operand;

   ----------
   -- Read --
   ----------

   procedure Read
     (Operand   : Operand_Type;
      R      : in out Registers;
      Memory : in out Memory_Interface'Class;
      Value  :    out Word)
   is
   begin
      if Operand.Mode = Register
        and then not Operand.Deferred
      then
         Value := R (Operand.Register);
      else
         declare
            A : constant Address := Get_Address (Operand, R, Memory);
         begin
            Value := Memory.Get_Word (A);
         end;
      end if;
   end Read;

   ---------------------
   -- Set_Instruction --
   ---------------------

   procedure Set_Instruction
     (W           : in out Word;
      Instruction : Aqua_Instruction)
   is
   begin
      W := (W and 16#00FF_FFFF#) or
        2 ** 24 * Aqua_Instruction'Pos (Instruction);
   end Set_Instruction;

   -----------------
   -- Set_Operand --
   -----------------

   procedure Set_Operand
     (Instruction : in out Word;
      Destination : Operand_Type)
   is
   begin
      Set_Operands (Instruction, (Register, False, 0), Destination);
   end Set_Operand;

   ------------------
   -- Set_Operands --
   ------------------

   procedure Set_Operands
     (Instruction : in out Word;
      Source      : Operand_Type;
      Destination : Operand_Type)
   is
      R_Src  : constant Word := Word (Source.Register);
      R_Dst  : constant Word := Word (Destination.Register);
      Op_Src : constant Word :=
                 Boolean'Pos (Source.Deferred) * 8
                 + Addressing_Mode'Pos (Source.Mode);
      Op_Dst : constant Word :=
                 Boolean'Pos (Destination.Deferred) * 8
                 + Addressing_Mode'Pos (Destination.Mode);
   begin
      Instruction := (Instruction and 16#FF00_0000#)
        + R_Src * 2 ** 16
        + R_Dst * 2 ** 8
        + Op_Src * 2 ** 4
        + Op_Dst;
   end Set_Operands;

   ------------
   -- Update --
   ------------

   procedure Update
     (Operand   : Operand_Type;
      R      : in out Registers;
      Memory : in out Memory_Interface'Class;
      Fn     : not null access
        function (X : Word) return Word)
   is
   begin
      if Operand.Mode = Register
        and then not Operand.Deferred
      then
         R (Operand.Register) := Fn (R (Operand.Register));
      else
         declare
            Addr : constant Address := Get_Address (Operand, R, Memory);
         begin
            Memory.Set_Word (Addr, Fn (Memory.Get_Word (Addr)));
         end;
      end if;
   end Update;

   -----------
   -- Write --
   -----------

   procedure Write
     (Operand   : Operand_Type;
      R      : in out Registers;
      Memory : in out Memory_Interface'Class;
      Value  : Word)
   is
   begin
      if Operand.Mode = Register
        and then not Operand.Deferred
      then
         R (Operand.Register) := Value;
      else
         declare
            A : constant Address := Get_Address (Operand, R, Memory);
         begin
            Memory.Set_Word (A, Value);
         end;
      end if;
   end Write;

end Aqua.Architecture;
