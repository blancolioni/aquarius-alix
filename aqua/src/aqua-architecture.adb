package body Aqua.Architecture is

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

            R (Operand.Register) := R (Operand.Register) + 2;
         when Autodecrement =>
            R (Operand.Register) := R (Operand.Register) - 2;
            Result := Get_Address (R (Operand.Register));
         when Indexed =>
            Result := Get_Address (R (Operand.Register));
            declare
               A : constant Word :=
                     Memory.Get_Word (Get_Address (R (7)));
            begin
               Result := Result + Get_Address (A);
            end;
            R (7) := R (7) + 2;
      end case;

      if Operand.Deferred then
         Result := Get_Address (Memory.Get_Word (Result));
      end if;

      return Result;
   end Get_Address;

   --------------
   -- Get_Bits --
   --------------

   function Get_Bits
     (Operand : Operand_Type;
      Shift   : Bit_Index)
      return Word
   is
      Result : constant Word :=
                 Word (Operand.Register)
                 + (if Operand.Deferred then 8 else 0)
                 + Addressing_Mode'Pos (Operand.Mode) * 16;
   begin
      return Result * 2 ** Natural (Shift);
   end Get_Bits;

   -----------------
   -- Get_Operand --
   -----------------

   function Get_Operand
     (Value : Word;
      Start : Bit_Index)
      return Operand_Type
   is
   begin
      return Result : Operand_Type do
         Result.Mode :=
           Addressing_Mode'Val
             (Get_Bits (Value, Start, 2));
         Result.Deferred :=
           Get_Bits (Value, Start - 2, 1) = 1;
         Result.Register :=
           Register_Index (Get_Bits (Value, Start - 3, 3));
      end return;
   end Get_Operand;

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
