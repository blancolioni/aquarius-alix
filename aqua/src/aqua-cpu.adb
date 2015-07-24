with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Unbounded;

with Ada.Text_IO;

with Aqua.Arithmetic;
with Aqua.CPU.Traps;
with Aqua.IO;
with Aqua.Objects;
with Aqua.Primitives;
with Aqua.Traps;

package body Aqua.CPU is

   Trace_Properties : constant Boolean := False;
   Trace_Code       : constant Boolean := False;
   Trace_Stack      : constant Boolean := False;

   Current_Output : Ada.Text_IO.File_Type;

   type Condition_Code is
     (Always, EQ, LT, LE, MI, LOS, VS, CS);

   type Double_Operand_Handler is access
     procedure (CPU       : in out Aqua_CPU_Type'Class;
                Byte      : Boolean;
                Src, Dst  : Aqua.Architecture.Operand_Type);

   type Extended_Double_Operand_Handler is access
     procedure (CPU      : in out Aqua_CPU_Type'Class;
                Src      : Aqua.Architecture.Operand_Type;
                R        : Aqua.Architecture.Register_Index);

   type Single_Operand_Handler is access
     procedure (CPU     : in out Aqua_CPU_Type'Class;
                Byte    : Boolean;
                Operand : Aqua.Architecture.Operand_Type);

   procedure Handle_Mov
     (CPU      : in out Aqua_CPU_Type'Class;
      Byte     : Boolean;
      Src, Dst : Aqua.Architecture.Operand_Type);

   procedure Handle_Cmp
     (CPU      : in out Aqua_CPU_Type'Class;
      Byte     : Boolean;
      Src, Dst : Aqua.Architecture.Operand_Type);

   procedure Handle_Add
     (CPU      : in out Aqua_CPU_Type'Class;
      Byte     : Boolean;
      Src, Dst : Aqua.Architecture.Operand_Type);

   procedure Handle_Clr
     (CPU     : in out Aqua_CPU_Type'Class;
      Byte    : Boolean;
      Operand : Aqua.Architecture.Operand_Type);

   procedure Handle_Dec
     (CPU     : in out Aqua_CPU_Type'Class;
      Byte    : Boolean;
      Operand : Aqua.Architecture.Operand_Type);

   procedure Handle_Inc
     (CPU     : in out Aqua_CPU_Type'Class;
      Byte    : Boolean;
      Operand : Aqua.Architecture.Operand_Type);

   procedure Handle_Tst
     (CPU     : in out Aqua_CPU_Type'Class;
      Byte    : Boolean;
      Operand : Aqua.Architecture.Operand_Type);

   procedure Handle_Mul (CPU      : in out Aqua_CPU_Type'Class;
                         Src      : Aqua.Architecture.Operand_Type;
                         R        : Aqua.Architecture.Register_Index);

   Double_Operand : constant array (Word range 1 .. 6)
     of Double_Operand_Handler :=
       (1      => Handle_Mov'Access,
        2      => Handle_Cmp'Access,
        6      => Handle_Add'Access,
        others => null);

   Extended_Double_Operand : constant array (Word range 0 .. 7)
     of Extended_Double_Operand_Handler :=
       (0      => Handle_Mul'Access,
        others => null);

   Single_Operand : constant array (Word range 8 .. 23)
     of Single_Operand_Handler :=
       (8  => Handle_Clr'Access,
        10 => Handle_Inc'Access,
        11 => Handle_Dec'Access,
        15 => Handle_Tst'Access,
        others => null);

   procedure Handle
     (CPU : in out Aqua_CPU_Type'Class;
      Op  : in     Word);

   procedure Handle_Branch
     (CPU         : in out Aqua_CPU_Type'Class;
      Condition   : Condition_Code;
      Negate      : Boolean;
      Offset      : Word);

   procedure Handle_Jump
     (CPU     : in out Aqua_CPU_Type'Class;
      Operand : Aqua.Architecture.Operand_Type);

   procedure Handle_Jsr
     (CPU      : in out Aqua_CPU_Type'Class;
      Register : Aqua.Architecture.Register_Index;
      Operand  : Aqua.Architecture.Operand_Type);

   procedure Handle_Trap
     (CPU   : in out Aqua_CPU_Type'Class;
      Index : Natural);

   procedure Set_NZ
     (CPU   : in out Aqua_CPU_Type'Class;
      Value : Word);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (CPU       : in out Aqua_CPU_Type;
      Start     : Address;
      Arguments : Array_Of_Words)
   is
      use type Ada.Calendar.Time;
      use Aqua.Arithmetic;
   begin

      if Trace_Code then
         Ada.Text_IO.Put_Line ("start: "
                               & CPU.Name (Arguments (2))
                               & (if Arguments'Length > 2
                                 then " " & CPU.Name (Arguments (3))
                                 else ""));
      end if;

      CPU.Start := Ada.Calendar.Clock;
      CPU.Push (CPU.R (7));

      for Arg of reverse Arguments loop
         CPU.Push (Arg);
      end loop;

      CPU.Push (0);

      CPU.R (7) := To_Address_Word (Start);
      CPU.B := False;

      while not CPU.B
        and then CPU.R (7) /= 0
      loop
         declare
            Op : constant Word :=
                   CPU.Image.Get_Word (Get_Address (CPU.R (7)));
         begin
            if Trace_Code then
               Ada.Text_IO.Put
                 (Aqua.IO.Hex_Image (Get_Address (CPU.R (5)))
                  & " "
                  & Aqua.IO.Hex_Image (Get_Address (CPU.R (6)))
                  & " "
                  & Aqua.IO.Hex_Image (Get_Address (CPU.R (7)))
                  & ": ");

               Ada.Text_IO.Put
                 (Aqua.IO.Octal_Image (Op));

            end if;
            Inc (CPU.R (7), 2);
            Handle (CPU, Op);
            if Trace_Code then
               Ada.Text_IO.New_Line;
            end if;
         end;
      end loop;

      Aqua.Arithmetic.Inc (CPU.R (6), 2 * Arguments'Length);
      CPU.R (7) := CPU.Pop;

      CPU.Exec_Time := CPU.Exec_Time + Ada.Calendar.Clock - CPU.Start;

   end Execute;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (CPU : in out Aqua_CPU_Type)
   is
   begin
      null;
   end Finalize;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (CPU : in out Aqua_CPU_Type'Class;
      Op  : in     Word)
   is
      use Aqua.Architecture;
      Double_Opcode : constant Word := Get_Bits (Op, 14, 3);
      Single_Opcode : constant Word := Get_Bits (Op, 10, 5);
      Byte          : constant Boolean := Get_Bits (Op, 15, 1) = 1;

      CC_Code       : constant Word :=
                        Get_Bits (Op, 15, 1) * 8
                        + Get_Bits (Op, 10, 3);
   begin
      if Op = 0 then
         CPU.B := True;
      elsif Op = 8#000240# then
         null;
      elsif (Op and 8#177000#) = 8#104000# then
         Handle_Trap (CPU, Natural (Op mod 512));
      elsif (Op and 8#177770#) = 8#000200# then
         declare
            R : constant Register_Index :=
                  Register_Index (Op mod 8);
         begin
            if R /= 7 then
               CPU.R (7) := CPU.R (R);
            end if;
            CPU.R (R) := CPU.Pop;
         end;
      elsif (Op and 8#177000#) = 8#004000# then
         Handle_Jsr (CPU, Register_Index (Get_Bits (Op, 8, 3)),
                     Get_Operand (Op, 5));
      elsif (Op and 8#074000#) = 8#004000# then
         Single_Operand (Single_Opcode)
           (CPU, Byte, Get_Operand (Op, 5));
      elsif Double_Opcode in 1 .. 6 then
         Double_Operand (Double_Opcode)
           (CPU, Byte, Get_Operand (Op, 11), Get_Operand (Op, 5));
      elsif (Op and 8#177700#) = 8#000100# then
         Handle_Jump (CPU, Get_Operand (Op, 5));
      elsif Get_Bits (Op, 14, 4) = 0
        and then CC_Code /= 0
      then
         Handle_Branch (CPU, Condition_Code'Val (CC_Code / 2),
                        CC_Code mod 2 = 0,
                        Op mod 256);
      elsif (Op and 8#170000#) = 8#070000# then
         declare
            Opcode : constant Word := Get_Bits (Op, 11, 3);
         begin
            Extended_Double_Operand (Opcode)
              (CPU, Get_Operand (Op, 8),
               Register_Index (Get_Bits (Op, 2, 3)));
         end;
      else
         raise Constraint_Error
           with "unimplemented instruction "
             & Aqua.IO.Octal_Image (Op);
      end if;
   end Handle;

   ----------------
   -- Handle_Add --
   ----------------

   procedure Handle_Add
     (CPU      : in out Aqua_CPU_Type'Class;
      Byte     : Boolean;
      Src, Dst : Aqua.Architecture.Operand_Type)
   is
      X, Y, Z : Aqua_Integer;
      T       : Word;

      function Update (W : Word) return Word;

      ------------
      -- Update --
      ------------

      function Update (W : Word) return Word is
      begin
         Y := CPU.To_Integer (W);
         if Byte then
            Z := Y - X;
         else
            Z := Y + X;
         end if;
         Set_NZ (CPU, To_Integer_Word (Z));
         return To_Integer_Word (Z);
      end Update;

   begin

      Aqua.Architecture.Read
        (Src, CPU.R, CPU.Image.all, T);

      X := CPU.To_Integer (T);

      Aqua.Architecture.Update
        (Dst, CPU.R, CPU.Image.all, Update'Access);

   end Handle_Add;

   -------------------
   -- Handle_Branch --
   -------------------

   procedure Handle_Branch
     (CPU         : in out Aqua_CPU_Type'Class;
      Condition   : Condition_Code;
      Negate      : Boolean;
      Offset      : Word)
   is
      Branch : Boolean;
   begin
      case Condition is
         when Always =>
            Branch := True;
         when EQ =>
            Branch := CPU.Z;
         when LT =>
            Branch := CPU.N or else CPU.V;
         when LE =>
            Branch := not (CPU.N xor CPU.V);
         when MI =>
            Branch := CPU.N;
         when LOS =>
            Branch := CPU.C or else CPU.Z;
         when VS =>
            Branch := CPU.V;
         when CS =>
            Branch := CPU.C;
      end case;
      if Negate then
         Branch := not Branch;
      end if;

      if Branch then
         if Offset < 128 then
            Aqua.Arithmetic.Inc (CPU.R (7), Integer (Offset * 2));
         else
            Aqua.Arithmetic.Dec (CPU.R (7), Integer ((256 - Offset) * 2));
         end if;

         if Trace_Code then
            Ada.Text_IO.Put_Line
              ("branch: " & CPU.Show (CPU.R (7)));
         end if;

      end if;

   end Handle_Branch;

   ----------------
   -- Handle_Clr --
   ----------------

   procedure Handle_Clr
     (CPU     : in out Aqua_CPU_Type'Class;
      Byte    : Boolean;
      Operand : Aqua.Architecture.Operand_Type)
   is
      pragma Unreferenced (Byte);
   begin
      Aqua.Architecture.Write
        (Operand => Operand,
         R       => CPU.R,
         Memory  => CPU.Image.all,
         Value   => 0);
      Set_NZ (CPU, 0);
   end Handle_Clr;

   ----------------
   -- Handle_Cmp --
   ----------------

   procedure Handle_Cmp
     (CPU      : in out Aqua_CPU_Type'Class;
      Byte      : Boolean;
      Src, Dst : Aqua.Architecture.Operand_Type)
   is
      pragma Unreferenced (Byte);
      X, Y : Word;
   begin
      Aqua.Architecture.Read
        (Src, CPU.R, CPU.Image.all, X);
      Aqua.Architecture.Read
        (Dst, CPU.R, CPU.Image.all, Y);

      if Is_Integer (X) and then Is_Integer (Y) then
         Set_NZ (CPU, X - Y);
      elsif Is_String_Reference (X) and then Is_String_Reference (Y) then
         --  unfortunately, if one of the operands is a string defined
         --  in a source file, while the other is a generated string,
         --  they will end up with different string references, so
         --  we have to use a string compare as a backup
         CPU.N := False;
         CPU.Z := X = Y
           or else Ada.Strings.Fixed.Equal_Case_Insensitive
             (CPU.Show (X), CPU.Show (Y));
      else
         CPU.N := False;
         CPU.Z := X = Y;
      end if;

      if Trace_Code then
         Ada.Text_IO.Put_Line
           ("cmp: " & CPU.Show (X) & "/" & Aqua.IO.Hex_Image (X)
            & " = " & CPU.Show (Y) & "/" & Aqua.IO.Hex_Image (Y)
            & " -> " & Boolean'Image (CPU.Z));
      end if;
   end Handle_Cmp;

   ----------------
   -- Handle_Dec --
   ----------------

   procedure Handle_Dec
     (CPU     : in out Aqua_CPU_Type'Class;
      Byte    : Boolean;
      Operand : Aqua.Architecture.Operand_Type)
   is
      pragma Unreferenced (Byte);

      function Update (W : Word) return Word;

      ------------
      -- Update --
      ------------

      function Update (W : Word) return Word is
         X : Word := W;
      begin
         Aqua.Arithmetic.Dec (X, 1);
         Set_NZ (CPU, X);
         return X;
      end Update;

   begin
      Aqua.Architecture.Update (Operand, CPU.R, CPU.Image.all,
                                Update'Access);
   end Handle_Dec;

   ----------------
   -- Handle_Inc --
   ----------------

   procedure Handle_Inc
     (CPU     : in out Aqua_CPU_Type'Class;
      Byte    : Boolean;
      Operand : Aqua.Architecture.Operand_Type)
   is
      pragma Unreferenced (Byte);

      function Update (W : Word) return Word;

      ------------
      -- Update --
      ------------

      function Update (W : Word) return Word is
         X : Word := W;
      begin
         Aqua.Arithmetic.Inc (X, 1);
         Set_NZ (CPU, X);
         return X;
      end Update;

   begin
      Aqua.Architecture.Update (Operand, CPU.R, CPU.Image.all,
                                Update'Access);
   end Handle_Inc;

   ----------------
   -- Handle_Jsr --
   ----------------

   procedure Handle_Jsr
     (CPU      : in out Aqua_CPU_Type'Class;
      Register : Aqua.Architecture.Register_Index;
      Operand  : Aqua.Architecture.Operand_Type)
   is
      T : constant Address :=
            Aqua.Architecture.Get_Address
              (Operand, CPU.R, CPU.Image.all);
   begin
      CPU.Push (CPU.R (Register));
      CPU.R (Register) := CPU.R (7);
      CPU.R (7) := To_Address_Word (T);
   end Handle_Jsr;

   -----------------
   -- Handle_Jump --
   -----------------

   procedure Handle_Jump
     (CPU     : in out Aqua_CPU_Type'Class;
      Operand : Aqua.Architecture.Operand_Type)
   is
   begin
      CPU.R (7) :=
        To_Address_Word
          (Aqua.Architecture.Get_Address
             (Operand => Operand,
              R       => CPU.R,
              Memory  => CPU.Image.all));
   end Handle_Jump;

   ----------------
   -- Handle_Mov --
   ----------------

   procedure Handle_Mov
     (CPU      : in out Aqua_CPU_Type'Class;
      Byte     : Boolean;
      Src, Dst : Aqua.Architecture.Operand_Type)
   is
      pragma Unreferenced (Byte);
      X : Word;
   begin
      Aqua.Architecture.Read
        (Src, CPU.R, CPU.Image.all, X);
      if Trace_Code then
         Ada.Text_IO.Put_Line ("mov: " & CPU.Show (X));
      end if;

      Aqua.Architecture.Write
        (Dst, CPU.R, CPU.Image.all, X);
   end Handle_Mov;

   ----------------
   -- Handle_Mul --
   ----------------

   procedure Handle_Mul (CPU      : in out Aqua_CPU_Type'Class;
                         Src      : Aqua.Architecture.Operand_Type;
                         R        : Aqua.Architecture.Register_Index)
   is
      Src_Word : Word;
      X, Y     : Aqua_Integer;
   begin
      Aqua.Architecture.Read
        (Src, CPU.R, CPU.Image.all, Src_Word);
      X := CPU.To_Integer (Src_Word);
      Y := CPU.To_Integer (CPU.R (R));
      CPU.R (R) := To_Integer_Word (X * Y);
   end Handle_Mul;

   -----------------
   -- Handle_Trap --
   -----------------

   procedure Handle_Trap
     (CPU   : in out Aqua_CPU_Type'Class;
      Index : Natural)
   is
   begin

      if Trace_Code then
         Ada.Text_IO.Put_Line ("trap" & Natural'Image (Index));
      end if;

      case Index is
         when Aqua.Traps.Allocate =>
            declare
               New_Item : constant External_Object_Access :=
                            new Aqua.Objects.Root_Object_Type;
            begin
               CPU.Ext.Append (New_Item);
               CPU.Push
                 (To_External_Word
                    (External_Reference
                         (CPU.Ext.Last_Index)));
            end;
         when Aqua.Traps.Join_Strings =>
            declare
               Right_Word : constant Word := CPU.Pop;
               Left_Word  : constant Word := CPU.Pop;
               pragma Assert (Is_Integer (Right_Word)
                              or else Is_String_Reference (Right_Word));
               pragma Assert (Is_Integer (Right_Word)
                              or else Is_String_Reference (Left_Word));
               Result     : constant String :=
                              CPU.To_String (Left_Word)
                            & CPU.To_String (Right_Word);
            begin
               CPU.Push
                 (CPU.To_String_Word (Result));
            end;
         when Aqua.Traps.Property_Get =>

            Aqua.CPU.Traps.Handle_Get_Property (CPU);

         when Aqua.Traps.Property_Set =>
            Aqua.CPU.Traps.Handle_Set_Property (CPU);

         when Aqua.Traps.Iterator_Start =>
            Aqua.CPU.Traps.Handle_Iterator_Start (CPU);

         when Aqua.Traps.Iterator_Next =>
            Aqua.CPU.Traps.Handle_Iterator_Next (CPU);

         when Aqua.Traps.IO_Put_String =>
            declare
               W : constant Word := CPU.Pop;
               S : constant String := CPU.To_String (W);
            begin
               Ada.Text_IO.Put (S);
            end;

         when Aqua.Traps.IO_Put_Char =>
            declare
               W : constant Word := CPU.Pop;
               Ch : constant Character :=
                      Character'Val (Get_Integer (W));
            begin
               if Ch = Character'Val (10) then
                  Ada.Text_IO.New_Line;
               else
                  Ada.Text_IO.Put (Ch);
               end if;
            end;

         when Aqua.Traps.String_Substitute =>
            declare
               Replace_With    : constant Word := CPU.Pop;
               Replace_String  : constant Word := CPU.Pop;
               Base_Text       : constant Word := CPU.Pop;
            begin

               if Trace_Properties then
                  Ada.Text_IO.Put_Line
                    ("sub: "
                     & CPU.Show (Base_Text)
                     & ": ["
                     & CPU.Show (Replace_String)
                     & "] -> ["
                     & CPU.Show (Replace_With)
                     & "]");
               end if;

               pragma Assert (Is_String_Reference (Base_Text));
               pragma Assert (Is_String_Reference (Replace_String));
               pragma Assert (Is_String_Reference (Replace_With));

               declare
                  use Ada.Strings.Fixed;
                  use Ada.Strings.Unbounded;
                  Result : Unbounded_String;
                  S : constant String := CPU.To_String (Base_Text);
                  X : constant String := CPU.To_String (Replace_String);
                  Y : constant String := CPU.To_String (Replace_With);
                  Index : Positive := S'First;
               begin
                  while Index <= S'Length - X'Length loop
                     if S (Index .. Index + X'Length - 1) = X then
                        Result := Result & Y;
                        Index := Index + X'Length;
                     else
                        Result := Result & S (Index);
                        Index := Index + 1;
                     end if;
                  end loop;

                  Result := Result & S (Index .. S'Last);

                  CPU.Push (CPU.To_String_Word (To_String (Result)));
               end;
            end;

         when Aqua.Traps.IO_Set_Output =>
            declare
               Name_Value : constant Word := CPU.Pop;
            begin
               if Name_Value = 0
                 or else (Is_String_Reference (Name_Value)
                          and then CPU.To_String (Name_Value) = "")
               then
                  Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
                  Ada.Text_IO.Close (Current_Output);
               elsif not Is_String_Reference (Name_Value) then
                  raise Constraint_Error
                    with "Set_Output: expected a string,but found "
                    & CPU.Show (Name_Value);
               else
                  Ada.Text_IO.Create (Current_Output,
                                      Ada.Text_IO.Out_File,
                                      CPU.To_String (Name_Value));
                  Ada.Text_IO.Set_Output (Current_Output);
               end if;
            end;

         when Aqua.Traps.Report_State =>
            CPU.Report;

         when others =>
            raise Constraint_Error
              with "unimplemented trap:" & Natural'Image (Index);
      end case;
   end Handle_Trap;

   ----------------
   -- Handle_Tst --
   ----------------

   procedure Handle_Tst
     (CPU     : in out Aqua_CPU_Type'Class;
      Byte    : Boolean;
      Operand : Aqua.Architecture.Operand_Type)
   is
      pragma Unreferenced (Byte);

      X : Word;
   begin
      Aqua.Architecture.Read
        (Operand => Operand,
         R       => CPU.R,
         Memory  => CPU.Image.all,
         Value   => X);
      Set_NZ (CPU, X);
   end Handle_Tst;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (CPU : in out Aqua_CPU_Type)
   is
   begin
      Aqua.Primitives.Load_Primitive_Objects (CPU);
   end Initialize;

   ----------
   -- Name --
   ----------

   function Name
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
      return String
   is
   begin
      if Is_Address (Value) then
         return Aqua.IO.Hex_Image (Get_Address (Value));
      elsif Is_Integer (Value) then
         return "#" & Aqua_Integer'Image (Get_Integer (Value));
      elsif Is_External_Reference (Value) then
         return CPU.To_External_Object (Value).Name;
      elsif Is_String_Reference (Value) then
         return CPU.To_String (Value);
      else
         return Aqua.IO.Hex_Image (Value);
      end if;
   end Name;

   ---------
   -- Pop --
   ---------

   overriding function Pop
     (CPU : in out Aqua_CPU_Type)
      return Word
   is
      X : constant Word :=
            CPU.Image.Get_Word
              (Get_Address (CPU.R (6)));
   begin
      if Trace_Stack or else Trace_Code then
         Ada.Text_IO.Put_Line
           ("pop("
            & Aqua.IO.Hex_Image
              (Get_Address (CPU.R (6)))
            & ")->"
            & Aqua.IO.Hex_Image (X));
      end if;

      Aqua.Arithmetic.Inc
        (CPU.R (6), 2);

      return X;

   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
   is
   begin
      Aqua.Arithmetic.Dec (CPU.R (6), 2);
      CPU.Image.Set_Word (Get_Address (CPU.R (6)), Value);
      if Trace_Stack or else Trace_Code then
         Ada.Text_IO.Put_Line
           ("push("
            & Aqua.IO.Hex_Image
              (Get_Address (CPU.R (6)))
            & ","
            & Aqua.IO.Hex_Image (Value)
            & ")");
      end if;
   end Push;

   ------------
   -- Report --
   ------------

   overriding procedure Report
     (CPU : Aqua_CPU_Type)
   is
      use Ada.Calendar;
      use Ada.Strings, Ada.Strings.Fixed;
      use Ada.Text_IO;
   begin
      Put_Line
        ("Memory:"
         & " reserved ="
         & Address'Image (CPU.Image.Code_Low)
         & " code ="
         & Address'Image (CPU.Image.Code_High - CPU.Image.Code_Low)
         & " heap ="
         & Address'Image
           (CPU.Image.Heap_High - CPU.Image.Code_High)
         & " free ="
         & Address'Image (Address'Last - CPU.Image.Heap_High + 1));

      Put_Line ("Objects:" & Natural'Image (CPU.Ext.Last_Index + 1)
                & "/"
                & Trim
                  (External_Reference'Image (External_Reference'Last),
                   Left));
      Put_Line ("Strings:"
                & Natural'Image (CPU.Str.Last_Index + 1)
                & "/"
                & Trim
                  (String_Reference'Image (String_Reference'Last),
                   Left));
      Put_Line ("CPU time:"
                & Natural'Image (Natural (CPU.Exec_Time * 1000.0))
                & "ms");
   end Report;

   ------------
   -- Set_NZ --
   ------------

   procedure Set_NZ
     (CPU   : in out Aqua_CPU_Type'Class;
      Value : Word)
   is
   begin
      CPU.Z := Value = 0
        or else (Is_Integer (Value) and then Get_Integer (Value) = 0)
        or else (Is_Address (Value) and then Get_Address (Value) = 0);
      CPU.N := Value >= 16#8000#
        or else (Is_Integer (Value) and then Get_Integer (Value) < 0);
   end Set_NZ;

   ----------
   -- Show --
   ----------

   overriding function Show
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
      return String
   is
   begin
      if Is_Address (Value) then
         return "@" & Aqua.IO.Hex_Image (Get_Address (Value));
      elsif Is_Integer (Value) then
         return "#" & Aqua_Integer'Image (Get_Integer (Value));
      elsif Is_External_Reference (Value) then
         return CPU.To_External_Object (Value).Show;
      elsif Is_String_Reference (Value) then
         return CPU.To_String (Value);
      else
         return Aqua.IO.Hex_Image (Value);
      end if;
   end Show;

   ----------------
   -- Show_Stack --
   ----------------

   procedure Show_Stack
     (CPU : in out Aqua_CPU_Type)
   is
   begin
      Ada.Text_IO.Put_Line ("---- stack dump");
      for A in Get_Address (CPU.R (6)) .. Address'Last - 1 loop
         if A mod 2 = 0 then
            Ada.Text_IO.Put_Line
              (Aqua.IO.Hex_Image (A)
               & ": "
               & CPU.Show (CPU.Image.Get_Word (A)));
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("---------------");
   end Show_Stack;

   ------------------------
   -- To_External_Object --
   ------------------------

   overriding function To_External_Object
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
      return access External_Object_Interface'Class
   is
   begin
      return CPU.Ext (Positive (Value and not External_Mask_Bits));
   end To_External_Object;

   ----------------
   -- To_Integer --
   ----------------

   overriding function To_Integer
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
      return Aqua_Integer
   is
   begin
      if Is_Integer (Value) then
         return Get_Integer (Value);
      else
         declare
            S : constant String := CPU.To_String (Value);
         begin
            if S = "" then
               return 0;
            else
               return Aqua_Integer'Value (S);
            end if;
         end;
      end if;
   end To_Integer;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
      return String
   is
   begin
      if Value = 0 then
         return "";
      elsif Is_Integer (Value) then
         return Ada.Strings.Fixed.Trim
           (Aqua_Integer'Image (Get_Integer (Value)),
            Ada.Strings.Left);
      elsif CPU.Image.Have_String (Value) then
         return CPU.Image.To_String (Value);
      else
         return CPU.Str (Natural (Get_String_Reference (Value))
                         - CPU.Image.String_Count);
      end if;
   end To_String;

   --------------------
   -- To_String_Word --
   --------------------

   overriding function To_String_Word
     (CPU  : in out Aqua_CPU_Type;
      Text : String)
      return Word
   is
   begin
      if not CPU.Str_Map.Contains (Text) then
         declare
            Ref : constant String_Reference :=
                    String_Reference
                      (CPU.Image.String_Count
                       + CPU.Str.Last_Index
                       + 1);
         begin
            CPU.Str_Map.Insert
              (Text, To_String_Word (Ref));
            CPU.Str.Append (Text);
         end;
      end if;
      return CPU.Str_Map (Text);
   end To_String_Word;

   -------------
   -- To_Word --
   -------------

   overriding function To_Word
     (CPU  : in out Aqua_CPU_Type;
      Item : not null access External_Object_Interface'Class)
      return Word
   is
   begin

      for I in 1 .. CPU.Ext.Last_Index loop
         if External_Object_Access (Item) = CPU.Ext (I) then
            return Word (I) or External_Mask_Value;
         end if;
      end loop;

      if CPU.Ext.Last_Index >= Positive (External_Reference'Last) then
         raise Storage_Error
           with "no free objects";
      end if;

      CPU.Ext.Append (External_Object_Access (Item));
      return Word (CPU.Ext.Last_Index) or External_Mask_Value;
   end To_Word;

end Aqua.CPU;
