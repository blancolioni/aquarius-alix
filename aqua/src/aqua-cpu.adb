with Ada.Strings.Unbounded;

with Ada.Text_IO;

with Aqua.Arithmetic;
with Aqua.IO;
with Aqua.Iterators;
with Aqua.Objects;
with Aqua.Primitives;
with Aqua.Traps;
with Aqua.Words;

package body Aqua.CPU is

   Trace_Properties : constant Boolean := False;
   Trace_Code       : constant Boolean := False;

   Current_Output : Ada.Text_IO.File_Type;

   type Condition_Code is
     (Always, EQ, LT, LE, MI, LOS, VS, CS);

   type Double_Operand_Handler is access
     procedure (CPU      : in out Aqua_CPU_Type'Class;
                Src, Dst : Aqua.Architecture.Operand_Type);

   type Single_Operand_Handler is access
     procedure (CPU     : in out Aqua_CPU_Type'Class;
                Operand : Aqua.Architecture.Operand_Type);

   procedure Handle_Mov
     (CPU      : in out Aqua_CPU_Type'Class;
      Src, Dst : Aqua.Architecture.Operand_Type);

   procedure Handle_Clr
     (CPU     : in out Aqua_CPU_Type'Class;
      Operand : Aqua.Architecture.Operand_Type);

   procedure Handle_Dec
     (CPU     : in out Aqua_CPU_Type'Class;
      Operand : Aqua.Architecture.Operand_Type);

   procedure Handle_Inc
     (CPU     : in out Aqua_CPU_Type'Class;
      Operand : Aqua.Architecture.Operand_Type);

   procedure Handle_Tst
     (CPU     : in out Aqua_CPU_Type'Class;
      Operand : Aqua.Architecture.Operand_Type);

   Double_Operand : constant array (Word range 1 .. 6)
     of Double_Operand_Handler :=
       (Handle_Mov'Access, others => null);

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
      CPU.Image.Set_Word (Address'Last - 1, 0);
      CPU.R (6) := To_Address_Word (Address'Last - 1);

      for Arg of reverse Arguments loop
         CPU.Push (Arg);
      end loop;

      CPU.Push (To_Address_Word (Address'Last - 1));

      CPU.R (7) := To_Address_Word (Start);
      CPU.B := False;

      while not CPU.B loop
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

      CPU.Exec_Time := CPU.Exec_Time + Ada.Calendar.Clock - CPU.Start;

   end Execute;

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
           (CPU, Get_Operand (Op, 5));
      elsif Double_Opcode in 1 .. 6 then
         Double_Operand (Double_Opcode)
           (CPU, Get_Operand (Op, 11), Get_Operand (Op, 5));
      elsif (Op and 8#177700#) = 8#000100# then
         Handle_Jump (CPU, Get_Operand (Op, 5));
      elsif Get_Bits (Op, 14, 4) = 0
        and then CC_Code /= 0
      then
         Handle_Branch (CPU, Condition_Code'Val (CC_Code / 2),
                        CC_Code mod 2 = 0,
                        Op mod 256);
      else
         raise Constraint_Error
           with "unimplemented instruction "
             & Aqua.IO.Octal_Image (Op);
      end if;
   end Handle;

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
      Operand : Aqua.Architecture.Operand_Type)
   is
   begin
      Aqua.Architecture.Write
        (Operand => Operand,
         R       => CPU.R,
         Memory  => CPU.Image.all,
         Value   => 0);
      Set_NZ (CPU, 0);
   end Handle_Clr;

   ----------------
   -- Handle_Dec --
   ----------------

   procedure Handle_Dec
     (CPU     : in out Aqua_CPU_Type'Class;
      Operand : Aqua.Architecture.Operand_Type)
   is
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
      Operand : Aqua.Architecture.Operand_Type)
   is
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
      Src, Dst : Aqua.Architecture.Operand_Type)
   is
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

   -----------------
   -- Handle_Trap --
   -----------------

   procedure Handle_Trap
     (CPU   : in out Aqua_CPU_Type'Class;
      Index : Natural)
   is
   begin
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
               pragma Assert (Right_Word = 0
                              or else Is_String_Reference (Right_Word));
               pragma Assert (Left_Word = 0
                                or else Is_String_Reference (Left_Word));
               Result     : constant String :=
                              CPU.To_String (Left_Word)
                            & CPU.To_String (Right_Word);
            begin
               CPU.Push
                 (CPU.To_String_Word (Result));
            end;
         when Aqua.Traps.Property_Get =>
            declare
               Name   : constant Word := CPU.Pop;
               Target : constant Word := CPU.Pop;
               Value  : Word;
            begin
               if not Is_String_Reference (Name) then
                  raise Constraint_Error
                    with "property name must be a string: "
                    & CPU.Show (Name);
               end if;

               if Is_String_Reference (Target) then
                  CPU.Push (Target);
                  declare
                     Prim : constant Subroutine_Reference :=
                              Aqua.Primitives.Get_Primitive
                                ("string__" & CPU.Image.To_String (Name));
                  begin
                     Value := Aqua.Primitives.Call_Primitive (CPU, Prim);
                  end;
               elsif not Is_External_Reference (Target) then
                  raise Constraint_Error
                    with "property "
                    & CPU.Show (Name)
                    & " defined only on objects; found "
                    & CPU.Show (Target);
               else
                  declare
                     Ext : constant access External_Object_Interface'Class :=
                             CPU.To_External_Object (Target);
                  begin
                     if Ext.all not in Aqua.Objects.Object_Interface'Class then
                        raise Constraint_Error
                          with "property "
                          & CPU.Show (Name)
                          & " defined only on objects; found "
                          & CPU.Show (Target);
                     end if;
                  end;

                  declare
                     use Aqua.Objects;
                     Target_Object : constant access Object_Interface'Class :=
                                       Object_Interface'Class
                                         (CPU.To_External_Object
                                            (Target).all)'Access;
                     Property_Name : constant String :=
                                       CPU.Image.To_String (Name);
                  begin
                     Value := Target_Object.Get_Property (Property_Name);
                  end;

                  if Aqua.Words.Is_Subroutine_Reference (Value) then
                     CPU.Push (Target);
                     Value :=
                       Aqua.Primitives.Call_Primitive
                         (CPU, Aqua.Words.Get_Subroutine_Reference (Value));
                  end if;

               end if;

               if Trace_Properties then
                  Ada.Text_IO.Put_Line
                    ("get: "
                     & CPU.Name (Target)
                     & "."
                     & CPU.Show (Name)
                     & " -> "
                     & CPU.Show (Value));
               end if;

               CPU.Push (Value);
            end;

         when Aqua.Traps.Property_Set =>
            declare
               Value : constant Word := CPU.Pop;
               Name  : constant Word := CPU.Pop;
               Target : constant Word := CPU.Pop;
            begin
               if Trace_Properties then
                  Ada.Text_IO.Put_Line
                    ("set: "
                     & CPU.Name (Target)
                     & "."
                     & CPU.Show (Name)
                     & " <- "
                     & CPU.Show (Value));
               end if;

               if not Is_String_Reference (Name) then
                  raise Constraint_Error
                    with "set: expected a string for property name, but found "
                    & CPU.Show (Name);
               end if;

               if not Is_External_Reference (Target) then
                  raise Constraint_Error
                    with "set: expected an object but found "
                    & CPU.Show (Target);
               end if;

--                 pragma Assert (Is_String_Reference (Name));
--                 pragma Assert (Is_External_Reference (Target));

               declare
                  Ext : constant access External_Object_Interface'Class :=
                          CPU.To_External_Object (Target);
               begin
                  if Ext.all not in Aqua.Objects.Object_Interface'Class then
                     raise Constraint_Error
                       with "property "
                       & CPU.Show (Name)
                       & " defined only on objects; found "
                       & CPU.Show (Target);
                  end if;
               end;

               declare
                  use Aqua.Objects;
                  Target_Object : constant access Object_Interface'Class :=
                                    Object_Interface'Class
                                      (CPU.To_External_Object
                                         (Target).all)'Access;
                  Property_Name : constant String :=
                                    CPU.Image.To_String (Name);
               begin
                  Target_Object.Set_Property (Property_Name, Value);
               end;
            end;

         when Aqua.Traps.Iterator_Start =>
            declare
               use Aqua.Iterators;
               Container_Word : constant Word := CPU.Pop;
               Container_Ext  : access External_Object_Interface'Class;
               Container      : access Aqua_Container_Interface'Class;
            begin
               if not Is_External_Reference (Container_Word) then
                  raise Constraint_Error
                    with "iterator_start: expected an object but found "
                    & CPU.Show (Container_Word);
               end if;

               Container_Ext := CPU.To_External_Object (Container_Word);

               if Container_Ext.all not in Aqua_Container_Interface'Class then
                  raise Constraint_Error
                    with "iterator_start: cannot iterate over "
                    & Container_Ext.Name;
               end if;

               Container :=
                 Aqua_Container_Interface'Class (Container_Ext.all)'Access;

               Ada.Text_IO.Put_Line ("iterating: " & Container.Show);

               declare
                  It : constant External_Object_Access :=
                         new Aqua_Iterator_Interface'Class'
                           (Container.Start);
               begin
                  CPU.Push (CPU.To_Word (It));
                  CPU.Push (0);
               end;
            end;

         when Aqua.Traps.Iterator_Next =>
            declare
               use Aqua.Iterators;
               Old_Value     : constant Word := CPU.Pop;
               pragma Unreferenced (Old_Value);
               Iterator_Word : constant Word := CPU.Pop;
               Iterator_Ext  : access External_Object_Interface'Class;
               Iterator      : access Aqua_Iterator_Interface'Class;
            begin
               if not Is_External_Reference (Iterator_Word) then
                  raise Constraint_Error
                    with "iterator_next: expected an object but found "
                    & CPU.Show (Iterator_Word);
               end if;

               Iterator_Ext := CPU.To_External_Object (Iterator_Word);

               if Iterator_Ext.all not in Aqua_Iterator_Interface'Class then
                  raise Constraint_Error
                    with "iterator_next: cannot iterate over "
                    & Iterator_Ext.Name;
               end if;

               Iterator :=
                 Aqua_Iterator_Interface'Class (Iterator_Ext.all)'Access;

               Iterator.Next (CPU.Z);
               if not CPU.Z then
                  CPU.Push (Iterator_Word);
                  CPU.Push (Iterator.Current);
               end if;
            end;

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
      Operand : Aqua.Architecture.Operand_Type)
   is
      X : Word;
   begin
      Aqua.Architecture.Read
        (Operand => Operand,
         R       => CPU.R,
         Memory  => CPU.Image.all,
         Value   => X);
      Set_NZ (CPU, X);
   end Handle_Tst;

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
   begin
      return X : constant Word :=
        CPU.Image.Get_Word
          (Get_Address (CPU.R (6)))
      do
         Aqua.Arithmetic.Inc
           (CPU.R (6), 2);
      end return;
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
   end Push;

   ------------
   -- Report --
   ------------

   procedure Report
     (CPU : Aqua_CPU_Type)
   is
      use Ada.Calendar;
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

      Put_Line ("Objects:" & Natural'Image (CPU.Ext.Last_Index + 1));
      Put_Line ("Strings:" & Natural'Image (CPU.Str.Last_Index + 1));
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
         return Aqua.IO.Hex_Image (Get_Address (Value));
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
