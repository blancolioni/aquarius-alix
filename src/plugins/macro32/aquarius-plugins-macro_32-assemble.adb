with Ada.Directories;
with Ada.Strings.Unbounded;

with Aqua.Assembler;
with Aqua.Architecture;

with Aquarius.Errors;

package body Aquarius.Plugins.Macro_32.Assemble is

   type Root_Assembly_Object is
     new Root_Aquarius_Object with
      record
         Path     : Ada.Strings.Unbounded.Unbounded_String;
         Assembly : Aqua.Assembler.Assembly;
      end record;

   overriding function Name (Item : Root_Assembly_Object) return String
   is (Ada.Strings.Unbounded.To_String (Item.Path));

   type Assembly_Object is access all Root_Assembly_Object'Class;

   function Get_Operand
     (Arg : Aquarius.Programs.Program_Tree)
      return Aqua.Architecture.Operand_Type;

   function Get_Operand_Size
     (Tree : Aquarius.Programs.Program_Tree)
      return Aqua.Data_Size;

   function Get_Size
     (Tree : Aquarius.Programs.Program_Tree)
      return Aqua.Data_Size;

   procedure Place_Operand
     (Arg      : Aquarius.Programs.Program_Tree;
      Operand  : Aqua.Architecture.Operand_Type;
      Size     : Aqua.Data_Size);

   procedure Evaluate_Operand
     (Assembly : Aqua.Assembler.Assembly;
      Tree     : Aquarius.Programs.Program_Tree;
      Relative : Boolean;
      Size     : Aqua.Data_Size);

   type Expression_Value (Deferred : Boolean) is
      record
         case Deferred is
            when True =>
               Deferred_Label : Aquarius.Programs.Program_Tree;
            when False =>
               Word_Value     : Aqua.Word;
         end case;
      end record;

   function Evaluate_Expression
     (Assembly    : Aqua.Assembler.Assembly;
      Tree        : Aquarius.Programs.Program_Tree;
      Deferred_OK : Boolean := True)
      return Expression_Value;

   ------------------
   -- After_Branch --
   ------------------

   procedure After_Branch
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Branch : constant Program_Tree := Program_Tree (Target);
      Mnemonic : constant String :=
                   Branch.Program_Child
                     ("branch_instruction").Concatenate_Children;
      Destination : constant Program_Tree :=
                      Branch.Program_Child
                        ("branch_destination");
      Dest_Address : Aqua.Word;
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Branch.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin

      declare
         use Aqua.Architecture;
      begin
         Assembly.Append_Octet
           (Aqua.Architecture.Encode
              (Branch_Instruction'Value ("A_" & Mnemonic)));
      end;

      if Destination.Chosen_Tree.Name = "identifier" then
         Dest_Address :=
           Assembly.Reference_Branch_Label
             (Destination.Chosen_Tree.Text);
      else
         declare
            Label_Tree : constant Program_Tree :=
                           Destination.Chosen_Tree.Program_Child ("integer");
            Label : constant Natural :=
                           Natural'Value (Label_Tree.Text);
            Direction  : constant Program_Tree :=
                           Destination.Chosen_Tree.Program_Child
                             ("label_direction").Chosen_Tree;
            Forward    : Boolean := Direction.Text = "+";
            Backward   : constant Boolean := Direction.Text = "-";
         begin
            if not Forward and then not Backward then
               Aquarius.Errors.Error
                 (Direction,
                  "unable to understand branch direction: "
                 & Direction.Text & Label_Tree.Text);
               Forward := True;
            end if;
            Dest_Address :=
              Assembly.Reference_Temporary_Branch_Label
                (Label, Forward);
         end;
      end if;

      declare
         use Aqua;
      begin
         Assembly.Append_Octet (Aqua.Octet (Dest_Address mod 256));
         Assembly.Append_Octet (Aqua.Octet (Dest_Address / 256 mod 256));
      end;

   end After_Branch;

   ----------------
   -- After_Call --
   ----------------

   procedure After_Call
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Op       : constant Program_Tree := Program_Tree (Target);
      Dst      : constant Program_Tree :=
                   Op.Program_Child ("arg");
      Dst_Op   : constant Aqua.Architecture.Operand_Type :=
                   Get_Operand (Dst);
      Argument_Tree  : constant Program_Tree :=
                         Op.Program_Child ("integer");
      Argument_Count : constant Natural :=
                         (if Argument_Tree /= null
                          then Natural'Value
                            (Argument_Tree.Text)
                          else 0);
      Assembly : constant Aqua.Assembler.Assembly :=
                   Assembly_Object
                     (Op.Property
                        (Global_Plugin.Assembly)).Assembly;
   begin
      Assembly.Append_Octet
        (Aqua.Architecture.Encode
           (Aqua.Architecture.A_Call,
            Immediate => Aqua.Octet (Argument_Count)));
      Assembly.Append_Octet
        (Aqua.Architecture.Encode (Dst_Op));
      Place_Operand (Dst, Dst_Op, Aqua.Word_32_Size);
   end After_Call;

   -----------------------
   -- After_Declaration --
   -----------------------

   procedure After_Declaration
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Dec : constant Program_Tree := Program_Tree (Target);
      Assembly : constant Aqua.Assembler.Assembly :=
                   Assembly_Object
                     (Dec.Property
                        (Global_Plugin.Assembly)).Assembly;
      Name     : constant String := Dec.Program_Child ("identifier").Text;
      Value    : constant Expression_Value :=
                   Evaluate_Expression (Assembly,
                                        Dec.Program_Child ("expression"),
                                        Deferred_OK => False);
   begin
      pragma Assert (not Value.Deferred);
      Assembly.Define_Value (Name, Value.Word_Value);
   end After_Declaration;

   ---------------------
   -- After_Directive --
   ---------------------

   procedure After_Directive
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Directive : constant Program_Tree := Program_Tree (Target);
      Assembly  : constant Aqua.Assembler.Assembly :=
                    Assembly_Object
                      (Directive.Property
                         (Global_Plugin.Assembly)).Assembly;
      Name      : constant String :=
                    Directive.Program_Child ("identifier").Text;
      Arguments : constant Array_Of_Program_Trees :=
                    Directive.Direct_Children ("directive_operand");
   begin
      if Name = "extern" then
         for Arg of Arguments loop
            declare
               Label : constant Program_Tree := Arg.Chosen_Tree;
            begin
               if Label.Name /= "identifier" then
                  Aquarius.Errors.Error
                    (Label, "expected an identifier");
               else
                  Assembly.Define_External_Label (Label.Text);
               end if;
            end;
         end loop;
      elsif Name = "export" then
         for Arg of Arguments loop
            declare
               Label : constant Program_Tree := Arg.Chosen_Tree;
            begin
               if Label.Name /= "identifier" then
                  Aquarius.Errors.Error
                    (Label, "expected an identifier");
               else
                  Assembly.Define_Exported_Label (Label.Text);
               end if;
            end;
         end loop;
      elsif Name = "bind_action" then
         declare
            Group_Name : constant String :=
                           Arguments (1).Chosen_Tree.Text;
            Position   : constant String :=
                           Arguments (2).Chosen_Tree.Text;
            Parent     : constant String :=
                           Arguments (3).Chosen_Tree.Text;
            Child      : constant String :=
                           (if Arguments'Length > 3
                            then Arguments (4).Chosen_Tree.Text
                            else "");
         begin
            Assembly.Bind_Action (Group_Name, Position = "before",
                                  Parent, Child);
         end;
      elsif Name = "exception" then
         declare
            Base    : constant String := Arguments (1).Chosen_Tree.Text;
            Bound   : constant String := Arguments (2).Chosen_Tree.Text;
            Handler : constant String := Arguments (3).Chosen_Tree.Text;
         begin
            Assembly.Exception_Handler
              (Base_Label    => Base,
               Bound_Label   => Bound,
               Handler_Label => Handler);
         end;
      elsif Name = "start" then
         declare
            Start : constant String :=
                      Arguments (1).Chosen_Tree.Text;
         begin
            Assembly.Set_Start_Label (Start);
         end;
      elsif Name = "word" then
         for Arg of Arguments loop
            if Arg.Chosen_Tree.Name = "identifier" then
               Assembly.Append_Word
                 (Assembly.Reference_Label (Arg.Chosen_Tree.Text, False));
            elsif Arg.Chosen_Tree.Name = "integer" then
               Assembly.Append_Word (Aqua.Word'Value (Arg.Chosen_Tree.Text));
            else
               raise Constraint_Error with
                 "cannot append word: " & Arg.Chosen_Tree.Name;
            end if;
         end loop;
      elsif Name = "data" then
         Assembly.Data_Segment;
      elsif Name = "text" then
         Assembly.Text_Segment;
      elsif Name = "code" then
         Assembly.Code_Segment;
      elsif Name = "source_file" then
         Assembly.Set_Source_File
           (Arguments (1).Chosen_Tree.Text);
      elsif Name = "source_position" then
         Assembly.Set_Source_Location
           (Line   => Natural'Value (Arguments (1).Chosen_Tree.Text),
            Column => Natural'Value (Arguments (2).Chosen_Tree.Text));
      end if;
   end After_Directive;

   --------------------------
   -- After_Double_Operand --
   --------------------------

   procedure After_Double_Operand
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Op : constant Program_Tree := Program_Tree (Target);
      Mnemonic : constant String :=
                   Op.Program_Child
                     ("double_operand_instruction").Concatenate_Children;
      Size_Tree : constant Program_Tree :=
                    Op.Program_Child ("size");
      Size      : constant Aqua.Data_Size := Get_Size (Size_Tree);
      Src      : constant Program_Tree :=
                   Op.Program_Child ("arg", 1);
      Dst      : constant Program_Tree :=
                   Op.Program_Child ("arg", 2);
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Op.Property
                              (Global_Plugin.Assembly)).Assembly;
      Src_Op         : constant Aqua.Architecture.Operand_Type :=
                         Get_Operand (Src);
      Dst_Op         : constant Aqua.Architecture.Operand_Type :=
                         Get_Operand (Dst);
   begin
      Assembly.Append_Octet
        (Aqua.Architecture.Encode
           (Aqua.Architecture.Aqua_Instruction'Value ("A_" & Mnemonic)));
      Assembly.Append_Octet
        (Aqua.Architecture.Encode (Src_Op));
      Place_Operand (Src, Src_Op, Size);
      Assembly.Append_Octet
        (Aqua.Architecture.Encode (Dst_Op));
      Place_Operand (Dst, Dst_Op, Size);
   end After_Double_Operand;

   ----------------
   -- After_Goto --
   ----------------

   procedure After_Goto
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Op       : constant Program_Tree := Program_Tree (Target);
      Dst      : constant Program_Tree :=
                   Op.Program_Child ("arg");
      Dst_Op   : constant Aqua.Architecture.Operand_Type :=
                   Get_Operand (Dst);
      Assembly : constant Aqua.Assembler.Assembly :=
                   Assembly_Object
                     (Op.Property
                        (Global_Plugin.Assembly)).Assembly;
   begin
      Assembly.Append_Octet
        (Aqua.Architecture.Encode (Aqua.Architecture.A_Goto));
      Assembly.Append_Octet
        (Aqua.Architecture.Encode (Dst_Op));
      Place_Operand (Dst, Dst_Op, Aqua.Word_32_Size);
   end After_Goto;

   --------------------
   -- After_Iterator --
   --------------------

   procedure After_Iterator
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Op : constant Program_Tree := Program_Tree (Target);
      Mnemonic       : constant String :=
                         Op.Program_Child
                           ("iterator_instruction").Concatenate_Children;
      Register       : constant Program_Tree :=
                         Op.Program_Child ("identifier");
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Op.Property
                              (Global_Plugin.Assembly)).Assembly;
      Register_Octet : constant Aqua.Octet :=
                         (if Register = null then 0
                          else Aqua.Octet
                            (Assembly.Get_Register (Register.Text)));
   begin
      Assembly.Append_Octet
        (Aqua.Architecture.Encode
           (Aqua.Architecture.Aqua_Instruction'Value
                ("A_" & Mnemonic),
            Immediate => Register_Octet));
   end After_Iterator;

   ----------------
   -- After_Jump --
   ----------------

   procedure After_Jump
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Jump : constant Program_Tree := Program_Tree (Target);
      Mnemonic : constant String :=
                   Jump.Program_Child
                     ("jump_instruction").Concatenate_Children;
      Destination : constant Program_Tree :=
                      Jump.Program_Child
                        ("jump_destination");
      Dest_Address : Aqua.Word;
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Jump.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin

      declare
         use Aqua.Architecture;
      begin
         Assembly.Append_Octet
           (Aqua.Architecture.Encode
              (Branch_Instruction'Value ("A_" & Mnemonic)));
      end;

      if Destination.Chosen_Tree.Name = "identifier" then
         Dest_Address :=
           Assembly.Reference_Label
             (Destination.Chosen_Tree.Text, True);
      else
         declare
            Label_Tree : constant Program_Tree :=
                           Destination.Chosen_Tree.Program_Child ("integer");
            Label : constant Natural :=
                           Natural'Value (Label_Tree.Text);
            Direction  : constant Program_Tree :=
                           Destination.Chosen_Tree.Program_Child
                             ("label_direction").Chosen_Tree;
            Forward    : Boolean := Direction.Text = "+";
            Backward   : constant Boolean := Direction.Text = "-";
         begin
            if not Forward and then not Backward then
               Aquarius.Errors.Error
                 (Direction,
                  "unable to understand jump direction: "
                 & Direction.Text & Label_Tree.Text);
               Forward := True;
            end if;
            Dest_Address :=
              Assembly.Reference_Temporary_Label
                (Label, Forward);
         end;
      end if;

      Assembly.Append_Word (Dest_Address);

   end After_Jump;

   ----------------------
   -- After_No_Operand --
   ----------------------

   procedure After_No_Operand
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Op : constant Program_Tree := Program_Tree (Target);
      Mnemonic : constant String := Op.Concatenate_Children;
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Op.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin
      Assembly.Append_Octet
        (Aqua.Architecture.Encode
           (Aqua.Architecture.Aqua_Instruction'Value ("A_" & Mnemonic)));
   end After_No_Operand;

   --------------------------
   -- After_Single_Operand --
   --------------------------

   procedure After_Single_Operand
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Op : constant Program_Tree := Program_Tree (Target);
      Mnemonic : constant String :=
                   Op.Program_Child
                     ("single_operand_instruction").Concatenate_Children;
      Size_Tree : constant Program_Tree :=
                    Op.Program_Child ("size");
      Size      : constant Aqua.Data_Size := Get_Size (Size_Tree);
      Dst      : constant Program_Tree :=
                   Op.Program_Child ("arg");
      Dst_Op         : constant Aqua.Architecture.Operand_Type :=
                         Get_Operand (Dst);
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Op.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin
      Assembly.Append_Octet
        (Aqua.Architecture.Encode
           (Aqua.Architecture.Aqua_Instruction'Value ("A_" & Mnemonic)));
      Assembly.Append_Octet
        (Aqua.Architecture.Encode (Dst_Op));
      Place_Operand (Dst, Dst_Op, Size);
   end After_Single_Operand;

   -----------------------
   -- After_Source_File --
   -----------------------

   procedure After_Source_File
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Source_File    : constant Program_Tree := Program_Tree (Target);
      Source_Name    : constant String :=
                         Source_File.Source_File_Name;
      Assembly       : Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Source_File.Property
                              (Global_Plugin.Assembly)).Assembly;
      Output_File    : constant String :=
                         Ada.Directories.Base_Name (Source_Name) & ".o32";
   begin
      --  Assembly.Write_Listing;
      Assembly.Write_Image (Output_File);
      Aqua.Assembler.Free (Assembly);
   end After_Source_File;

   ----------------
   -- After_Trap --
   ----------------

   procedure After_Trap
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Trap_Tree    : constant Program_Tree := Program_Tree (Target);
      Operand_Tree : constant Program_Tree :=
                       Trap_Tree.Program_Child ("trap_operand").Chosen_Tree;
      Assembly     : constant Aqua.Assembler.Assembly :=
                       Assembly_Object
                         (Operand_Tree.Property
                            (Global_Plugin.Assembly)).Assembly;
      Trap         : Natural;
   begin
      if Operand_Tree.Name = "identifier" then
         if not Assembly.Is_Defined (Operand_Tree.Text) then
            Aquarius.Errors.Error
              (Operand_Tree,
               Operand_Tree.Text & " must be defined before this point");
            Trap := 0;
            Assembly.Define_Value (Operand_Tree.Text, 0);
         else
            Trap :=
              Natural
                (Assembly.Reference_Label (Operand_Tree.Text, False));
         end if;
      else
         Trap := Natural'Value (Operand_Tree.Text);
      end if;

      if Trap > 15 then
         Aquarius.Errors.Error (Operand_Tree,
                                "trap must be in range 0 .. 15");
         Trap := 0;
      end if;

      Assembly.Append_Octet
        (Aqua.Architecture.Encode
           (Aqua.Architecture.A_Trap,
            Immediate => Aqua.Octet (Trap)));

   end After_Trap;

   --------------------------
   -- After_Triple_Operand --
   --------------------------

   procedure After_Triple_Operand
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Op : constant Program_Tree := Program_Tree (Target);
      Mnemonic : constant String :=
                   Op.Program_Child
                     ("double_operand_instruction").Concatenate_Children;
      Size_Tree : constant Program_Tree :=
                    Op.Program_Child ("size");
      Size      : constant Aqua.Data_Size := Get_Size (Size_Tree);
      Src_1     : constant Program_Tree :=
                   Op.Program_Child ("arg", 1);
      Src_2     : constant Program_Tree :=
                    Op.Program_Child ("arg", 2);
      Dst      : constant Program_Tree :=
                   Op.Program_Child ("arg", 3);
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Op.Property
                              (Global_Plugin.Assembly)).Assembly;
      Src_1_Op       : constant Aqua.Architecture.Operand_Type :=
                         Get_Operand (Src_1);
      Src_2_Op       : constant Aqua.Architecture.Operand_Type :=
                         Get_Operand (Src_2);
      Dst_Op         : constant Aqua.Architecture.Operand_Type :=
                         Get_Operand (Dst);
   begin
      Assembly.Append_Octet
        (Aqua.Architecture.Encode
           (Aqua.Architecture.Aqua_Instruction'Value
                ("A_" & Mnemonic & "_3")));
      Assembly.Append_Octet
        (Aqua.Architecture.Encode (Src_1_Op));
      Place_Operand (Src_1, Src_1_Op, Size);
      Assembly.Append_Octet
        (Aqua.Architecture.Encode (Src_2_Op));
      Place_Operand (Src_2, Src_2_Op, Size);
      Assembly.Append_Octet
        (Aqua.Architecture.Encode (Dst_Op));
      Place_Operand (Dst, Dst_Op, Size);
   end After_Triple_Operand;

   ------------------------
   -- Before_Source_File --
   ------------------------

   procedure Before_Source_File
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Source_File : constant Program_Tree := Program_Tree (Target);
      A           : constant Aqua.Assembler.Assembly :=
                      new Aqua.Assembler.Root_Assembly_Type;
      Object      : constant Assembly_Object :=
                      new Root_Assembly_Object'
                        (Path     =>
                          Ada.Strings.Unbounded.To_Unbounded_String
                           (Source_File.Source_File_Name),
                         Assembly => A);
   begin
      A.Start;
      Source_File.Set_Property
        (Global_Plugin.Assembly, Object);
   end Before_Source_File;

   -------------------------
   -- Evaluate_Expression --
   -------------------------

   function Evaluate_Expression
     (Assembly    : Aqua.Assembler.Assembly;
      Tree        : Aquarius.Programs.Program_Tree;
      Deferred_OK : Boolean := True)
      return Expression_Value
   is
      use Aquarius.Programs;
      use Aqua;
      Es     : constant Array_Of_Program_Trees :=
             Tree.Direct_Children;
      Op     : Program_Tree := null;
      First  : Boolean := True;
      Result : Word := 0;
   begin
--        if Es'Length = 1 then
--           Place_Operand (Es (1));
--           return;
--        end if;

      for E of Es loop
         if First or else Op = null then
            declare
               Operand : constant Program_Tree := E.Chosen_Tree;
               Value   : Word;
            begin
               if Operand.Name = "identifier" then
                  if Assembly.Is_Defined (Operand.Text) then
                     Value := Assembly.Get_Value (Operand.Text);
                  elsif not Deferred_OK then
                     Aquarius.Errors.Error
                       (E, "undefined: " & Operand.Text);
                     Value := 0;
                  elsif Es'Length > 1 then
                     Aquarius.Errors.Error
                       (E, "invalid deferred expression");
                     Value := 0;
                  else
                     return (True, Operand);
                  end if;
               elsif Operand.Name = "integer" then
                  Value := Word'Value (Operand.Text);
               elsif Operand.Name = "negative_integer" then
                  declare
                     Node : constant Program_Tree :=
                              Operand.Program_Child ("integer");
                  begin
                     Value := (not Word'Value (Node.Text)) + 1;
                  end;
               else
                  raise Constraint_Error
                    with "invalid operand: " & Operand.Name;
               end if;
               if First then
                  Result := Value;
                  First := False;
               else
                  if Op.Name = "+" then
                     Result := Result + Value;
                  elsif Op.Name = "-" then
                     Result := Result - Value;
                  else
                     raise Constraint_Error
                       with "unknown operator: " & Op.Name;
                  end if;
               end if;
            end;
         else
            Op := E.Chosen_Tree;
         end if;
      end loop;

      return (False, Result);

   end Evaluate_Expression;

   ----------------------
   -- Evaluate_Operand --
   ----------------------

   procedure Evaluate_Operand
     (Assembly : Aqua.Assembler.Assembly;
      Tree     : Aquarius.Programs.Program_Tree;
      Relative : Boolean;
      Size     : Aqua.Data_Size)
   is
      use type Aqua.Word;
      use Aquarius.Programs;
      Op : constant Program_Tree := Tree.Chosen_Tree;
   begin
      if Op.Name = "identifier" then
         Assembly.Append
           (Assembly.Reference_Label
              (Op.Text, Relative),
           Size);
      elsif Op.Name = "integer" then
         Assembly.Append
           (Aqua.Word'Value (Op.Text),
            Size);
      elsif Op.Name = "negative_integer" then
         Assembly.Append
           ((not Aqua.Word'Value (Op.Program_Child ("integer").Text)) + 1,
            Size);
      else
         raise Constraint_Error
           with "invalid operand: " & Op.Name;
      end if;
   end Evaluate_Operand;

   -----------------
   -- Get_Operand --
   -----------------

   function Get_Operand
     (Arg : Aquarius.Programs.Program_Tree)
      return Aqua.Architecture.Operand_Type
   is
      use Aqua.Architecture;
      use Aquarius.Programs;
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Arg.Property
                              (Global_Plugin.Assembly)).Assembly;
      Operand_Tree   : constant Program_Tree := Arg.Chosen_Tree;
      Operand_Name   : constant String := Operand_Tree.Name;
   begin
      if Operand_Name = "identifier" then
         if Assembly.Is_Register (Operand_Tree.Text) then
            return (Register, False,
                    Assembly.Get_Register (Operand_Tree.Text), 0);
         else
            return (Indexed_32, False, Aqua.Architecture.R_PC, 0);
         end if;
      elsif Operand_Name = "deferred" then
         if Assembly.Is_Register
           (Operand_Tree.Program_Child ("identifier").Text)
         then
            return (Register, True,
                    Assembly.Get_Register
                      (Operand_Tree.Program_Child ("identifier").Text), 0);
         else
            raise Constraint_Error with
            Operand_Tree.Show_Location & ": expected a register name";
         end if;
      elsif Operand_Name = "autoincrement" then
         return (Postincrement, False,
                 Assembly.Get_Register
                   (Operand_Tree.Program_Child ("identifier").Text), 0);
      elsif Operand_Name = "autodecrement" then
         return (Predecrement, False,
                 Assembly.Get_Register
                   (Operand_Tree.Program_Child ("identifier").Text), 0);
      elsif Operand_Name = "indexed" then
         declare
            use Aqua;
            Size : constant Aqua.Data_Size :=
                     Get_Operand_Size
                       (Operand_Tree.Program_Child ("operand"));
            Mode : constant Addressing_Mode :=
                     (case Size is
                         when Aqua.Word_8_Size =>
                            Indexed_8,
                         when Aqua.Word_16_Size | Aqua.Word_32_Size =>
                            Indexed_32);
         begin
            return (Mode, False,
                    Assembly.Get_Register
                      (Operand_Tree.Program_Child ("identifier").Text), 0);
         end;
      elsif Operand_Name = "indexed_deferred" then
         declare
            use Aqua;
            Size : constant Aqua.Data_Size :=
                     Get_Operand_Size
                       (Operand_Tree.Program_Child ("operand"));
            Mode : constant Addressing_Mode :=
                     (case Size is
                         when Aqua.Word_8_Size  =>
                            Indexed_8,
                         when Aqua.Word_16_Size | Aqua.Word_32_Size =>
                            Indexed_32);
         begin
            return (Mode, True,
                    Assembly.Get_Register
                      (Operand_Tree.Program_Child ("identifier").Text), 0);
         end;

      elsif Operand_Name = "immediate" then
         declare
            use Aqua;
            X : constant Expression_Value :=
                  Evaluate_Expression
                    (Assembly,
                     Operand_Tree.Program_Child ("expression"));
         begin
            if not X.Deferred and then X.Word_Value < 32 then
               return (Small_Immediate, False, 0, Octet (X.Word_Value));
            else
               return (Postincrement, False, Aqua.Architecture.R_PC, 0);
            end if;
         end;
      else
         raise Constraint_Error
           with "unknown mode: " & Operand_Name;
      end if;
   end Get_Operand;

   ----------------------
   -- Get_Operand_Size --
   ----------------------

   function Get_Operand_Size
     (Tree : Aquarius.Programs.Program_Tree)
      return Aqua.Data_Size
   is
      use Aquarius.Programs;
      Op : constant Program_Tree := Tree.Chosen_Tree;
   begin
      if Op.Name = "identifier" then
         return Aqua.Word_32_Size;
      elsif Op.Name = "integer" then
         declare
            Value : constant Integer :=
                      Integer'Value (Op.Text);
         begin
            if Value < 128 then
               return Aqua.Word_8_Size;
            elsif Value < 32768 then
               return Aqua.Word_16_Size;
            else
               return Aqua.Word_32_Size;
            end if;
         end;
      elsif Op.Name = "negative_integer" then
         declare
            Value : constant Integer :=
                  Integer'Value (Op.Program_Child ("integer").Text);
         begin
            if Value <= 128 then
               return Aqua.Word_8_Size;
            elsif Value <= 32768 then
               return Aqua.Word_16_Size;
            else
               return Aqua.Word_32_Size;
            end if;
         end;
      else
         raise Constraint_Error
           with "invalid operand: " & Op.Name;
      end if;
   end Get_Operand_Size;

   --------------
   -- Get_Size --
   --------------

   function Get_Size
     (Tree : Aquarius.Programs.Program_Tree)
      return Aqua.Data_Size
   is
      use Aquarius.Programs;
   begin
      if Tree = null
        or else Tree.Program_Child ("integer") = null
      then
         return Aqua.Word_32_Size;
      else
         declare
            Size : constant Integer :=
                     Integer'Value (Tree.Program_Child ("integer").Text);
         begin
            if Size = 1 then
               return Aqua.Word_8_Size;
            elsif Size = 2 then
               return Aqua.Word_16_Size;
            else
               return Aqua.Word_32_Size;
            end if;
         end;
      end if;
   end Get_Size;

   -------------------
   -- Place_Operand --
   -------------------

   procedure Place_Operand
     (Arg      : Aquarius.Programs.Program_Tree;
      Operand  : Aqua.Architecture.Operand_Type;
      Size     : Aqua.Data_Size)
   is
      use Aqua.Architecture;
      use Aquarius.Programs;
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Arg.Property
                              (Global_Plugin.Assembly)).Assembly;
      Operand_Tree   : constant Program_Tree := Arg.Chosen_Tree;
      Operand_Name   : constant String := Operand_Tree.Name;

   begin
      if Operand.Mode = Small_Immediate
        or else Operand.Mode = Register
      then
         null;
      elsif Operand_Name = "identifier" then
         if Assembly.Is_Register (Operand_Tree.Text) then
            null;
         else
            Assembly.Append
              (Assembly.Reference_Label (Operand_Tree.Text, True), Size);
         end if;
      elsif Operand_Name = "deferred" then
         if Assembly.Is_Register
           (Operand_Tree.Program_Child ("identifier").Text)
         then
            null;
         else
            Assembly.Append
              (Assembly.Reference_Label
                 (Operand_Tree.Program_Child ("identifier").Text, True),
               Aqua.Word_32_Size);
         end if;
      elsif Operand_Name = "indexed"
        or else Operand_Name = "indexed_deferred"
      then
         Evaluate_Operand (Assembly, Operand_Tree.Program_Child ("operand"),
                           False, Get_Mode_Size (Operand.Mode));
      elsif Operand_Name = "immediate"
        or else Operand_Name = "absolute"
      then
         declare
            Value : constant Expression_Value :=
                      Evaluate_Expression
                        (Assembly,
                         Operand_Tree.Program_Child ("expression"));
         begin
            if Value.Deferred then
               Assembly.Append
                 (Assembly.Reference_Label
                    (Value.Deferred_Label.Text, Relative => False),
                  Aqua.Word_32_Size);
            else
               Assembly.Append
                 (Value.Word_Value, Aqua.Word_32_Size);
            end if;
         end;
      end if;
   end Place_Operand;

   --------------------------------
   -- Source_Element_After_Label --
   --------------------------------

   procedure Source_Element_After_Label
     (Parent : not null access Aquarius.Actions.Actionable'Class;
      Child  : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Source_Element : constant Program_Tree :=
                         Program_Tree (Parent);
      Label          : constant Program_Tree :=
                         Program_Tree (Child).Chosen_Tree;
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Source_Element.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin
      if Label.Name = "identifier" then
         Assembly.Define_Label (Label.Text);
      else
         Assembly.Define_Temporary_Label (Natural'Value (Label.Text));
      end if;
   end Source_Element_After_Label;

end Aquarius.Plugins.Macro_32.Assemble;
