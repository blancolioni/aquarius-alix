with Ada.Strings.Unbounded;

with Aqua.Assembler.Instructions;
with Aqua.Architecture;

with Aquarius.Errors;

package body Aquarius.Plugins.Macro_11.Assemble is

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

   procedure Place_Operand
     (Arg : Aquarius.Programs.Program_Tree);

   procedure Evaluate_Operand
     (Assembly : Aqua.Assembler.Assembly;
      Tree     : Aquarius.Programs.Program_Tree;
      Relative : Boolean);

   function Evaluate_Expression
     (Assembly : Aqua.Assembler.Assembly;
      Tree     : Aquarius.Programs.Program_Tree)
      return Aqua.Word;

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

      Assembly.Append
        (Aqua.Assembler.Instructions.Create_Branch_Instruction_Word
           (Mnemonic => Mnemonic,
            Dst      => Dest_Address));

   end After_Branch;

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
      Value    : constant Aqua.Word :=
                   Evaluate_Expression (Assembly,
                                        Dec.Program_Child ("expression"));
   begin
      Assembly.Define_Value (Name, Value);
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
                    Directive.Direct_Children ("operand");
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
      Src      : constant Program_Tree :=
                   Op.Program_Child ("arg", 1);
      Dst      : constant Program_Tree :=
                   Op.Program_Child ("arg", 2);
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Op.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin
      Assembly.Append
        (Aqua.Assembler.Instructions.Create_Instruction_Word
           (Mnemonic => Mnemonic,
            Src      => Get_Operand (Src),
            Dst      => Get_Operand (Dst)));
      Place_Operand (Src);
      Place_Operand (Dst);
   end After_Double_Operand;

   ----------------
   -- After_Jump --
   ----------------

   procedure After_Jump
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Op : constant Program_Tree := Program_Tree (Target);
      Dst      : constant Program_Tree :=
                   Op.Program_Child ("arg");
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Op.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin
      Assembly.Append
        (Aqua.Assembler.Instructions.Create_Jump_Instruction
           (Dst      => Get_Operand (Dst)));
      Place_Operand (Dst);
   end After_Jump;

   ---------------------------
   -- After_Jump_Subroutine --
   ---------------------------

   procedure After_Jump_Subroutine
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Jsr : constant Program_Tree := Program_Tree (Target);
      Reg : constant String := Jsr.Program_Child ("identifier").Text;
      Dst : constant Program_Tree :=
              Jsr.Program_Child ("arg");
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Jsr.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin
      if not Assembly.Is_Register (Reg) then
         raise Constraint_Error
           with "jsr: require a register but found " & Reg;
      end if;
      Assembly.Append
        (Aqua.Assembler.Instructions.Create_Jsr_Instruction
           (Register => Assembly.Get_Register (Reg),
            Dst      => Get_Operand (Dst)));
      Place_Operand (Dst);
   end After_Jump_Subroutine;

   ------------------
   -- After_Return --
   ------------------

   procedure After_Return
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Rts            : constant Program_Tree := Program_Tree (Target);
      Reg            : constant String :=
                         Rts.Program_Child ("identifier").Text;
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Rts.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin
      if not Assembly.Is_Register (Reg) then
         raise Constraint_Error
           with "rts: require a register but found " & Reg;
      end if;
      Assembly.Append
        (Aqua.Assembler.Instructions.Create_Return_Instruction_Word
           (Assembly.Get_Register (Reg)));
   end After_Return;

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
      Dst      : constant Program_Tree :=
                   Op.Program_Child ("arg");
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Op.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin
      Assembly.Append
        (Aqua.Assembler.Instructions.Create_Instruction_Word
           (Mnemonic => Mnemonic,
            Dst      => Get_Operand (Dst)));
      Place_Operand (Dst);
   end After_Single_Operand;

   -----------------------
   -- After_Source_File --
   -----------------------

   procedure After_Source_File
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Source_File : constant Program_Tree := Program_Tree (Target);
      Assembly       : constant Aqua.Assembler.Assembly :=
                         Assembly_Object
                           (Source_File.Property
                              (Global_Plugin.Assembly)).Assembly;
   begin
      --  Assembly.Write_Listing;
      Assembly.Write_Image
        (Source_File.Source_File_Name & ".o11");
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

      if Trap > 255 then
         Aquarius.Errors.Error (Operand_Tree,
                                "trap must be in range 0 .. 255");
         Trap := 0;
      end if;

      Assembly.Append
        (Aqua.Assembler.Instructions.Create_Trap_Instruction_Word
           (Trap));

   end After_Trap;

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
     (Assembly : Aqua.Assembler.Assembly;
      Tree     : Aquarius.Programs.Program_Tree)
      return Aqua.Word
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
                  else
                     raise Constraint_Error
                       with "undefined: " & Operand.Text;
                  end if;
               elsif Operand.Name = "integer" then
                  Value := Word'Value (Operand.Text);
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

      return Result;

   end Evaluate_Expression;

   ----------------------
   -- Evaluate_Operand --
   ----------------------

   procedure Evaluate_Operand
     (Assembly : Aqua.Assembler.Assembly;
      Tree     : Aquarius.Programs.Program_Tree;
      Relative : Boolean)
   is
      use Aquarius.Programs;
      Op : constant Program_Tree := Tree.Chosen_Tree;
   begin
      if Op.Name = "identifier" then
         Assembly.Append
           (Assembly.Reference_Label
              (Op.Text, Relative));
      elsif Op.Name = "integer" then
         Assembly.Append
           (Aqua.Word'Value (Op.Text));
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
                    Assembly.Get_Register (Operand_Tree.Text));
         else
            return (Indexed, False, 7);
         end if;
      elsif Operand_Name = "deferred" then
         if Assembly.Is_Register
           (Operand_Tree.Program_Child ("identifier").Text)
         then
            return (Register, True,
                    Assembly.Get_Register
                      (Operand_Tree.Program_Child ("identifier").Text));
         else
            return (Indexed, True, 7);
         end if;
      elsif Operand_Name = "autoincrement" then
         return (Autoincrement, False,
                 Assembly.Get_Register
                   (Operand_Tree.Program_Child ("identifier").Text));
      elsif Operand_Name = "autodecrement" then
         return (Autodecrement, False,
                 Assembly.Get_Register
                   (Operand_Tree.Program_Child ("identifier").Text));
      elsif Operand_Name = "autoincrement_deferred" then
         return (Autoincrement, True,
                 Assembly.Get_Register
                   (Operand_Tree.Program_Child ("identifier").Text));
      elsif Operand_Name = "autodecrement_deferred" then
         return (Autodecrement, True,
                 Assembly.Get_Register
                   (Operand_Tree.Program_Child ("identifier").Text));
      elsif Operand_Name = "indexed" then
         return (Indexed, False,
                 Assembly.Get_Register
                   (Operand_Tree.Program_Child ("identifier").Text));
      elsif Operand_Name = "indexed_deferred" then
         return (Indexed, True,
                 Assembly.Get_Register
                   (Operand_Tree.Program_Child ("identifier").Text));
      elsif Operand_Name = "immediate" then
         return (Autoincrement, False, 7);
      elsif Operand_Name = "absolute" then
         return (Autoincrement, True, 7);
      elsif Operand_Name = "string" then
         return (Autoincrement, False, 7);
      else
         raise Constraint_Error
           with "unknown mode: " & Operand_Name;
      end if;
   end Get_Operand;

   -------------------
   -- Place_Operand --
   -------------------

   procedure Place_Operand
     (Arg : Aquarius.Programs.Program_Tree)
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
            null;
         else
            Assembly.Append
              (Assembly.Reference_Label (Operand_Tree.Text, True));
         end if;
      elsif Operand_Name = "deferred" then
         if Assembly.Is_Register
           (Operand_Tree.Program_Child ("identifier").Text)
         then
            null;
         else
            Assembly.Append
              (Assembly.Reference_Label
                 (Operand_Tree.Program_Child ("identifier").Text, True));
         end if;
      elsif Operand_Name = "indexed"
        or else Operand_Name = "indexed_deferred"
      then
         Evaluate_Operand (Assembly, Operand_Tree.Program_Child ("operand"),
                           False);
      elsif Operand_Name = "immediate"
        or else Operand_Name = "absolute"
      then
         Assembly.Append
           (Evaluate_Expression
              (Assembly,
               Operand_Tree.Program_Child ("expression")));
      elsif Operand_Name = "string" then
         Assembly.Append
           (Assembly.Reference_String (Operand_Tree.Text));
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

end Aquarius.Plugins.Macro_11.Assemble;
