with Aquarius.Entries.Objects;
with Aquarius.Trees.Properties;
with Aquarius.Types.Maps;
with Aquarius.Values;

package body Ada_Plugin.Ch06 is

   ---------------------------
   -- Formal_Argument_After --
   ---------------------------

   procedure Formal_Argument_After
     (Arg  : Program_Tree)
   is
      Defining_Identifier_List : constant Program_Tree :=
        Arg.Program_Child ("defining_identifier_list");
      Defining_Identifiers     : constant Array_Of_Program_Trees :=
        Defining_Identifier_List.Direct_Children
        ("defining_identifier");
      Argument_Type_Tree       : constant Program_Tree :=
        Arg.Program_Child ("subtype_mark");
      Access_Type_Tree       : constant Program_Tree :=
        Arg.Program_Child ("access_definition");
      pragma Unreferenced (Access_Type_Tree);
   begin
      if Argument_Type_Tree /= null and then
        Argument_Type_Tree.Has_Type
      then
         declare
            Arg_Type   : constant Aquarius.Types.Aquarius_Type :=
              Argument_Type_Tree.Get_Type;
         begin
            for I in Defining_Identifiers'Range loop
               declare
                  use Aquarius.Types.Maps;
                  Arg_Decl        : constant Program_Tree :=
                    Program_Tree (Defining_Identifiers (I).First_Leaf);
                  Arg_Name   : constant String := Arg_Decl.Text;
                  Arg_Std_Name    : constant String :=
                    Arg_Decl.Standard_Text;
                  New_Entry       : constant Aquarius.Entries.Table_Entry :=
                    Aquarius.Entries.Objects.New_Object_Entry
                    (Arg_Std_Name, Arg_Decl, Arg_Type,
                     Aquarius.Values.No_Value, False);
               begin
                  New_Entry.Set_Display_Name (Arg_Name);
                  Arg.Symbol_Table.Insert (New_Entry);
                  Arg_Decl.Set_Entry (New_Entry);
                  if  Arg.Has_Type then
                     Add_From_Type (Arg.Get_Type, Arg_Type, False);
                  end if;
               end;
            end loop;
         end;

      end if;

   end Formal_Argument_After;

   --------------------------------
   -- Function_Declaration_After --
   --------------------------------

   procedure Function_Declaration_After
     (Function_Declaration : Program_Tree)
   is
      Spec : constant Program_Tree :=
               Function_Declaration.Program_Child
                 ("function_specification");
   begin
      if Function_Declaration.Program_Child ("subprogram_body") /= null then
         Spec.Get_Entry.Set_Implementation (Function_Declaration);
      end if;

   end Function_Declaration_After;

   --------------------------------------------------
   -- Function_Specification_After_Type_Indication --
   --------------------------------------------------

   procedure Function_Specification_After_Type_Indication
     (Spec       : Program_Tree;
      Indication : Program_Tree)
   is
      use Aquarius.Entries, Aquarius.Entries.Objects;
      use Aquarius.Types;
      Proc_Type  : constant Aquarius_Type := Spec.Get_Type;
      Return_Type : constant Aquarius_Type :=
        Indication.Get_Type;
   begin
      Aquarius.Types.Maps.Set_Return_Type (Proc_Type, Return_Type);
   end Function_Specification_After_Type_Indication;

   -----------------------------------
   -- Function_Specification_Before --
   -----------------------------------

   procedure Function_Specification_Before
     (Spec : Program_Tree)
   is
      use Aquarius.Types.Maps;
   begin
      Spec.Set_Type (New_Map_Type);
      Spec.Create_Symbol_Table;
   end Function_Specification_Before;

   ---------------------------------
   -- Procedure_Declaration_After --
   ---------------------------------

   procedure Procedure_Declaration_After
     (Procedure_Declaration : Program_Tree)
   is
      Spec : constant Program_Tree :=
               Procedure_Declaration.Program_Child
                 ("procedure_specification");
   begin
      if Procedure_Declaration.Program_Child ("subprogram_body") /= null then
         Spec.Get_Entry.Set_Implementation (Procedure_Declaration);
      end if;

   end Procedure_Declaration_After;

   ----------------------------------------
   -- Procedure_Spec_After_Defining_Name --
   ----------------------------------------

   procedure Procedure_Spec_After_Defining_Name
     (Procedure_Spec, Procedure_Name  : Program_Tree)
   is
      use Aquarius.Entries;

      Reference     : constant Program_Tree :=
        Procedure_Name.Program_Child ("defining_qualified_reference");
      Defining_Name : constant Aquarius.Programs.Program_Tree :=
        Program_Tree
        (Reference.Property (Plugin.Last_Identifier_Property));
      Procedure_Entry : Aquarius.Entries.Table_Entry;
      Parent_Entry  : Aquarius.Entries.Table_Entry;
      pragma Warnings (Off, Parent_Entry);
      Compilation_Unit : constant Program_Tree :=
        Program_Tree (Procedure_Spec.Property
                        (Plugin.Compilation_Unit_Property));
   begin

      if Reference.Has_Entry then
         Parent_Entry := Reference.Get_Entry;
      end if;

      Procedure_Entry :=
        Aquarius.Entries.Objects.New_Object_Entry
        (Defining_Name.Standard_Text,
         Procedure_Spec,
         Procedure_Spec.Get_Type,
         Aquarius.Values.No_Value,
         True);
      Procedure_Entry.Set_Display_Name (Defining_Name.Text);

      Procedure_Spec.Set_Entry (Procedure_Entry);

      if not Compilation_Unit.Has_Entry then
         Compilation_Unit.Set_Entry (Procedure_Entry);
      end if;

      Compilation_Unit.Symbol_Table.Insert (Procedure_Entry);

      declare
         Project : constant Aquarius.Projects.Aquarius_Project :=
                     Aquarius.Trees.Properties.Get_Project
                       (Compilation_Unit.all);
      begin
         Project.Add_Entry (Procedure_Entry);
      end;

   end Procedure_Spec_After_Defining_Name;

   ------------------------------------
   -- Procedure_Specification_Before --
   ------------------------------------

   procedure Procedure_Specification_Before
     (Spec : Program_Tree)
   is
      use Aquarius.Types.Maps;
   begin
      Spec.Set_Type (New_Map_Type (Get_Void_Type));
      Spec.Create_Symbol_Table;
   end Procedure_Specification_Before;

   --------------------------
   -- Spec_After_Arguments --
   --------------------------

   procedure Spec_After_Arguments
     (Spec : Program_Tree;
      Args : Program_Tree)
   is
      pragma Unreferenced (Args);

      use Aquarius.Entries;

      Stack_Object : constant Entry_Constraint'Class :=
                       Create_Proposition_Constraint
                         (Aquarius.Entries.Objects.Is_Stack_Object'Access);
      Entries      : constant Array_Of_Entries :=
                       Spec.Symbol_Table.Search (Stack_Object);
      Offset  : Positive := 1;
   begin
      for I in Entries'Range loop
         Aquarius.Entries.Objects.Set_Frame_Offset (Entries (I), Offset);
         Offset := Offset + 1;
      end loop;
   end Spec_After_Arguments;

   ---------------------------
   -- Spec_Before_Arguments --
   ---------------------------

   procedure Spec_Before_Arguments
     (Spec : Program_Tree;
      Args : Program_Tree)
   is
   begin
      if Spec.Has_Type then
         Args.Set_Type (Spec.Get_Type);
      end if;
   end Spec_Before_Arguments;

   ----------------------------
   -- Subprogram_Before_Body --
   ----------------------------

   procedure Subprogram_Before_Body
     (Spec       : Program_Tree;
      Subprogram : Program_Tree)
   is
   begin
      Subprogram.Set_Symbol_Table
        (Spec.First_Program_Child.Symbol_Table);
   end Subprogram_Before_Body;

end Ada_Plugin.Ch06;
