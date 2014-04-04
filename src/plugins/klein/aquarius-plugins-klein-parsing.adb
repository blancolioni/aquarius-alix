with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Aquarius.Entries.Packages;
with Aquarius.Entries.Types;
with Aquarius.Entries.Objects;
with Aquarius.Errors;
with Aquarius.Loader;
with Aquarius.Programs;                 use Aquarius.Programs;
with Aquarius.Projects;
with Aquarius.Source;
with Aquarius.Target;
with Aquarius.Trees.Properties;         use Aquarius.Trees.Properties;
with Aquarius.Types.Maps;
with Aquarius.UI;
with Aquarius.Values;

with Aquarius.Plugins.Klein.Inference;
with Aquarius.Plugins.Klein.Types;

package body Aquarius.Plugins.Klein.Parsing is

   function To_File_Name (Package_Name : String) return String;

   ------------------
   -- Block_Before --
   ------------------

   procedure Block_Before
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Program_Tree := Program_Tree (Target);
   begin
      Tree.Create_Symbol_Table;
   end Block_Before;

   ------------------------------------
   -- Defining_Identifier_List_After --
   ------------------------------------

   procedure Defining_Identifier_List_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Program_Tree := Program_Tree (Target);
      Identifier_Trees : constant Array_Of_Program_Trees :=
        Tree.Direct_Children ("identifier");
   begin
      for I in Identifier_Trees'Range loop
         Identifier_Trees (I).Set_Property (Plugin.Object_Property);
      end loop;

   end Defining_Identifier_List_After;

   ---------------------------------
   -- Defining_Package_Name_After --
   ---------------------------------

   procedure Defining_Package_Name_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Entries;
      use Ada.Strings.Unbounded;
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item            : constant Aquarius.Programs.Program_Tree :=
        Program_Tree (Tree);
      Defining_Name : constant Aquarius.Programs.Program_Tree :=
        Item.Program_Child ("identifier");
      Package_Entry : constant Aquarius.Entries.Table_Entry :=
        Aquarius.Entries.Packages.New_Package_Entry
        (Defining_Name.Standard_Text, null, Item,
         Item.Symbol_Table);
   begin
      Item.Set_Entry (Package_Entry);
   end Defining_Package_Name_After;

   --------------------------------
   -- List_Of_Declarations_After --
   --------------------------------

   procedure List_Of_Declarations_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Entries, Aquarius.Entries.Objects;
      use Aquarius.Types, Aquarius.Entries.Types;
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item : constant Program_Tree := Program_Tree (Tree);
      Is_Object : constant Entry_Constraint'Class :=
        Object_Entry_Constraint;
      Entries : constant Array_Of_Entries :=
        Item.Symbol_Table.Search (Is_Object);
      Offset  : Natural := 0;
   begin
      for I in Entries'Range loop
         declare
            V  : constant Table_Entry   := Entries (I);
            T  : constant Aquarius_Type := Object_Entry_Type (V);
         begin
            if T = null then
               raise Constraint_Error with
                 "value " & V.Name & " has no type";
            end if;

            Offset := Offset + Aquarius.Target.Size_In_Words (T.Size_Bits);
            Set_Frame_Offset (V, Offset);
         end;
      end loop;
      Item.Symbol_Table.Set_Value_Size (Offset);
   end List_Of_Declarations_After;

   ----------------------
   -- Named_Type_After --
   ----------------------

   procedure Named_Type_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item          : constant Aquarius.Programs.Program_Tree :=
        Program_Tree (Tree);
      Type_Name     : constant Aquarius.Programs.Program_Tree :=
        Item.Program_Child ("qualified_identifier");
      Type_Entry    : Aquarius.Entries.Table_Entry;
   begin
      if Type_Name.Has_Entry then
         Type_Entry := Type_Name.Get_Entry;
         if not Aquarius.Entries.Types.Is_Type_Entry (Type_Entry) then
            Aquarius.Errors.Error (Type_Name,
                                   "subtype mark required in this context");
         else
            Set_Type (Tree.all, Aquarius.Entries.Types.Get_Type (Type_Entry));
         end if;
      end if;
   end Named_Type_After;

   ---------------------------
   -- Numeric_Literal_After --
   ---------------------------

   procedure Numeric_Literal_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Types;
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item : constant Program_Tree := Program_Tree (Tree);
   begin
      Inference.Set_Inferred_Types
        (Item,
         Single_Possible_Type
           (Plugins.Klein.Types.Universal_Integer));
   end Numeric_Literal_After;

   --------------------------------
   -- Object_Statement_After --
   --------------------------------

   procedure Object_Statement_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Types;
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Tree);
      Name : constant Aquarius.Programs.Program_Tree :=
        Item.Program_Child ("name");
      Expression : constant Aquarius.Programs.Program_Tree :=
        Item.Program_Child ("expression");
      Result : Aquarius_Type;
      Possibles  : Possible_Types;
   begin

      if Expression /= null then
         Possibles :=
           Inference.Get_Inferred_Types (Item.Program_Child ("expression"));
         if Possibles.Count = 1 then
            Result := Possibles.Get_Type;
            Name.Set_Type (Result);
            Expression.Set_Type (Result);
         elsif Possibles.Count = 0 then
            Aquarius.Errors.Error (Tree, "no candidate types match");
         else
            Aquarius.Errors.Error (Tree, "ambiguous assignment");
         end if;
      end if;
   end Object_Statement_After;

   ----------------------------------------
   -- Object_Statement_Before_Expression --
   ----------------------------------------

   procedure Object_Statement_Before_Expression
     (Object_Actionable     : not null access Actions.Actionable'Class;
      Expression_Actionable : not null access Actions.Actionable'Class)
   is
      use Aquarius.Types;
      Object     : constant Program_Tree := Program_Tree (Object_Actionable);
      Expression : constant Program_Tree :=
        Program_Tree (Expression_Actionable);
      Name       : constant Program_Tree :=
        Object.Program_Child ("name");
      Inferred_Target_Type : constant Possible_Types :=
        Inference.Get_Inferred_Types (Name);
   begin
      Inference.Set_Possible_Types (Expression, Inferred_Target_Type);
   end Object_Statement_Before_Expression;

   -----------------------------
   -- Package_Reference_After --
   -----------------------------

   procedure Package_Reference_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Entries;
      use Ada.Strings.Unbounded;
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item            : constant Aquarius.Programs.Program_Tree :=
        Program_Tree (Tree);
      Children        : constant Aquarius.Programs.Array_Of_Program_Trees :=
        Item.Direct_Children (Skip_Separators => True);
      Package_Name    : Unbounded_String;
      Grammar        : constant Aquarius.Grammars.Aquarius_Grammar :=
        Get_Grammar (Item);
      UI             : constant Aquarius.UI.Aquarius_UI :=
        Get_UI (Item.all);
      Project        : constant Aquarius.Projects.Aquarius_Project :=
        Get_Project (Item.all);
   begin

      for I in Children'Range loop
         declare
            Standard_Name  : constant String := Children (I).Standard_Text;
            Withed_Package : Program_Tree;
         begin

            if I = Children'First then
               Package_Name := To_Unbounded_String (Standard_Name);
            else
               Package_Name := Package_Name & '.' & Standard_Name;
            end if;

            Withed_Package :=
              Aquarius.Loader.Load_From_File
                (Grammar, Project, UI,
                 To_File_Name (To_String (Package_Name)));

            if Withed_Package = null then
               Aquarius.Errors.Error (Item,
                                      "file """ &
                                        To_File_Name (To_String
                                                        (Package_Name)) &
                                        """ not found");
               return;
            end if;

            declare
               Ref : constant Aquarius.Entries.Table_Entry :=
                 Aquarius.Entries.Packages.New_Package_Reference
                 (Item,
                  Withed_Package.Program_Child ("program_unit").Get_Entry);
            begin
               Item.Symbol_Table.Insert (Ref);
            end;
         end;
      end loop;

   end Package_Reference_After;

   ------------------------
   -- Package_Spec_After --
   ------------------------

   procedure Package_Spec_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item   : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Tree);
      Package_Name : constant Program_Tree :=
        Item.Program_Child ("defining_package_name");
   begin
      Item.Set_Entry (Package_Name.Get_Entry);
   end Package_Spec_After;

   ---------------------------------
   -- Procedure_Declaration_After --
   ---------------------------------

   procedure Procedure_Declaration_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item   : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Tree);
      Procedure_Definition : constant Program_Tree :=
        Item.Program_Child ("procedure_definition");
   begin
      Item.Set_Entry (Procedure_Definition.Get_Entry);
   end Procedure_Declaration_After;

   --------------------------------
   -- Procedure_Definition_After --
   --------------------------------

   procedure Procedure_Definition_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item   : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Tree);
      Defining_Name : constant Aquarius.Programs.Program_Tree :=
        Item.Program_Child ("identifier");
      Procedure_Type : constant Aquarius.Types.Aquarius_Type :=
                         Aquarius.Types.Maps.New_Map_Type
                           ((1 => Klein.Types.Universal_Integer),
                            Klein.Types.Universal_Integer);
      Procedure_Entry : constant Aquarius.Entries.Table_Entry :=
        Aquarius.Entries.Objects.New_Object_Entry
        (Defining_Name.Standard_Text, Item, Procedure_Type,
         Aquarius.Values.No_Value, True);
   begin
      Item.Set_Entry (Procedure_Entry);
   end Procedure_Definition_After;

   ------------------------
   -- Program_Unit_After --
   ------------------------

   procedure Program_Unit_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item   : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Tree);
      Subprogram : constant Program_Tree :=
        Item.Program_Child ("subprogram");
   begin
      Item.Set_Entry (Subprogram.Get_Entry);
   end Program_Unit_After;

   -------------------------
   -- Program_Unit_Before --
   -------------------------

   procedure Program_Unit_Before
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item   : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Tree);
      Standard_Ref : constant Aquarius.Entries.Table_Entry :=
        Aquarius.Entries.Packages.New_Package_Reference
        (Tree, Plugin.Get_Standard_Entry ("standard"));
   begin
      Item.Create_Symbol_Table;
      Item.Symbol_Table.Insert (Standard_Ref);
      Aquarius.Entries.Packages.Make_Visible (Standard_Ref);
      declare
         Name : constant String :=
           Aquarius.Source.Get_File_Name
           (Aquarius.Source.Get_Source_File
              (Item.Get_Location));
      begin
         if Ada.Strings.Fixed.Index (Name, ".kls") /= 0 then
            Item.Set_Property (Plugin.Package_Spec_Property);
         elsif Ada.Strings.Fixed.Index (Name, ".klb") /= 0 then
            Item.Set_Property (Plugin.Package_Body_Property);
         end if;
      end;

   end Program_Unit_Before;

   --------------------------------
   -- Qualified_Identifier_After --
   --------------------------------

   procedure Qualified_Identifier_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Entries;
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item            : constant Aquarius.Programs.Program_Tree :=
        Program_Tree (Tree);
      Children        : constant Aquarius.Programs.Array_Of_Program_Trees :=
        Item.Direct_Children (Skip_Separators => True);
      Current_Entry   : Aquarius.Entries.Table_Entry;
   begin
      for I in Children'Range loop
         declare
            Display_Name  : constant String := Children (I).Text;
            Standard_Name : constant String := Children (I).Standard_Text;
            Child_Entry   : Table_Entry;
         begin
            if Current_Entry = null then
               Child_Entry := Item.Symbol_Table.Retrieve (Standard_Name);
               if Child_Entry = null then
                  Aquarius.Errors.Error
                    (Item,
                     """" & Display_Name & """ is undefined");
                  return;
               end if;
            elsif Aquarius.Entries.Packages.Is_Package_Reference
              (Current_Entry)
            then
               declare
                  Table : constant Aquarius.Entries.Symbol_Table :=
                            Aquarius.Entries.Packages.Get_Symbol_Table
                              (Current_Entry);
               begin
                  Child_Entry := Table.Retrieve (Standard_Name);
                  if Child_Entry = null then
                     Aquarius.Errors.Error
                       (Item,
                        """" & Display_Name &
                          """ is not declared in package " &
                          Current_Entry.Name);
                     return;
                  end if;
               end;
            else
               Aquarius.Errors.Error
                 (Children (I - 1),
                  "invalid prefix in selected component " &
                    Display_Name);
               return;
            end if;
            Current_Entry := Child_Entry;
         end;
      end loop;

      Item.Set_Entry (Current_Entry);
   end Qualified_Identifier_After;

   ----------------------------
   -- Record_Component_After --
   ----------------------------

   procedure Record_Component_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item            : constant Program_Tree := Program_Tree (Tree);
      Declarations    : constant Array_Of_Program_Trees :=
        Item.Program_Child
        ("defining_identifier_list").Direct_Children ("identifier");
      Type_Definition : constant Program_Tree :=
        Item.Program_Child ("named_type");
      Record_Type : constant Aquarius.Types.Aquarius_Type :=
        Item.Get_Type;
   begin
      for I in Declarations'Range loop
         declare
            Var_Decl        : constant Program_Tree :=
              Program_Tree (Declarations (I).First_Leaf);
            Variable_Name   : constant String :=
              Var_Decl.Text;
            Var_Std_Name    : constant String :=
              Var_Decl.Standard_Text;
            Variable_Type   : constant Aquarius.Types.Aquarius_Type :=
              Type_Definition.Get_Type;
            New_Entry       : constant Aquarius.Entries.Table_Entry :=
              Aquarius.Entries.Objects.New_Object_Entry
              (Var_Std_Name, Declarations (I), Variable_Type,
               Aquarius.Values.No_Value, False);
         begin
            New_Entry.Set_Display_Name (Variable_Name);
            Aquarius.Plugins.Klein.Types.Add_Component (Record_Type,
                                                        New_Entry);
         end;
      end loop;
   end Record_Component_After;

   ------------------------
   -- Record_Type_Before --
   ------------------------

   procedure Record_Type_Before
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
   begin
      Set_Type (Tree.all,
                Aquarius.Plugins.Klein.Types.New_Record_Type);
   end Record_Type_Before;

   ----------------------------------
   -- Record_Type_Before_Component --
   ----------------------------------

   procedure Record_Type_Before_Component
     (Record_Type_Actionable : not null access Actions.Actionable'Class;
      Component_Actionable   : not null access Actions.Actionable'Class)
   is
      Record_Type : constant Program_Tree :=
        Program_Tree (Record_Type_Actionable);
      Component   : constant Program_Tree :=
        Program_Tree (Component_Actionable);
   begin
      Component.Set_Type (Record_Type.Get_Type);
   end Record_Type_Before_Component;

   ----------------------
   -- Subprogram_After --
   ----------------------

   procedure Subprogram_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item   : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Tree);
      Subprogram : constant Program_Tree := Item.Chosen_Tree;
   begin
      Item.Set_Entry (Subprogram.Get_Entry);
   end Subprogram_After;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name (Package_Name : String) return String is
      Result : String := Package_Name & ".kls";
   begin
      for I in Package_Name'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         end if;
      end loop;
      return Result;
   end To_File_Name;

   ----------------
   -- Type_After --
   ----------------

   procedure Type_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item       : constant Aquarius.Programs.Program_Tree :=
        Program_Tree (Tree);
   begin
      Item.Set_Type (Item.Chosen_Tree.Get_Type);
   end Type_After;

   ----------------------------
   -- Type_Declaration_After --
   ----------------------------

   procedure Type_Declaration_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item            : constant Program_Tree := Program_Tree (Tree);
      Identifier      : constant Program_Tree :=
        Program_Tree (Item.Program_Child ("identifier").First_Leaf);
      Type_Definition : constant Program_Tree :=
        Item.Program_Child ("type_definition");
      Type_Name       : constant String := Identifier.Text;
      Standard_Name   : constant String := Identifier.Standard_Text;
      Defined_Type    : constant Aquarius.Types.Aquarius_Type :=
        Type_Definition.Get_Type;
      Type_Entry      : constant Aquarius.Entries.Table_Entry :=
        Aquarius.Entries.Types.New_Type_Entry (Standard_Name,
                                               Tree,
                                               Defined_Type);
   begin
      Type_Entry.Set_Display_Name (Type_Name);
      Item.Symbol_Table.Insert (Type_Entry);
   end Type_Declaration_After;

   ----------------------------
   -- Until_Statement_Before --
   ----------------------------

   procedure Until_Statement_Before
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Types;
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Tree);
      Condition     : constant Program_Tree :=
        Item.Program_Child ("expression");
   begin
      Inference.Set_Possible_Types
        (Condition,
         Single_Possible_Type (Types.Universal_Boolean));
   end Until_Statement_Before;

   --------------------------------
   -- Variable_Declaration_After --
   --------------------------------

   procedure Variable_Declaration_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item            : constant Program_Tree := Program_Tree (Tree);
      Declarations    : constant Array_Of_Program_Trees :=
        Item.Program_Child
        ("defining_identifier_list").Direct_Children ("identifier");
      Type_Definition : constant Program_Tree :=
        Item.Program_Child ("named_type");
   begin
      for I in Declarations'Range loop
         declare
            Var_Decl        : constant Program_Tree :=
              Program_Tree (Declarations (I).First_Leaf);
            Variable_Name   : constant String :=
              Var_Decl.Text;
            Var_Std_Name    : constant String :=
              Var_Decl.Standard_Text;
            Variable_Type   : constant Aquarius.Types.Aquarius_Type :=
              Type_Definition.Get_Type;
            New_Entry       : constant Aquarius.Entries.Table_Entry :=
              Aquarius.Entries.Objects.New_Object_Entry
              (Var_Std_Name, Declarations (I), Variable_Type,
               Aquarius.Values.No_Value, False);
         begin
            New_Entry.Set_Display_Name (Variable_Name);
            Item.Symbol_Table.Insert (New_Entry);
         end;
      end loop;
   end Variable_Declaration_After;

end Aquarius.Plugins.Klein.Parsing;
