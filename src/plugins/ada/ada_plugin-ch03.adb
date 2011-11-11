with Aquarius.Trees.Properties;         use Aquarius.Trees.Properties;

with Aquarius.Entries;
with Aquarius.Entries.Objects;
with Aquarius.Entries.Types;
with Aquarius.Errors;
with Aquarius.Values;

with Aquarius.Types.Access_Types;
with Aquarius.Types.Arrays;
with Aquarius.Types.Enumeration;
with Aquarius.Types.Errors;
with Aquarius.Types.Integral;
with Aquarius.Types.Maps;
with Aquarius.Types.Private_Types;
with Aquarius.Types.Records;
with Aquarius.Types.Tagged_Types;

package body Ada_Plugin.Ch03 is

   ------------------------------------
   -- Access_Type_After_Subtype_Mark --
   ------------------------------------

   procedure Access_Type_After_Subtype_Mark
     (Access_Type_Tree  : Program_Tree;
      Subtype_Mark      : Program_Tree)
   is
      use Aquarius.Types;
      Access_Modifier    : constant Program_Tree :=
        Access_Type_Tree.Program_Child ("general_access_modifier");
      Access_All         : constant Boolean :=
        Access_Modifier /= null and then
        Access_Modifier.Program_Child ("all") /= null;
--        Constant_Token     : constant Program_Tree :=
--          Access_Modifier.Program_Child ("constant");
      Access_To          : Aquarius_Type;
      Access_Type        : Aquarius_Type;
   begin
      if Subtype_Mark.Has_Type then
         Access_To := Subtype_Mark.Get_Type;
         if Access_Type_Tree.Has_Type then
            Access_Type := Access_Type_Tree.Get_Type;
            Aquarius.Types.Access_Types.Set_Access_To
              (Access_Type, Access_To);
            Aquarius.Types.Access_Types.Set_Access_All
              (Access_Type, Access_All);
         else
            Access_Type :=
              Aquarius.Types.Access_Types.New_Access_Type
              (Access_To, Access_All, False);
            Access_Type_Tree.Set_Type (Access_Type);
         end if;
      else
         Access_Type := Aquarius.Types.Access_Types.New_Access_Type
           (Aquarius.Types.Private_Types.New_Private_Type,
            Access_All, False);
         Access_Type_Tree.Set_Type (Access_Type);
      end if;
   end Access_Type_After_Subtype_Mark;

   ------------------------
   -- Array_Type_Before --
   ------------------------

   procedure Array_Type_Definition_Before
     (Array_Type_Tree : Program_Tree)
   is
      Array_Type      : constant Aquarius.Types.Aquarius_Type :=
        Aquarius.Types.Arrays.New_Array_Type;
      Type_Declaration : Program_Tree;
   begin
      if Array_Type_Tree.Has_Property
        (Plugin.Type_Declaration_Property)
      then
         Type_Declaration :=
           Program_Tree (Array_Type_Tree.Property
                           (Plugin.Type_Declaration_Property));
         Aquarius.Entries.Types.Complete_Type
           (Type_Declaration.Get_Entry,
            Array_Type);
      end if;

      Array_Type_Tree.Set_Type (Array_Type);
   end Array_Type_Definition_Before;

   ------------------
   -- Block_Before --
   ------------------

   procedure Block_Before
     (Block : Program_Tree)
   is
   begin
      Block.Create_Symbol_Table;
   end Block_Before;

   --------------------------------
   -- Component_Definition_After --
   --------------------------------

   procedure Component_Definition_After
     (Item : Program_Tree)
   is
   begin

      if Item.Program_Child ("null") /= null then
         return;
      end if;

      declare
         Declarations    : constant Array_Of_Program_Trees :=
           Item.Program_Child
           ("defining_identifier_list").Direct_Children ("identifier");
         Type_Indication : constant Program_Tree :=
           Item.Program_Child ("type_indication");
         Record_Type : Aquarius.Types.Aquarius_Type;
      begin
         if not Type_Indication.Has_Type or else
           not Item.Has_Type
         then
            return;
         end if;

         Record_Type := Item.Get_Type;
         for I in Declarations'Range loop
            declare
               Var_Decl        : constant Program_Tree :=
                 Program_Tree (Declarations (I).First_Leaf);
               Variable_Name   : constant String :=
                 Var_Decl.Text;
               Var_Std_Name    : constant String :=
                 Var_Decl.Standard_Text;
               Variable_Type   : constant Aquarius.Types.Aquarius_Type :=
                 Type_Indication.Get_Type;
               New_Entry       : constant Aquarius.Entries.Table_Entry :=
                 Aquarius.Entries.Objects.New_Object_Entry
                 (Var_Std_Name, Declarations (I), Variable_Type,
                  Aquarius.Values.No_Value, False);
            begin
               New_Entry.Set_Display_Name (Variable_Name);
               Aquarius.Types.Records.Add_Component (Record_Type, New_Entry);
            end;
         end loop;
      end;
   end Component_Definition_After;

   --------------------------------------------
   -- Derived_Type_After_Qualified_Reference --
   --------------------------------------------

   procedure Derived_Type_After_Qualified_Reference
     (Derived_Type_Tree : Program_Tree;
      Reference_Tree    : Program_Tree)
   is
      Base_Type_Entry   : Aquarius.Entries.Table_Entry;
      Base_Type         : Aquarius.Types.Aquarius_Type;
      Derived_Type      : Aquarius.Types.Aquarius_Type;
   begin
      if Reference_Tree.Has_Entry then
         Base_Type_Entry := Reference_Tree.Get_Entry;
         if Aquarius.Entries.Types.Is_Type_Entry (Base_Type_Entry) then
            Base_Type := Aquarius.Entries.Types.Get_Type (Base_Type_Entry);
            Derived_Type :=
              Aquarius.Types.Create_Derived_Type (Base_Type.all);
            Derived_Type_Tree.Set_Type (Derived_Type);
         else
            Aquarius.Errors.Error (Reference_Tree,
                                   "expected a type name");
            Aquarius.Errors.Error (Reference_Tree,
                                   "found " & Base_Type_Entry.Display_Name);
         end if;
      end if;
   end Derived_Type_After_Qualified_Reference;

   -----------------------------------
   -- Enumerated_Type_After_Literal --
   -----------------------------------

   procedure Enumerated_Type_Definition_After_Enumerated_Literal
     (Enumerated_Type : Program_Tree;
      Literal         : Program_Tree)
   is
      Literal_Name    : constant String := Literal.First_Leaf.Standard_Text;
      Display_Name    : constant String := Literal.First_Leaf.Text;
      Literal_Entry   : Aquarius.Entries.Table_Entry;
   begin
      if Literal.Has_Entry then
         Literal_Entry := Literal.Get_Entry;
         if Literal_Name /= Literal_Entry.Name then
            Literal_Entry.Rename (Literal_Name);
         end if;
      else
         Literal_Entry :=
           Aquarius.Types.Enumeration.New_Enumeration_Literal
           (Enumeration   => Enumerated_Type.Get_Type,
            Declaration   => Literal,
            Name          => Literal_Name);
         Literal.Set_Entry (Literal_Entry);
      end if;
      Literal_Entry.Set_Display_Name (Display_Name);
   end Enumerated_Type_Definition_After_Enumerated_Literal;

   ----------------------------
   -- Enumerated_Type_Before --
   ----------------------------

   procedure Enumerated_Type_Definition_Before
     (Enumerated_Type : Program_Tree)
   is
   begin
      if not Enumerated_Type.Has_Type then
         Enumerated_Type.Set_Type
                   (Aquarius.Types.Enumeration.New_Enumeration_Type);
      end if;
   end Enumerated_Type_Definition_Before;

   ----------------
   -- Fetch_Type --
   ----------------

   --  procedure Fetch_Type
   --    (To, From : access Aquarius.Actions.Actionable'Class)
   --  is
   --     To_Tree   : constant Program_Tree :=
   --       Program_Tree (To);
   --     From_Tree : constant Program_Tree :=
   --       Program_Tree (From);
   --  begin
   --     if Has_Type (From_Tree) then
   --        Set_Type (To_Tree, Get_Type (From_Tree));
   --     end if;
   --  end Fetch_Type;

   ------------------------------------
   -- Interface_Type_After_Interface --
   ------------------------------------

   procedure Interface_Type_After_Interface
     (Interface_Type : Program_Tree;
      Interface_Tree : Program_Tree)
   is
      pragma Unreferenced (Interface_Tree);
   begin
      if not Interface_Type.Has_Type then
         Interface_Type.Set_Type
           (Aquarius.Types.Tagged_Types.New_Interface_Type);
      end if;
   end Interface_Type_After_Interface;

   --------------------------------
   -- List_Of_Declarations_After --
   --------------------------------

   procedure List_Of_Declarations_After
     (Decls : Program_Tree)
   is
      use Aquarius.Entries;

      Stack_Object_Constraint : constant Entry_Constraint'Class :=
        Create_Proposition_Constraint
        (Aquarius.Entries.Objects.Is_Stack_Object'Access);
      Table   : constant Aquarius.Entries.Symbol_Table :=
        Aquarius.Trees.Properties.Get_Symbol_Table (Decls.all);
      Entries : constant Array_Of_Entries :=
        Table.Search (Stack_Object_Constraint);
      Offset  : Positive := 1;
   begin
      for I in Entries'Range loop
         Aquarius.Entries.Objects.Set_Frame_Offset (Entries (I), -Offset);
         Offset := Offset + 1;
      end loop;
   end List_Of_Declarations_After;

   -----------------------------------------------------
   -- Modular_Type_Definition_After_Static_Expression --
   -----------------------------------------------------

   procedure Modular_Type_Definition_After_Static_Expression
     (Modular_Type : Program_Tree;
      Expression   : Program_Tree)
   is
      pragma Unreferenced (Modular_Type);
      pragma Unreferenced (Expression);
   begin
      null;
   end Modular_Type_Definition_After_Static_Expression;

   ------------------------------------
   -- Modular_Type_Definition_Before --
   ------------------------------------

   procedure Modular_Type_Definition_Before
     (Modular_Type : Program_Tree)
   is
      pragma Unreferenced (Modular_Type);
   begin
      null;
   end Modular_Type_Definition_Before;

   -------------------------
   -- Private_Type_Before --
   -------------------------

   procedure Private_Type_Definition_Before
     (Private_Type : Program_Tree)
   is
   begin
      if not Private_Type.Has_Type then
         Private_Type.Set_Type
           (Aquarius.Types.Private_Types.New_Private_Type);
      end if;
   end Private_Type_Definition_Before;

   ----------------------
   -- Range_Type_After --
   ----------------------

   procedure Range_Type_Definition_After
     (Range_Type_Tree : Program_Tree)
   is
      Range_Type      : constant Aquarius.Types.Aquarius_Type :=
        Aquarius.Types.Integral.New_Signed_Integer_Type
        (Aquarius.Values.To_Value (Integer'First),
         Aquarius.Values.To_Value (Integer'Last));
      Type_Declaration : constant Program_Tree :=
        Program_Tree (Range_Type_Tree.Property
                        (Plugin.Type_Declaration_Property));
   begin
      Range_Type_Tree.Set_Type (Range_Type);
      Aquarius.Entries.Types.Complete_Type
        (Type_Declaration.Get_Entry,
         Range_Type);
   end Range_Type_Definition_After;

   ------------------------
   -- Record_Type_Before --
   ------------------------

   procedure Record_Type_Definition_Before
     (Record_Type_Tree : Program_Tree)
   is
      Record_Type      : constant Aquarius.Types.Aquarius_Type :=
        Aquarius.Types.Records.New_Record_Type;
      Type_Declaration : constant Program_Tree :=
        Program_Tree (Record_Type_Tree.Property
                        (Plugin.Type_Declaration_Property));
   begin
      Record_Type_Tree.Set_Type (Record_Type);
      Aquarius.Entries.Types.Complete_Type (Type_Declaration.Get_Entry,
                                            Record_Type);
   end Record_Type_Definition_Before;

   ----------------------------------
   -- Record_Type_Before_Component --
   ----------------------------------

   procedure Record_Type_Definition_Before_Component
     (Record_Type, Component : Program_Tree)
   is
   begin
      if Record_Type.Has_Type then
         Component.Set_Type (Record_Type.Get_Type);
      end if;
   end Record_Type_Definition_Before_Component;

   ---------------------------------------
   -- Subprogram_Type_Definition_Before --
   ---------------------------------------

   procedure Subprogram_Type_Definition_Before
     (Function_Type_Tree : Program_Tree)
   is
      Function_Type      : Aquarius.Types.Aquarius_Type;
   begin
      if Function_Type_Tree.Has_Type then
         Function_Type := Function_Type_Tree.Get_Type;
      else
         Function_Type := Aquarius.Types.Maps.New_Map_Type;
         Function_Type_Tree.Set_Type (Function_Type);
      end if;
   end Subprogram_Type_Definition_Before;

   ----------------------------
   -- Subtype_Declaration_After --
   ----------------------------

   procedure Subtype_Declaration_After
     (Subtype_Declaration : Program_Tree)
   is
      Subtype_Name         : constant Program_Tree :=
        Program_Tree
        (Subtype_Declaration.Program_Child ("subtype_name").First_Leaf);
      Subtype_Definition   : constant Program_Tree :=
        Subtype_Declaration.Program_Child ("type_indication");
      Subtype_Entry        : Aquarius.Entries.Table_Entry;
   begin
      if Subtype_Definition.Has_Type then
         Subtype_Entry :=
           Aquarius.Entries.Types.New_Type_Entry
           (Subtype_Name.Standard_Text, Subtype_Declaration,
            Subtype_Definition.Get_Type);
         Subtype_Entry.Set_Display_Name (Subtype_Name.Text);
         Subtype_Declaration.Set_Entry (Subtype_Entry);
         Subtype_Declaration.Symbol_Table.Insert (Subtype_Entry);
      end if;

   end Subtype_Declaration_After;

   -------------------------------------------
   -- Subtype_Indication_After_Subtype_Mark --
   -------------------------------------------

   procedure Subtype_Indication_After_Subtype_Mark
     (Indication_Tree, Mark_Tree : Program_Tree)
   is
      use Aquarius.Types;
      Subtype_Indication : Aquarius_Type;
   begin
      if Mark_Tree.Has_Type then
         Subtype_Indication := Mark_Tree.Get_Type;
         Indication_Tree.Set_Type (Subtype_Indication);
         Indication_Tree.Set_Entry (Mark_Tree.Get_Entry);
      end if;
   end Subtype_Indication_After_Subtype_Mark;

   ------------------------
   -- Subtype_Mark_After --
   ------------------------

   procedure Subtype_Mark_After
     (Subtype_Tree : Program_Tree)
   is
      Reference_Tree : constant Program_Tree :=
        Subtype_Tree.Program_Child ("object_reference");
      Subtype_Entry  : Aquarius.Entries.Table_Entry;
   begin
      if Reference_Tree.Has_Entry then
         Subtype_Entry := Reference_Tree.Get_Entry;
         if Aquarius.Entries.Types.Is_Type_Entry (Subtype_Entry) then
            Subtype_Tree.Set_Entry (Subtype_Entry);
            Subtype_Tree.Set_Type
                      (Aquarius.Entries.Types.Get_Type (Subtype_Entry));
         else
            Aquarius.Errors.Error (Subtype_Tree, "expected a type name");
            Aquarius.Errors.Error
              (Subtype_Tree, "found " & Subtype_Entry.Name);
         end if;
      end if;
   end Subtype_Mark_After;

   --------------------
   -- Transfer_Entry --
   --------------------

   procedure Transfer_Entry (Source, Dest : Program_Tree) is
   begin
      Aquarius.Entries.Transfer_Entry (Source.all, Dest.all);
   end Transfer_Entry;

   -------------------
   -- Transfer_Type --
   -------------------

   procedure Transfer_Type (Source, Dest : Program_Tree) is
   begin
      Aquarius.Types.Transfer_Type (Source.all, Dest.all);
   end Transfer_Type;

   ----------------------------
   -- Type_Declaration_After --
   ----------------------------

   procedure Type_Declaration_After
     (Type_Declaration : Program_Tree)
   is
      Type_Header       : constant Program_Tree :=
        Type_Declaration.Program_Child ("type_header");
      Type_Definition   : constant Program_Tree :=
        Type_Declaration.Program_Child ("type_definition");
      Type_Entry        : constant Aquarius.Entries.Table_Entry :=
        Type_Header.Get_Entry;
      Defined_Type      : constant Aquarius.Types.Aquarius_Type :=
        Type_Definition.Get_Type;
   begin
      Aquarius.Entries.Types.Complete_Type (Type_Entry, Defined_Type);
      Type_Declaration.Set_Entry (Type_Entry);
   end Type_Declaration_After;

   -----------------------------
   -- Type_Declaration_Before --
   -----------------------------

   procedure Type_Declaration_Before
     (Type_Declaration : Program_Tree)
   is
   begin
      Type_Declaration.Set_Property (Plugin.Type_Declaration_Property,
                                     Type_Declaration);
   end Type_Declaration_Before;

   ---------------------------
   -- Type_Definition_After --
   ---------------------------

   procedure Type_Definition_After
     (Type_Definition : Program_Tree)
   is
      Chosen          : constant Program_Tree :=
        Type_Definition.Chosen_Tree;
   begin
      if Chosen.Has_Type then
         Type_Definition.Set_Type (Type_Definition.Chosen_Tree.Get_Type);
      else
         Aquarius.Errors.Error (Chosen, "no type");
         Type_Definition.Set_Type (Aquarius.Types.Errors.New_Error_Type);
      end if;
   end Type_Definition_After;

   ----------------------------
   -- Type_Definition_Before --
   ----------------------------

   procedure Type_Definition_Before
     (Type_Definition : Program_Tree)
   is
   begin
      if Type_Definition.Has_Entry then
         Type_Definition.Program_Child (1).Set_Entry
           (Type_Definition.Get_Entry);
      end if;
   end Type_Definition_Before;

   -----------------------
   -- Type_Header_After --
   -----------------------

   procedure Type_Header_After
     (Type_Header : Program_Tree)
   is
      Type_Declaration  : constant Program_Tree :=
                            Program_Tree
                              (Type_Header.Property
                                 (Plugin.Type_Declaration_Property));
      Identifier        : constant Program_Tree :=
        Type_Header.Program_Child ("identifier");
      Type_Entry        : Aquarius.Entries.Table_Entry :=
        Type_Header.Symbol_Table.Retrieve (Identifier.Standard_Text);
   begin

      if not Aquarius.Entries.Is_Null (Type_Entry) then
         if not Aquarius.Entries.Types.Is_Type_Entry (Type_Entry) or else
           not Aquarius.Types.Private_Types.Is_Private_Type
           (Aquarius.Entries.Types.Get_Type (Type_Entry)) or else
           not Type_Header.Has_Property (Plugin.Private_Section_Property)
         then
            Aquarius.Errors.Error (Identifier, Type_Entry.Declaration,
                                   "redefinition of " &
                                     Type_Entry.Display_Name,
                                   "original definition of " &
                                     Type_Entry.Display_Name);
            Type_Entry :=
              Aquarius.Entries.Types.New_Type_Entry
              (Identifier.Standard_Text, Identifier);
            Type_Entry.Set_Display_Name (Identifier.Text);
         end if;
      else
         Type_Entry :=
           Aquarius.Entries.Types.New_Type_Entry
           (Identifier.Standard_Text,
            Identifier);
         Type_Entry.Set_Display_Name (Identifier.Text);
         Type_Header.Symbol_Table.Insert (Type_Entry);
      end if;

      Type_Header.Set_Entry (Type_Entry);
      Type_Declaration.Set_Entry (Type_Entry);
   end Type_Header_After;

   ---------------------------
   -- Type_Indication_After --
   ---------------------------

   procedure Type_Indication_After
     (Type_Indication : Program_Tree)
   is
      Chosen_Type     : constant Program_Tree :=
        Type_Indication.Chosen_Tree;
      Type_Entry      : Aquarius.Entries.Table_Entry;
   begin
      if Chosen_Type.Has_Entry then
         Type_Entry :=  Chosen_Type.Get_Entry;
         if not Aquarius.Entries.Types.Is_Type_Entry (Type_Entry) then
            Aquarius.Errors.Error (Chosen_Type,
                                   "subtype mark required in this context");
            Type_Indication.Set_Type (Plugin.Error_Type);
         else
            Type_Indication.Set_Type
              (Aquarius.Entries.Types.Get_Type (Type_Entry));
         end if;
      else
         Type_Indication.Set_Type (Plugin.Error_Type);
      end if;
   end Type_Indication_After;

   --------------------------------
   -- Variable_Declaration_After --
   --------------------------------

   procedure Variable_Declaration_After
     (Variable_Declaration : Program_Tree)
   is
      Defining_Identifier_List : constant Program_Tree :=
        Variable_Declaration.Program_Child ("defining_identifier_list");
      Defining_Identifiers     : constant Array_Of_Program_Trees :=
        Defining_Identifier_List.Direct_Children
        ("defining_identifier");
      Variable_Type_Tree       : constant Program_Tree :=
        Variable_Declaration.Program_Child ("variable_type");
      Exception_Tree           : constant Program_Tree :=
        Variable_Declaration.Program_Child ("exception");
   begin
      if Variable_Type_Tree /= null then
         declare
            Type_Indication          : constant Program_Tree :=
              Variable_Type_Tree.Program_Child ("type_indication");
            Variable_Type   : constant Aquarius.Types.Aquarius_Type :=
              Type_Indication.Get_Type;
         begin
            for I in Defining_Identifiers'Range loop
               declare
                  Var_Decl        : constant Program_Tree :=
                    Program_Tree (Defining_Identifiers (I).First_Leaf);
                  Variable_Name   : constant String :=
                    Var_Decl.Text;
                  Var_Std_Name    : constant String :=
                    Var_Decl.Standard_Text;
                  New_Entry       : constant Aquarius.Entries.Table_Entry :=
                    Aquarius.Entries.Objects.New_Object_Entry
                    (Var_Std_Name, Var_Decl, Variable_Type,
                     Aquarius.Values.No_Value, False);
               begin
                  New_Entry.Set_Display_Name (Variable_Name);
                  Variable_Declaration.Symbol_Table.Insert (New_Entry);
                  Var_Decl.Set_Entry (New_Entry);
               end;
            end loop;
         end;
      elsif Exception_Tree /= null then
         null;
      end if;
   end Variable_Declaration_After;

end Ada_Plugin.Ch03;
