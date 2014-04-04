with Ada.Text_IO;

with Aquarius.Programs;                 use Aquarius.Programs;
with Aquarius.Trees.Properties;         use Aquarius.Trees.Properties;

with Aquarius.Actions;
with Aquarius.Entries.Packages;
with Aquarius.Entries.Objects;
with Aquarius.Entries.Types;
with Aquarius.Errors;
with Aquarius.Properties;
with Aquarius.Types.Arrays;
with Aquarius.Types.Inference;
with Aquarius.Types.Maps;
with Aquarius.Types.Records;

with Ada_Plugin.Inference;

package body Ada_Plugin.Names is

   --  Update_Properties (1): update tree properties based on a single
   --  interpretation of the contents.
   procedure Update_Properties (Tree      : Program_Tree;
                                For_Entry : Aquarius.Entries.Table_Entry);

   --  Update_Properties (2): update tree properties base on an
   --  ambiguous name
   procedure Update_Properties
     (Tree        : Program_Tree;
      Candidates  : Aquarius.Entries.Array_Of_Entries);

   --  Clear_Properties: remove name related properties from the tree
   procedure Clear_Properties (Tree      : Program_Tree);

   function Get_Name_Entry (Tree : Program_Tree)
                           return Aquarius.Entries.Table_Entry;

   function Get_Name_Type (Tree : Program_Tree)
                           return Aquarius.Types.Aquarius_Type;

   function Get_Name_Node (Tree : Program_Tree)
                          return Program_Tree;

   procedure On_Expanded_Name_Change
     (Item   : not null access Aquarius.Programs.Program_Tree_Type'Class;
      Object : not null access Aquarius.Root_Aquarius_Object'Class);

   procedure On_Object_Reference_Change
     (Item   : not null access Aquarius.Programs.Program_Tree_Type'Class;
      Object : not null access Aquarius.Root_Aquarius_Object'Class);

   --------------------------------
   -- Actual_Argument_List_After --
   --------------------------------

   procedure Actual_Argument_List_After
     (Argument_List : Aquarius.Programs.Program_Tree)
   is
      Name_Node         : constant Program_Tree :=
        Get_Name_Node (Argument_List);
      Function_Name_Tree : constant Program_Tree :=
        Program_Tree (Name_Node.Property (Plugin.Last_Identifier_Property));
   begin
      if Function_Name_Tree.Has_Entry then
         if Aquarius.Entries.Types.Is_Type_Entry
           (Function_Name_Tree.Get_Entry)
         then
            Aquarius.Types.Inference.Set_Inferred_Types
              (Name_Node,
               Aquarius.Types.Single_Possible_Type
                 (Aquarius.Entries.Types.Get_Type
                    (Function_Name_Tree.Get_Entry)));
            return;
         end if;
         declare
            use type Aquarius.Types.Aquarius_Type;
            Entry_Type : constant Aquarius.Types.Aquarius_Type :=
                           Aquarius.Entries.Objects.Object_Entry_Type
                             (Function_Name_Tree.Get_Entry);
         begin
            if Aquarius.Types.Arrays.Is_Array_Type (Entry_Type) then
               --  FIXME: check types

               if Aquarius.Types.Arrays.Get_Component_Type (Entry_Type) /=
                 null
               then
                  Aquarius.Types.Inference.Set_Inferred_Types
                    (Name_Node,
                     Aquarius.Types.Single_Possible_Type
                       (Aquarius.Types.Arrays.Get_Component_Type
                          (Entry_Type)));
               end if;
               return;
            elsif not Aquarius.Types.Maps.Is_Map_Type (Entry_Type) then
               Aquarius.Errors.Error (Function_Name_Tree,
                                      """" & Function_Name_Tree.Text &
                                        """ is neither " &
                                        "an array nor a function");
               return;
            end if;
         end;
      end if;

      if not Aquarius.Types.Inference.Has_Possible_Types (Name_Node) then
         Aquarius.Types.Inference.Set_Possible_Types
           (Name_Node, new Aquarius.Types.Possible_Type_Record);
      end if;

      Ada_Plugin.Inference.Check_Function
        (Expr          => Argument_List,
         Possibles     =>
           Aquarius.Types.Inference.Get_Possible_Types
           (Name_Node),
         Function_Name => Function_Name_Tree,
         Args          => Argument_List.Direct_Children ("actual_argument"));
   end Actual_Argument_List_After;

   -------------------------------
   -- Attribute_Reference_After --
   -------------------------------

   procedure Attribute_Reference_After
     (Tree : Program_Tree)
   is
      use Aquarius.Properties;
      Name_Node : constant Program_Tree := Get_Name_Node (Tree);
   begin
      Name_Node.Set_Property (Plugin.Attribute_Property);
   end Attribute_Reference_After;

   ----------------------
   -- Clear_Properties --
   ----------------------

   procedure Clear_Properties (Tree      : Program_Tree) is
      Name_Node : constant Program_Tree := Get_Name_Node (Tree);
   begin

      --  The following if statements are fucked

      if Name_Node.Has_Property (Plugin.Package_Property) then
         Name_Node.Clear_Property (Plugin.Package_Property);
      end if;

      if Name_Node.Has_Property (Plugin.Object_Property) then
         Name_Node.Clear_Property (Plugin.Object_Property);
      end if;

      if Name_Node.Has_Property (Plugin.Record_Property) then
         Name_Node.Clear_Property (Plugin.Record_Property);
      end if;

      if Name_Node.Has_Property (Plugin.Procedure_Property) then
         Name_Node.Clear_Property (Plugin.Procedure_Property);
      end if;

   end Clear_Properties;

   --------------------------
   -- Component_Name_After --
   --------------------------

--     procedure Component_Name_After
--       (Target : access Aquarius.Actions.Actionable'Class)
--     is
--        use Aquarius.Types;
--        use Aquarius.Entries.Objects;
--        Tree : constant Aquarius.Trees.Tree :=
--          Aquarius.Trees.Tree (Target);
--        Item          : constant Aquarius.Programs.Program_Tree :=
--          Program_Tree (Tree);
--        Variable_Name : constant String := Tree.Leaf ("identifier").Text;
--        Var_Std_Name  : constant String :=
--          Tree.Leaf ("identifier").Standard_Text;
--        Current_Type  : constant Aquarius.Types.Aquarius_Type :=
--          Get_Name_Type (Item);
--        Var_Entry     : constant Aquarius.Entries.Table_Entry :=
--          Aquarius.Types.Records.Get_Component (Current_Type, Var_Std_Name);
--     begin
--        if Aquarius.Entries.Is_Null (Var_Entry) then
--           Aquarius.Errors.Error (Tree,
--                                  """" & Variable_Name & """ is undefined");
--        end if;

--        Update_Properties (Item, Var_Entry);
--     end Component_Name_After;

   ----------------------------
   -- Create_Change_Handlers --
   ----------------------------

   procedure Create_Change_Handlers
     (Plugin  : not null access Ada_Plugin_Type'Class)
   is
   begin
      Plugin.Register_Change_Handler
        ("expanded_name", On_Expanded_Name_Change'Access);
      Plugin.Register_Change_Handler
        ("object_reference", On_Object_Reference_Change'Access);
   end Create_Change_Handlers;

   ----------------------------------------
   -- Defining_Qualified_Reference_After --
   ----------------------------------------

   procedure Defining_Qualified_Reference_After
     (Tree       : Program_Tree)
   is
      use Aquarius.Entries;
      Identifiers : constant Array_Of_Program_Trees :=
                      Tree.Direct_Children ("identifier");
      Current     : Table_Entry  := null;
      Table       : Aquarius.Entries.Symbol_Table := Tree.Symbol_Table;
   begin

      for I in Identifiers'First .. Identifiers'Last - 1 loop
         declare
            Identifier : constant Program_Tree := Identifiers (I);
            This       : constant Table_Entry :=
                           Table.Retrieve (Identifiers (I).Standard_Text);
         begin
            if Aquarius.Entries.Is_Null (This) then

               --  We don't know this name, but because of its
               --  position in the name we know it's a package.
               --  Since this is a defining reference, we can
               --  load the package on the fly.

               Current := Load_Child_Package
                 (Aquarius.Trees.Properties.Get_Project (Tree.all),
                  Identifiers (I),
                  Current,
                  Identifier.Standard_Text);

               if Is_Null (Current) then

                  if I = Identifiers'First then
                     Aquarius.Errors.Error
                       (Identifiers (I),
                        Identifiers (I).Text & " undefined");
                  else
                     Aquarius.Errors.Error
                       (Identifiers (I),
                        Identifiers (I).Text &
                        " is not a known child of package " &
                        Identifiers (I - 1).Text);
                  end if;
               else
                  Table :=
                    Aquarius.Entries.Packages.Get_Symbol_Table (Current);
               end if;
            else
               --  Following the children
               if Aquarius.Entries.Packages.Is_Package_Reference (This) then
                  Table   :=
                    Aquarius.Entries.Packages.Get_Symbol_Table (This);
                  Current := This;
               else
                  Aquarius.Errors.Error (Identifiers (I), Tree,
                                         "expected a package name",
                                         "found " & Identifier.Text);
               end if;
            end if;
         end;

         Tree.Set_Entry (Current);

      end loop;

      Tree.Set_Property (Plugin.Last_Identifier_Property,
                         Identifiers (Identifiers'Last));

   end Defining_Qualified_Reference_After;

   -----------------------------------------
   -- Defining_Qualified_Reference_Before --
   -----------------------------------------

   procedure Defining_Qualified_Reference_Before
     (Tree : Aquarius.Programs.Program_Tree)
   is
   begin
      Tree.Set_Property (Plugin.Defining_Instance_Property);
   end Defining_Qualified_Reference_Before;

   -----------------------
   -- Direct_Name_After --
   -----------------------

   procedure Direct_Name_After
     (Item : Aquarius.Programs.Program_Tree)
   is
      Identifier    : constant Program_Tree :=
        Program_Tree (Item.Leaf ("identifier"));
      Name_Node     : constant Program_Tree := Get_Name_Node (Item);
      Name_Text     : constant String := Item.Leaf ("identifier").Text;
      Name_Std_Text : constant String :=
        Item.Leaf ("identifier").Standard_Text;
      Name_Entries  : constant Aquarius.Entries.Array_Of_Entries :=
        Item.Symbol_Table.Search (Name_Std_Text);
   begin
      if Name_Entries'Length = 0 then
         Aquarius.Errors.Error (Item,
                                """" & Name_Text & """ is undefined");
      elsif Name_Entries'Length = 1 then
         declare
            Ent : constant Aquarius.Entries.Table_Entry :=
              Name_Entries (Name_Entries'First);
         begin
            Update_Properties (Item, Ent);
            Identifier.Set_Entry (Ent);
         end;
      else
         Update_Properties (Item, Name_Entries);
      end if;
      Name_Node.Set_Property (Plugin.Last_Identifier_Property,
                              Item.Leaf ("identifier"));
   end Direct_Name_After;

   --------------------------
   -- Expanded_Name_After --
   --------------------------

   procedure Expanded_Name_After
     (Expanded_Name : Aquarius.Programs.Program_Tree)
   is
      use Aquarius.Entries;
      use Aquarius.Entries.Packages;
      Object_Entry  : constant Table_Entry :=
        Get_Name_Entry (Expanded_Name);
      Element_Name_Tree : constant Program_Tree :=
        Expanded_Name.Chosen_Tree;
      Element_Name      : constant String := Element_Name_Tree.Standard_Text;
      Display_Name      : constant String := Element_Name_Tree.Text;
      Element_Entry     : Table_Entry;
      Name_Node         : constant Program_Tree :=
        Get_Name_Node (Expanded_Name);
      Current_Type      : Aquarius.Types.Aquarius_Type;
   begin

      Name_Node.Set_Property (Plugin.Last_Identifier_Property,
                              Element_Name_Tree);

      if Object_Entry = null then
         return;
      end if;

      if Aquarius.Entries.Packages.Is_Package_Reference (Object_Entry) then

         Get_Symbol_Table (Object_Entry).Add_Watcher (Plugin, Name_Node);
         Element_Name_Tree.Set_Symbol_Table
           (Get_Symbol_Table (Object_Entry));

         declare
            Element_Entries  : constant Aquarius.Entries.Array_Of_Entries :=
              Get_Symbol_Table (Object_Entry).Search (Element_Name);
            Not_Found : Boolean := False;
         begin
            if Element_Entries'Length = 0 then
               Element_Entry :=
                 Get_Child_Package (Object_Entry, Element_Name);
               Not_Found := Element_Entry = null;
            elsif Element_Entries'Length = 1 then
               Element_Entry := Element_Entries (Element_Entries'First);
            end if;

            if Not_Found then
               Aquarius.Errors.Error (Expanded_Name,
                                      Display_Name &
                                        " is not defined in package " &
                                        Object_Entry.Display_Name);
            elsif Element_Entry /= null then
               Update_Properties (Expanded_Name, Element_Entry);
            else
               Update_Properties (Expanded_Name, Element_Entries);
            end if;

            if Aquarius.Types.Inference.Has_Inferred_Types
              (Expanded_Name)
            then
               Aquarius.Types.Inference.Set_Inferred_Types
                 (Element_Name_Tree,
                  Aquarius.Types.Inference.Get_Inferred_Types
                    (Expanded_Name));
            end if;

         end;
      elsif Aquarius.Entries.Objects.Is_Object_Entry (Object_Entry) then
         Current_Type := Get_Name_Type (Expanded_Name);

         if Aquarius.Types.Records.Is_Record_Type (Current_Type) then
            Element_Entry :=
              Aquarius.Types.Records.Get_Component (Current_Type,
                                                    Element_Name);
            if Element_Entry /= null then
               Update_Properties (Expanded_Name, Element_Entry);
            end if;
         end if;
      end if;

   end Expanded_Name_After;

   --------------------
   -- Get_Name_Entry --
   --------------------

   function Get_Name_Entry (Tree : Program_Tree)
                           return Aquarius.Entries.Table_Entry
   is
      Name : constant Program_Tree := Get_Name_Node (Tree);
   begin
      if Name.Has_Entry then
         return Name.Get_Entry;
      else
         return null;
      end if;
   end Get_Name_Entry;

   -------------------
   -- Get_Name_Node --
   -------------------

   function Get_Name_Node (Tree : Program_Tree)
                          return Program_Tree
   is
   begin
      if Tree.Has_Property (Plugin.Object_Reference_Property) then
         return Program_Tree
           (Tree.Property (Plugin.Object_Reference_Property));
      else
         raise Constraint_Error with "No object reference at " &
           Tree.Image;
      end if;
   end Get_Name_Node;

   -------------------
   -- Get_Name_Type --
   -------------------

   function Get_Name_Type (Tree : Program_Tree)
                          return Aquarius.Types.Aquarius_Type
   is
   begin
      return Get_Name_Node (Tree).Get_Type;
   end Get_Name_Type;

   ----------------------------
   -- Object_Reference_After --
   ----------------------------

   procedure Object_Reference_After
     (Item : Aquarius.Programs.Program_Tree)
   is
      Name_Entry  : Aquarius.Entries.Table_Entry;
   begin
      if Item.Has_Entry then
         Name_Entry := Item.Get_Entry;
      else
         return;
      end if;

      if Aquarius.Entries.Objects.Is_Object_Entry (Name_Entry) then
         Item.Set_Type
           (Aquarius.Entries.Objects.Object_Entry_Type (Name_Entry));
         Aquarius.Types.Inference.Set_Inferred_Types
           (Item,
            Aquarius.Types.Single_Possible_Type (Item.Get_Type));
      end if;
   end Object_Reference_After;

   -----------------------------
   -- Object_Reference_Before --
   -----------------------------

   procedure Object_Reference_Before
     (Tree : Aquarius.Programs.Program_Tree)
   is
      use Aquarius.Types.Inference;
   begin
      Tree.Set_Property (Plugin.Object_Reference_Property, Tree);
   end Object_Reference_Before;

   -----------------------------
   -- On_Expanded_Name_Change --
   -----------------------------

   procedure On_Expanded_Name_Change
     (Item   : not null access Aquarius.Programs.Program_Tree_Type'Class;
      Object : not null access Aquarius.Root_Aquarius_Object'Class)
   is
      pragma Unreferenced (Object);
   begin
      Item.Clear_Messages;
      Expanded_Name_After (Program_Tree (Item));
   end On_Expanded_Name_Change;

   --------------------------------
   -- On_Object_Reference_Change --
   --------------------------------

   procedure On_Object_Reference_Change
     (Item   : not null access Aquarius.Programs.Program_Tree_Type'Class;
      Object : not null access Aquarius.Root_Aquarius_Object'Class)
   is
      pragma Unreferenced (Object);
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
        Get_Grammar (Item.all);
   begin
      Item.Clear_Messages;
      Grammar.Run_Action_Trigger (Program_Tree (Item),
                                  Aquarius.Actions.Parse_Trigger);
   end On_Object_Reference_Change;

   ------------------------------------------
   -- Qualified_Reference_After_Identifier --
   ------------------------------------------

   procedure Qualified_Reference_After_Identifier
     (Tree       : Program_Tree;
      Identifier : Program_Tree)
   is
      Current    : Aquarius.Entries.Table_Entry;
   begin

      Tree.Set_Property (Plugin.Last_Identifier_Property, Identifier);

      if Tree.Has_Entry then
         declare
            use Aquarius.Entries, Aquarius.Entries.Packages;
            P : constant Aquarius.Entries.Table_Entry := Tree.Get_Entry;
         begin
            if Is_Package_Reference (P) then
               Current := Get_Symbol_Table (P).Retrieve
                 (Identifier.Standard_Text);
               if Aquarius.Entries.Is_Null (Current) then
                  Current := Get_Child_Package (P, Identifier.Standard_Text);
                  if Current = null then
                     Aquarius.Errors.Error (Identifier,
                                            Identifier.Text &
                                              " not declared in " &
                                              P.Name);
                  end if;
               end if;
               if Current /= null then
                  Tree.Set_Entry (Current);
               end if;
            else
               Aquarius.Errors.Error (Tree, Tree,
                                      "expected a package name",
                                      "found " & Identifier.Text);
            end if;
         end;
      else
         Current := Tree.Symbol_Table.Retrieve (Identifier.Standard_Text);
         if Aquarius.Entries.Is_Null (Current) then
            if Tree.Has_Property (Plugin.Defining_Instance_Property) then
               Current := Load_Child_Package
                 (Aquarius.Trees.Properties.Get_Project (Tree.all),
                  Identifier,
                  Current,
                  Identifier.Standard_Text);
               if not Aquarius.Entries.Is_Null (Current) then
                  Tree.Set_Entry (Current);
               end if;
            else
               Aquarius.Errors.Error (Identifier,
                                      Identifier.Text & " undefined");
            end if;
         else
            Tree.Set_Entry (Current);
         end if;
      end if;
   exception
      when others =>
         Ada.Text_IO.Put_Line ("exception in "
                               & "Qualified_Reference_After_Identifier");
         Ada.Text_IO.Put_Line ("   " & Tree.Image);
         Ada.Text_IO.Put_Line ("   " & Identifier.Image);
         raise;
   end Qualified_Reference_After_Identifier;

   -----------------------
   -- Update_Properties --
   -----------------------

   procedure Update_Properties (Tree      : Program_Tree;
                                For_Entry : Aquarius.Entries.Table_Entry)
   is
      use Aquarius.Entries.Objects;
      use Aquarius.Entries.Packages;
      Name_Node : constant Program_Tree := Get_Name_Node (Tree);
   begin

      Clear_Properties (Tree);

      if Aquarius.Entries.Is_Null (For_Entry) then
         if Name_Node.Has_Entry then
            Name_Node.Clear_Property (Aquarius.Properties.Entry_Property);
         end if;
         if Name_Node.Has_Type then
            Name_Node.Clear_Property (Aquarius.Properties.Type_Property);
         end if;
      else
         Tree.Set_Entry (For_Entry);
         Name_Node.Set_Entry (For_Entry);
         if Is_Object_Entry (For_Entry) then
            Name_Node.Set_Property (Plugin.Object_Property);
            declare
               use Aquarius.Types;
               Entry_Type : constant Aquarius_Type :=
                 Object_Entry_Type (For_Entry);
            begin
               if Aquarius.Types.Records.Is_Record_Type (Entry_Type) then
                  Name_Node.Set_Property (Plugin.Record_Property);
               elsif Aquarius.Types.Maps.Is_Map_Type (Entry_Type) then
                  Name_Node.Set_Property (Plugin.Procedure_Property);
                  Name_Node.Set_Property (Plugin.Property_Function);
               end if;
               Name_Node.Set_Type (Entry_Type);
            end;
         elsif Is_Package_Reference (For_Entry) then
            Name_Node.Set_Property (Plugin.Package_Property);
         end if;
      end if;
   end Update_Properties;

   -----------------------
   -- Update_Properties --
   -----------------------

   procedure Update_Properties
     (Tree        : Program_Tree;
      Candidates  : Aquarius.Entries.Array_Of_Entries)
   is
      use Aquarius.Types;
      use Aquarius.Types.Inference;
      use Aquarius.Types.Maps;
      use Aquarius.Entries.Objects;
      Name_Node : constant Program_Tree := Get_Name_Node (Tree);
   begin

      Clear_Properties (Tree);

      for I in Candidates'Range loop
         if not Is_Object_Entry (Candidates (I)) or else
           not Is_Map_Type (Object_Entry_Type (Candidates (I)))
         then
            Aquarius.Errors.Error (Tree,
                                   "cannot resolve overloading of '" &
                                     Name_Node.Text & "' due to " &
                                     "non-overloadable interpretations");
            Aquarius.Errors.Error (Candidates (I).Declaration,
                                   " cannot be overloaded");
            return;
         end if;
      end loop;

      declare
         Inferred : constant Possible_Types := new Possible_Type_Record;
      begin
         for I in Candidates'Range loop
            Inferred.Add_Type
              (Object_Entry_Type (Candidates (I)));
         end loop;

         Aquarius.Types.Inference.Set_Inferred_Types (Tree, Inferred);

         Name_Node.Set_Property (Plugin.Object_Property);
         Name_Node.Set_Property (Plugin.Procedure_Property);
         Name_Node.Set_Property (Plugin.Property_Function);
      end;

   end Update_Properties;

end Ada_Plugin.Names;
