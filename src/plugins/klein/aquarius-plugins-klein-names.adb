with Aquarius.Entries.Packages;
with Aquarius.Entries.Objects;
with Aquarius.Errors;
with Aquarius.Programs;                 use Aquarius.Programs;
with Aquarius.Trees;
with Aquarius.Types.Maps;

with Aquarius.Plugins.Klein.Inference;
with Aquarius.Plugins.Klein.Types;

package body Aquarius.Plugins.Klein.Names is

   procedure Update_Properties (Tree      : Program_Tree;
                                For_Entry : Aquarius.Entries.Table_Entry);

   function Get_Name_Entry (Tree : Program_Tree)
                           return Aquarius.Entries.Table_Entry;

   function Get_Name_Type (Tree : Program_Tree)
                           return Aquarius.Types.Aquarius_Type;

   function Get_Name_Node (Tree : Program_Tree)
                          return Program_Tree;

   --------------------------
   -- Component_Name_After --
   --------------------------

   procedure Component_Name_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      use Aquarius.Types;
      use Aquarius.Entries.Objects;
      Item          : constant Aquarius.Programs.Program_Tree :=
                        Program_Tree (Target);
      Variable_Name : constant String := Item.Leaf ("identifier").Text;
      Var_Std_Name  : constant String :=
        Item.Leaf ("identifier").Standard_Text;
      Current_Type  : constant Aquarius.Types.Aquarius_Type :=
        Get_Name_Type (Item);
      Var_Entry     : constant Aquarius.Entries.Table_Entry :=
        Types.Get_Component (Current_Type, Var_Std_Name);
   begin
      if Aquarius.Entries.Is_Null (Var_Entry) then
         Aquarius.Errors.Error (Item,
                                """" & Variable_Name & """ is undefined");
      end if;

      Update_Properties (Item, Var_Entry);
   end Component_Name_After;

   -----------------------
   -- Direct_Name_After --
   -----------------------

   procedure Direct_Name_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Item            : constant Aquarius.Programs.Program_Tree :=
        Program_Tree (Target);
      Variable_Name   : constant String := Item.Leaf ("identifier").Text;
      Var_Std_Name    : constant String :=
        Item.Leaf ("identifier").Standard_Text;
      Var_Entry       : constant Aquarius.Entries.Table_Entry :=
        Item.Symbol_Table.Retrieve (Var_Std_Name);
   begin
      if Aquarius.Entries.Is_Null (Var_Entry) then
         Aquarius.Errors.Error (Item,
                                """" & Variable_Name & """ is undefined");
      end if;

      Update_Properties (Item, Var_Entry);

   end Direct_Name_After;

   --------------------------
   -- Expanded_Name_After --
   --------------------------

   procedure Expanded_Name_After
     (Expanded_Name_Actionable : not null access Actions.Actionable'Class)
   is
      use Aquarius.Entries;
      use Aquarius.Entries.Packages;
      Expanded_Name : constant Program_Tree :=
        Program_Tree (Expanded_Name_Actionable);
      Package_Entry : constant Table_Entry :=
        Get_Name_Entry (Expanded_Name);
      Element_Name  : constant String :=
        Expanded_Name.Leaf ("identifier").Standard_Text;
      Display_Name  : constant String :=
        Expanded_Name.Leaf ("identifier").Text;
      Element_Entry : constant Table_Entry :=
        Get_Symbol_Table (Package_Entry).Retrieve (Element_Name);
   begin
      if Element_Entry = null then
         Aquarius.Errors.Error (Expanded_Name,
                                Display_Name & " is not defined in package " &
                                  Package_Entry.Display_Name);
      end if;

      Update_Properties (Expanded_Name, Element_Entry);

   end Expanded_Name_After;

   --------------------
   -- Get_Name_Entry --
   --------------------

   function Get_Name_Entry (Tree : Program_Tree)
                           return Aquarius.Entries.Table_Entry
   is
   begin
      return Get_Name_Node (Tree).Get_Entry;
   end Get_Name_Entry;

   -------------------
   -- Get_Name_Node --
   -------------------

   function Get_Name_Node (Tree : Program_Tree)
                          return Program_Tree
   is
      It : Aquarius.Trees.Tree := Aquarius.Trees.Tree (Tree);
   begin
      while It.Name /= "name" loop
         It := It.Parent;
      end loop;
      return Program_Tree (It);
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

   ----------------
   -- Name_After --
   ----------------

   procedure Name_After
     (Target : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Target);
      Item            : constant Aquarius.Programs.Program_Tree :=
        Program_Tree (Tree);
      Name_Entry      : constant Aquarius.Entries.Table_Entry :=
                          Item.Get_Entry;
   begin
      if Aquarius.Entries.Objects.Is_Object_Entry (Name_Entry) then
         Item.Set_Type
           (Aquarius.Entries.Objects.Object_Entry_Type (Name_Entry));
         Inference.Set_Inferred_Types
           (Item,
            Aquarius.Types.Single_Possible_Type (Item.Get_Type));
      end if;
   end Name_After;

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
      if Aquarius.Entries.Is_Null (For_Entry) then
         if Name_Node.Has_Entry then
            Name_Node.Clear_Property (Aquarius.Properties.Entry_Property);
         end if;
         if Name_Node.Has_Type then
            Name_Node.Clear_Property (Aquarius.Properties.Type_Property);
         end if;
      else
         Name_Node.Set_Entry (For_Entry);
         if Is_Object_Entry (For_Entry) then
            Name_Node.Set_Property (Plugin.Object_Property);
            declare
               use Aquarius.Types;
               Entry_Type : constant Aquarius_Type :=
                 Object_Entry_Type (For_Entry);
            begin
               if Entry_Type.all in Types.Record_Type'Class then
                  Name_Node.Set_Property (Plugin.Record_Property);
               elsif Aquarius.Types.Maps.Is_Map_Type (Entry_Type) then
                  Name_Node.Set_Property (Plugin.Procedure_Property);
               end if;
               Name_Node.Set_Type (Entry_Type);
            end;
         elsif Is_Package_Reference (For_Entry) then
            Name_Node.Set_Property (Plugin.Package_Property);
         end if;
      end if;
   end Update_Properties;

end Aquarius.Plugins.Klein.Names;
