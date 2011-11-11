with Tagatha.Fragments;
with Tagatha.Units;

with Aquarius.Names;
with Aquarius.Programs;                 use Aquarius.Programs;
with Aquarius.Tagatha_Object;
with Aquarius.Trees;
with Aquarius.Trees.Properties;

package body Ada_Plugin.Ch05.Gen is

   ----------------------
   -- Assignment_After --
   ----------------------

   procedure Assignment_After
     (Assignment : Program_Tree)
   is
      Object_Reference : constant Program_Tree :=
        Assignment.Program_Child ("object_reference");
      Expression       : constant Program_Tree :=
        Assignment.Program_Child ("expression");
   begin
      Assignment.Append_Fragment (Expression.Get_Fragment);
      Assignment.Append_Fragment (Object_Reference.Get_Fragment);
      Assignment.Append_Fragment (Tagatha.Fragments.Pop);
   end Assignment_After;

   --------------------------
   -- Loop_Statement_After --
   --------------------------

   procedure Loop_Statement_After
     (Tree : Aquarius.Programs.Program_Tree)
   is
      Top_Label : constant String :=
        Tree.Property (Plugin.Top_Label_Property).Name;
      Out_Label : constant String :=
        Tree.Property (Plugin.Out_Label_Property).Name;
      Unit   : constant Tagatha.Units.Tagatha_Unit_Access :=
        Aquarius.Tagatha_Object.Get_Unit
        (Aquarius.Trees.Properties.Get_Tagatha (Tree.all));
   begin
      Unit.Jump (Integer'Value (Top_Label));
      Unit.Label (Integer'Value (Out_Label));
   end Loop_Statement_After;

   ---------------------------
   -- Loop_Statement_Before --
   ---------------------------

   procedure Loop_Statement_Before
     (Tree : Aquarius.Programs.Program_Tree)
   is
      Top_Label, Out_Label : Positive;
      Unit   : constant Tagatha.Units.Tagatha_Unit_Access :=
        Aquarius.Tagatha_Object.Get_Unit
        (Aquarius.Trees.Properties.Get_Tagatha (Tree.all));
   begin
      Unit.Next_Label (Top_Label);
      Unit.Next_Label (Out_Label);
      Tree.Set_Property (Plugin.Top_Label_Property,
                         Aquarius.Names.Name_Value (Top_Label'Img));
      Tree.Set_Property (Plugin.Out_Label_Property,
                         Aquarius.Names.Name_Value (Out_Label'Img));
      Unit.Label (Top_Label);
   end Loop_Statement_Before;

   ---------------------------
   -- Save_Tagatha_Fragment --
   ---------------------------

   procedure Save_Tagatha_Fragment
     (Tree : Aquarius.Programs.Program_Tree)
   is
      Unit   : constant Tagatha.Units.Tagatha_Unit_Access :=
        Aquarius.Tagatha_Object.Get_Unit
        (Aquarius.Trees.Properties.Get_Tagatha (Tree.all));
   begin
      Tagatha.Fragments.Append_To_Unit (Unit.all, Tree.Get_Fragment);
   end Save_Tagatha_Fragment;

   ---------------------
   -- Statement_After --
   ---------------------

   procedure Statement_After
     (Statement  : Aquarius.Programs.Program_Tree)
   is
      Chosen : constant Program_Tree := Statement.Chosen_Tree;
      Unit   : constant Tagatha.Units.Tagatha_Unit_Access :=
        Aquarius.Tagatha_Object.Get_Unit
        (Aquarius.Trees.Properties.Get_Tagatha (Statement.all));
   begin
      if Chosen /= null then
         Tagatha.Fragments.Append_To_Unit (Unit.all, Chosen.Get_Fragment);
      end if;
   end Statement_After;

   -------------------------------
   -- Transfer_Tagatha_Fragment --
   -------------------------------

   procedure Transfer_Tagatha_Fragment
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
   begin
      Parent.Append_Fragment (Child.Get_Fragment);
   end Transfer_Tagatha_Fragment;

   ----------------------------------
   -- While_Statement_Header_After --
   ----------------------------------

   procedure While_Statement_Header_After
     (Tree : Aquarius.Programs.Program_Tree)
   is
      Out_Label : constant String :=
        Tree.Property (Plugin.Out_Label_Property).Name;
      Condition : Tagatha.Fragments.Tagatha_Fragment :=
        Tree.Get_Fragment;
      Unit   : constant Tagatha.Units.Tagatha_Unit_Access :=
        Aquarius.Tagatha_Object.Get_Unit
        (Aquarius.Trees.Properties.Get_Tagatha (Tree.all));
      use Tagatha.Fragments;

   begin
      Append (Condition, Branch (Integer'Value (Out_Label), False));
      Append_To_Unit (Unit.all, Condition);
   end While_Statement_Header_After;

end Ada_Plugin.Ch05.Gen;
