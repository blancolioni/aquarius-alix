with Ada.Text_IO;

with Aquarius.Errors;
with Aquarius.Names;
with Aquarius.Plugins.Dynamic;
with Aquarius.Script.Expressions;
with Aquarius.Source;
with Aquarius.Trees;

package body Aquarius.Plugins.Script_Plugin.Bindings is

   use Aquarius.Programs;

   ---------------------------------
   -- Action_After_Action_Context --
   ---------------------------------

   procedure Action_After_Action_Context
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
      Ids : constant Array_Of_Program_Trees :=
        Child.Direct_Children;
   begin
      Parent.Set_Property
        (Plugin.Property_Action_Context,
         Child);

      if Ids'Length > 2 then
         Aquarius.Errors.Error
           (Child,
            "Aquarius supports up to two action contexts");
      elsif Ids'Length = 2 then
         Parent.Set_Property
           (Plugin.Property_Parent_Node, Ids (1));
         Parent.Set_Property
           (Plugin.Property_Child_Node, Ids (2));
      else
         Parent.Set_Property
           (Plugin.Property_Child_Node, Ids (1));
      end if;
   end Action_After_Action_Context;

   -------------------------------------
   -- Action_After_Action_Definition --
   -------------------------------------

   procedure Action_After_Action_Definition
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
      Time_Definition : constant Program_Tree :=
        Program_Tree (Parent.Property (Plugin.Property_Action_Time));
      Parent_Context  : constant Program_Tree :=
        (if Parent.Has_Property (Plugin.Property_Parent_Node)
           then  Program_Tree (Parent.Property (Plugin.Property_Parent_Node))
           else null);
      Child_Context  : constant Program_Tree :=
        Program_Tree (Parent.Property (Plugin.Property_Child_Node));
      Group_Name     : constant String :=
        Parent.Property (Plugin.Property_Group_Name).Name;
      Target_Plugin  : constant Aquarius_Plugin :=
          Aquarius_Plugin (Parent.Property (Plugin.Property_Plugin));
      Group          : constant Aquarius.Actions.Action_Group :=
        Target_Plugin.Get_Action_Group (Group_Name);
      Script         : constant Aquarius.Script.Aquarius_Script :=
          Aquarius.Script.Aquarius_Script
                            (Parent.Property (Plugin.Property_Action_Script));
      Exprs           : constant Array_Of_Program_Trees :=
                          Child.Direct_Children
                            (Name => "expression");
   begin
      for I in Exprs'Range loop
         Script.Append
           (Item =>
              Aquarius.Script.Script_Element
                (Exprs (I).Property (Plugin.Property_Expression)));
      end loop;
      if Parent_Context = null then
         Ada.Text_IO.Put_Line ("script: " & Time_Definition.Text &
                               " " & Child_Context.Text);
         Target_Plugin.Register_Script
           (Syntax_Name => Child_Context.Text,
            Group       => Group,
            Position    => (if Time_Definition.Text = "before"
                            then Before
                            else After),
            Action      => Script);
      else
         Ada.Text_IO.Put_Line ("script: " &
                               Parent_Context.Text & " " &
                               Time_Definition.Text &
                               " " & Child_Context.Text);
         Target_Plugin.Register_Script
           (Parent_Name => Parent_Context.Text,
            Child_Name  => Child_Context.Text,
            Group       => Group,
            Position    => (if Time_Definition.Text = "before"
                            then Before
                            else After),
            Action      => Script);
      end if;

   end Action_After_Action_Definition;

   ------------------------------
   -- Action_After_Action_Time --
   ------------------------------

   procedure Action_After_Action_Time
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
   begin
      Parent.Set_Property
        (Plugin.Property_Action_Time,
         Child.Chosen_Tree);
   end Action_After_Action_Time;

   -------------------------------------
   -- Action_Before_Action_Definition --
   -------------------------------------

   procedure Action_Before_Action_Definition
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
      pragma Unreferenced (Child);
   begin
      Parent.Set_Property
        (Plugin.Property_Action_Script,
         Aquarius.Script.New_Script
           (Name => "internal",
            Path => Aquarius.Source.Containing_Directory
              (Parent.Source)));
   end Action_Before_Action_Definition;

   --------------------------------
   -- Arguments_After_Expression --
   --------------------------------

   procedure Arguments_After_Expression
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
   begin
      null;
   end Arguments_After_Expression;

   ------------------------------
   -- Binding_After_Expression --
   ------------------------------

   procedure Binding_After_Expression
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
   begin
      Parent.Set_Property (Plugin.Property_Binding_Value,
                           Child.Property (Plugin.Property_Expression));
   end Binding_After_Expression;

   ------------------------------
   -- Binding_After_Identifier --
   ------------------------------

   procedure Binding_After_Identifier
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
      Prop : constant Aquarius.Trees.Aquarius_Object :=
               Aquarius.Trees.Aquarius_Object
                 (Aquarius.Names.Name_Value (Child.Text));
   begin
      Parent.Set_Property (Plugin.Property_Binding_Name, Prop);
   end Binding_After_Identifier;

   ---------------------------
   -- Call_Expression_After --
   ---------------------------

   procedure Call_Expression_After
     (Item : Aquarius.Programs.Program_Tree)
   is
   begin
      null;
   end Call_Expression_After;

   -------------------------------------
   -- Call_Expression_After_Arguments --
   -------------------------------------

   procedure Call_Expression_After_Arguments
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
      Name   : constant String :=
                 Parent.Program_Child ("name").Concatenate_Children;
      Arg_Children : constant Array_Of_Program_Trees :=
                       Child.Direct_Children (Skip_Separators => True);
      Arguments : Aquarius.Script.Expressions.Array_Of_Expressions
        (Arg_Children'Range);
   begin
      Ada.Text_IO.Put_Line ("call: " & Name & " /" &
                            Natural'Image (Arguments'Length));

      for I in Arguments'Range loop
         Arguments (I) :=
           Aquarius.Script.Expressions.Expression_Access
             (Arg_Children (I).Property (Plugin.Property_Expression));
      end loop;

      Parent.Set_Property
        (Plugin.Property_Expression,
         Aquarius.Script.Expressions.Call_Expression
           (Callee    => Name,
            Arguments => Arguments));
   end Call_Expression_After_Arguments;

   --------------------------------
   -- Call_Expression_After_Name --
   --------------------------------

   procedure Call_Expression_After_Name
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
   begin
      Parent.Set_Property
        (Plugin.Property_Expression,
         Aquarius.Script.Expressions.Identifier_Expression
           (Child.Concatenate_Children));
   end Call_Expression_After_Name;

   ----------------------
   -- Expression_After --
   ----------------------

   procedure Expression_After
     (Item : Aquarius.Programs.Program_Tree)
   is
      Choice : constant Program_Tree := Item.Chosen_Tree;
   begin
      if not Choice.Has_Property (Plugin.Property_Expression) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "expression with no expression property: " & Choice.Image);
      end if;

      Item.Set_Property
        (Plugin.Property_Expression,
         Item.Chosen_Tree.Property
           (Plugin.Property_Expression));

   end Expression_After;

   -----------------------------
   -- Group_Declaration_After --
   -----------------------------

   procedure Group_Declaration_After
     (Item : Aquarius.Programs.Program_Tree)
   is
   begin
      null;
   end Group_Declaration_After;

   ---------------------------------------------
   -- Group_Declaration_After_List_Of_Actions --
   ---------------------------------------------

   procedure Group_Declaration_After_List_Of_Actions
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
   begin
      null;
   end Group_Declaration_After_List_Of_Actions;

   ------------------------------
   -- Group_Declaration_Before --
   ------------------------------

   procedure Group_Declaration_Before
     (Item : Aquarius.Programs.Program_Tree)
   is
   begin
      null;
   end Group_Declaration_Before;

   ----------------------------------------------
   -- Group_Declaration_Before_List_Of_Actions --
   ----------------------------------------------

   procedure Group_Declaration_Before_List_Of_Actions
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
      pragma Unreferenced (Child);
      New_Plugin : constant Aquarius_Plugin :=
        Aquarius_Plugin (Parent.Property (Plugin.Property_Plugin));
      Ids        : constant Array_Of_Program_Trees :=
        Parent.Direct_Children ("identifier");
      Trigger    : constant Aquarius.Actions.Action_Execution_Trigger :=
        Aquarius.Actions.Action_Execution_Trigger'Value
        (Ids (2).Text & "_Trigger");
      Group      : Aquarius.Actions.Action_Group;
   begin
      Parent.Set_Property (Plugin.Property_Plugin, New_Plugin);
      Ada.Text_IO.Put_Line
        (Plugin.Name
         & ": new action group "
         & Ids (1).Text
         & " with trigger "
         & Ids (2).Text);
--        New_Plugin.Create_Action_Group
--          (Ids (1).Text, Trigger, Group);
      New_Plugin.Grammar.Add_Action_Group
        (Ids (1).Text, Trigger, Group);
      New_Plugin.Add_Action_Group (Group);

      Parent.Set_Property (Plugin.Property_Group_Name,
                           Aquarius.Names.Name_Value (Ids (1).Text));
   end Group_Declaration_Before_List_Of_Actions;

   --------------------------
   -- Let_Expression_After --
   --------------------------

   procedure Let_Expression_After
     (Item : Aquarius.Programs.Program_Tree)
   is
   begin
      null;
   end Let_Expression_After;

   ----------------------------------
   -- Let_Expression_After_Binding --
   ----------------------------------

   procedure Let_Expression_After_Binding
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
      Bound_Name : constant String :=
        Child.Property (Plugin.Property_Binding_Name).Name;
      Bound_Value : constant Aquarius.Script.Script_Element :=
        Aquarius.Script.Script_Element
        (Child.Property (Plugin.Property_Binding_Value));
   begin
      Ada.Text_IO.Put_Line ("let " & Bound_Name & " = ...");
      Parent.Set_Property (Plugin.Property_Expression,
                           Aquarius.Script.Expressions.Let_Expression
                             (Bound_Name, Bound_Value));
   end Let_Expression_After_Binding;

   ------------------------------
   -- Literal_Expression_After --
   ------------------------------

   procedure Literal_Expression_After
     (Item : Aquarius.Programs.Program_Tree)
   is
      Literal : constant Program_Tree := Item.Chosen_Tree;
      Text    : constant String := Literal.Text;
   begin
      Item.Set_Property (Plugin.Property_Expression,
                         Aquarius.Script.Expressions.String_Expression
                           (Text (Text'First + 1 .. Text'Last - 1)));
   end Literal_Expression_After;

   ----------------------------------------------------
   -- Plugin_Declaration_Before_List_Of_Declarations --
   ----------------------------------------------------

   procedure Plugin_Declaration_Before_List_Of_Declarations
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
      pragma Unreferenced (Child);
      Plugin_Name : constant String :=
        Parent.Program_Child ("identifier").Text;
      Plugin_Version : constant String :=
        Parent.Program_Child ("version_spec").Concatenate_Children;
      New_Plugin : constant Aquarius_Plugin :=
        Aquarius.Plugins.Dynamic.New_Dynamic_Plugin
        (Plugin_Name, Plugin_Version);
   begin
      Parent.Program_Root.Set_Property (Plugin.Property_Plugin, New_Plugin);
   end Plugin_Declaration_Before_List_Of_Declarations;

   --------------------------------
   -- Property_Declaration_After --
   --------------------------------

   procedure Property_Declaration_After
     (Item : Aquarius.Programs.Program_Tree)
   is
      New_Plugin : constant Aquarius_Plugin :=
                     Aquarius_Plugin
                       (Item.Property (Plugin.Property_Plugin));
      New_Property : Aquarius.Properties.Property_Type;
   begin
      Ada.Text_IO.Put_Line
        (New_Plugin.Name & ": new property: "
           & Item.Program_Child ("identifier").Standard_Text);
      Aquarius.Properties.Create_Property
        (Pool      => New_Plugin.Grammar.all,
         Prop      => New_Property,
         Name      => Item.Program_Child ("identifier").Standard_Text,
         Inherited => False,
         Has_Value => True);
   end Property_Declaration_After;

   ---------------------------
   -- With_Expression_After --
   ---------------------------

   procedure With_Expression_After
     (Item : Aquarius.Programs.Program_Tree)
   is
   begin
      null;
   end With_Expression_After;

end Aquarius.Plugins.Script_Plugin.Bindings;
