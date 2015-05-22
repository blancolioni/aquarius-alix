with Ada.Directories;
with Ada.Text_IO;

with Aquarius.Actions.Interpreter;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Names;
with Aquarius.Plugins.Dynamic;

package body Aquarius.Plugins.Script_Plugin.Bindings is

   use Aquarius.Programs;

   ---------------------------------
   -- After_Action_File_Reference --
   ---------------------------------

   procedure After_Action_File_Reference
     (Item : Aquarius.Programs.Program_Tree)
   is
      Path : constant String :=
               Ada.Directories.Compose
                 (Containing_Directory => Item.Source_Directory,
                  Name                 => Item.Concatenate_Children,
                  Extension            => "action");
      Action_Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                         Aquarius.Grammars.Manager.Get_Grammar_For_File (Path);
      Action_Program : constant Aquarius.Programs.Program_Tree :=
                         Aquarius.Loader.Load_From_File
                           (Grammar => Action_Grammar,
                            Path    => Path);
      Group_Name     : constant String :=
                         Item.Property (Plugin.Property_Group_Name).Name;
      Group          : constant Aquarius.Actions.Action_Group :=
                         Get_Plugin (Item).Grammar.Group (Group_Name);
   begin
      Aquarius.Actions.Interpreter.Bind_Actions
        (Action  => Action_Program,
         Group   => Group,
         Grammar => Get_Plugin (Item).Grammar);
   end After_Action_File_Reference;

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

end Aquarius.Plugins.Script_Plugin.Bindings;
