with Ada.Directories;
with Ada.Text_IO;

--  with Aquarius.Actions.Interpreter;
with Aquarius.Config_Paths;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Names;
with Aquarius.Plugins.Dynamic;
with Aquarius.Syntax;

with Aquarius.Actions.Scanner;
with Aquarius.Actions.Pdp_11;

with Aqua.Images;

with Komnenos.Entities.Aqua_Entities;

package body Aquarius.Plugins.Script_Plugin.Bindings is

   use Aquarius.Programs;

   type Aqua_Action_Executor is
     new Aquarius.Actions.Action_Execution_Interface with
      record
         Plugin : Dynamic.Dynamic_Plugin;
         Start  : Aqua.Address;
      end record;

   overriding procedure Execute
     (Executor : Aqua_Action_Executor;
      Item     : not null access Aquarius.Actions.Actionable'Class);

   overriding procedure Execute
     (Executor : Aqua_Action_Executor;
      Parent   : not null access Aquarius.Actions.Actionable'Class;
      Child    : not null access Aquarius.Actions.Actionable'Class);

   procedure Process_Compiled_Plugin
     (Plugin : Aquarius.Plugins.Dynamic.Dynamic_Plugin_Type'Class;
      Path   : String);

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

      declare
         Processor : Aquarius.Actions.Pdp_11.Pdp_Scanner;
      begin
         Aquarius.Actions.Scanner.Scan_Actions
           (Processor, Action_Program, Group);
         Process_Compiled_Plugin
           (Dynamic.Dynamic_Plugin (Get_Plugin (Item)).all,
            Processor.Output_Path);
      end;

   end After_Action_File_Reference;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Executor : Aqua_Action_Executor;
      Item     : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree    : constant Program_Tree := Program_Tree (Item);
      Komnenos_Arg : constant Aqua.Word :=
                       Executor.Plugin.Executor.To_Word
                         (Komnenos.Entities.Aqua_Entities.Get_Aqua_Object);
      Top_Arg     : constant Aqua.Word :=
                      Executor.Plugin.Executor.To_Word (Tree.Program_Root);
      Tree_Arg  : constant Aqua.Word :=
                      Executor.Plugin.Executor.To_Word (Tree);
   begin
      Executor.Plugin.Executor.Execute
        (Executor.Start,
         (Komnenos_Arg, Top_Arg, Tree_Arg));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Executor : Aqua_Action_Executor;
      Parent   : not null access Aquarius.Actions.Actionable'Class;
      Child    : not null access Aquarius.Actions.Actionable'Class)
   is
      Parent_Tree : constant Program_Tree := Program_Tree (Parent);
      Child_Tree  : constant Program_Tree := Program_Tree (Child);
      Komnenos_Arg : constant Aqua.Word :=
                       Executor.Plugin.Executor.To_Word
                         (Komnenos.Entities.Aqua_Entities.Get_Aqua_Object);
      Top_Arg     : constant Aqua.Word :=
                      Executor.Plugin.Executor.To_Word
                        (Parent_Tree.Program_Root);
      Parent_Arg  : constant Aqua.Word :=
                      Executor.Plugin.Executor.To_Word
                        (Parent_Tree);
      Child_Arg   : constant Aqua.Word :=
                      Executor.Plugin.Executor.To_Word
                        (Child_Tree);
   begin
      Executor.Plugin.Executor.Execute
        (Executor.Start,
         (Komnenos_Arg, Top_Arg, Parent_Arg, Child_Arg));
   end Execute;

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

   ---------------------------------------------------
   -- Plugin_Declaration_After_List_Of_Declarations --
   ---------------------------------------------------

   procedure Plugin_Declaration_After_List_Of_Declarations
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
      pragma Unreferenced (Child);
      Plugin : constant Dynamic.Dynamic_Plugin :=
                 Dynamic.Dynamic_Plugin
                   (Get_Plugin (Parent));
      Image : constant Aqua.Images.Image_Type := Plugin.Image;
      Grammar  : constant Aquarius.Grammars.Aquarius_Grammar :=
                   Plugin.Grammar;

      procedure Bind_To_Grammar
        (Group_Name  : String;
         Before      : Boolean;
         Parent_Name : String;
         Child_Name  : String;
         Start       : Aqua.Address);

      ---------------------
      -- Bind_To_Grammar --
      ---------------------

      procedure Bind_To_Grammar
        (Group_Name  : String;
         Before      : Boolean;
         Parent_Name : String;
         Child_Name  : String;
         Start       : Aqua.Address)
      is
         Executor : constant Aqua_Action_Executor :=
                      (Plugin => Plugin,
                       Start  => Start);
         pragma Assert (Plugin.Have_Action_Group (Group_Name));

         Group    : constant Aquarius.Actions.Action_Group :=
                      Plugin.Get_Action_Group (Group_Name);
         Position : constant Rule_Position :=
                      (if Before then Aquarius.Before else Aquarius.After);
         Parent_Node       : constant Aquarius.Syntax.Syntax_Tree :=
                               Grammar.Get_Definition (Parent_Name);
         Child_Node        : constant Aquarius.Syntax.Syntax_Tree :=
                               (if Child_Name = "" then null
                                else Grammar.Get_Definition
                                  (Child_Name));
      begin
         if Child_Name = "" then
            Parent_Node.Set_Action
              (Group    => Group,
               Position => Position,
               Action   => Executor);
         else
            Parent_Node.Set_Action
              (Child      => Child_Node,
               Group      => Group,
               Position   => Position,
               Action     => Executor);
         end if;
      end Bind_To_Grammar;

   begin
      Process_Compiled_Plugin
        (Plugin.all,
         Aquarius.Config_Paths.Config_Path & "/aqua/text_io.m11");

      Image.Link;
      Image.Bind (Bind_To_Grammar'Access);
   end Plugin_Declaration_After_List_Of_Declarations;

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

   -----------------------------
   -- Process_Compiled_Plugin --
   -----------------------------

   procedure Process_Compiled_Plugin
     (Plugin : Aquarius.Plugins.Dynamic.Dynamic_Plugin_Type'Class;
      Path   : String)
   is
      Output_Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                         Aquarius.Grammars.Manager.Get_Grammar_For_File
                           (Path);
      Output_Program : constant Aquarius.Programs.Program_Tree :=
                         Aquarius.Loader.Load_From_File
                           (Grammar => Output_Grammar,
                            Path    => Path);
   begin
      Output_Grammar.Run_Action_Trigger
        (Output_Program, Aquarius.Actions.Semantic_Trigger);

      Plugin.Image.Load
        (Ada.Directories.Simple_Name (Path) & ".o11");
   end Process_Compiled_Plugin;

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
