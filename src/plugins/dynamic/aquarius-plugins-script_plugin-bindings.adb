with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

--  with Aquarius.Actions.Interpreter;
with Aquarius.Configuration;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Messages.Console;
with Aquarius.Names;
with Aquarius.Paths;
with Aquarius.Plugins.Dynamic;
with Aquarius.Source;
with Aquarius.Syntax;

with Aquarius.Programs.Aqua_Driver;

--  with Aquarius.Actions.Tagatha_Scanner;

with Ack;
with Ack.Bindings;
with Ack.Compile;

with Aqua.Images;

--  with Komnenos.Entities.Aqua_Entities;

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
     (Assembly_Name : String);

   procedure Load_Action_File
     (Full_Path : String;
      Group     : Aquarius.Actions.Action_Group;
      Image     : Aqua.Images.Image_Type);

   ---------------------------------
   -- After_Action_File_Reference --
   ---------------------------------

   procedure After_Action_File_Reference
     (Item : Aquarius.Programs.Program_Tree)
   is
      Base_Name   : constant String := Item.Concatenate_Children;
      Action_Path : constant String :=
                      Ada.Directories.Compose
                        (Containing_Directory => Item.Source_Directory,
                         Name                 => Base_Name,
                         Extension            => "action");
      Assembly_Path : constant String :=
                        Aquarius.Paths.Scratch_File
                          (Base_Name, "m32");
      Object_Path : constant String :=
                      Aquarius.Paths.Scratch_File
                        (Base_Name, "o32");
      Action_Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                         Aquarius.Grammars.Manager.Get_Grammar_For_File
                           (Action_Path);
      Action_Program : constant Aquarius.Programs.Program_Tree :=
                         Aquarius.Loader.Load_From_File
                           (Grammar => Action_Grammar,
                            Path    => Action_Path);
      Group_Name     : constant String :=
                         Item.Property (Plugin.Property_Group_Name).Name;
      Group          : constant Aquarius.Actions.Action_Group :=
                         Get_Plugin (Item).Grammar.Group (Group_Name);
      Target_Plugin  : constant Dynamic.Dynamic_Plugin :=
                         Dynamic.Dynamic_Plugin (Get_Plugin (Item));
   begin

      Target_Plugin.Grammar.Add_Action_Program
        (Group, Action_Program);

--        if Aquarius.Paths.Is_Newer (Action_Path, Assembly_Path) then
--           declare
--              use Aquarius.Messages;
--              Processor : Aquarius.Actions.Tagatha_Scanner.Tagatha_Scanner;
--           begin
--              Aquarius.Actions.Scanner.Scan_Actions
--                (Processor, Action_Program, Group);
--              if Console.Check_Messages (Action_Program.all) >= Warning then
--                 Ada.Text_IO.Put_Line
--                   (Ada.Text_IO.Standard_Error,
--                    "errors detected");
--                 return;
--              end if;
--              Processor.Write;
--           end;
--        end if;

      if Aquarius.Paths.Is_Newer (Assembly_Path, Object_Path) then
         Process_Compiled_Plugin (Base_Name);
      end if;

      Target_Plugin.Image.Load (Base_Name & ".o32");

   end After_Action_File_Reference;

   -----------------------------
   -- After_Value_Declaration --
   -----------------------------

   procedure After_Value_Declaration
     (Item : Aquarius.Programs.Program_Tree)
   is
      use Ada.Characters.Handling;
      Name : constant String :=
               To_Lower (Item.Program_Child ("identifier").Text);
      New_Plugin : constant Aquarius_Plugin :=
                     Aquarius_Plugin
                       (Item.Property (Plugin.Property_Plugin));
      Value      : constant Program_Tree :=
                     Item.Program_Child ("value");
      Simple_Value : constant Program_Tree :=
                       Value.Program_Child ("simple_value");
      List_Value   : constant Program_Tree :=
                       Value.Program_Child ("list_value");

      function To_Path (X : String) return String;

      -------------
      -- To_Path --
      -------------

      function To_Path (X : String) return String is
      begin
         if X'Length >= 2
           and then X (X'First) = '"'
           and then X (X'Last) = '"'
         then
            declare
               Result : constant String (1 .. X'Length - 2) :=
                          X (X'First + 1 .. X'Last - 1);
            begin
               return Result;
            end;
         else
            return X;
         end if;
      end To_Path;

   begin
      if Name = "source_path" then
         if List_Value /= null then
            declare
               Elements : constant Array_Of_Program_Trees :=
                            List_Value.Direct_Children ("value");
            begin
               for List_Element of Elements loop
                  New_Plugin.Add_Search_Path
                    (To_Path (List_Element.Concatenate_Children));
               end loop;
            end;
         else
            New_Plugin.Add_Search_Path
              (To_Path (Simple_Value.Concatenate_Children));
         end if;
      end if;

   end After_Value_Declaration;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Executor : Aqua_Action_Executor;
      Item     : not null access Aquarius.Actions.Actionable'Class)
   is
      Tree    : constant Program_Tree := Program_Tree (Item);
--        Komnenos_Arg : constant Aqua.Word := 0;
--                         Executor.Plugin.Executor.To_Word
--                         (Komnenos.Entities.Aqua_Entities.Get_Aqua_Object);
      Top_Arg      : constant Aqua.Word :=
                       Aquarius.Programs.Aqua_Driver.To_Address
                         (Tree.Program_Root);
      Tree_Arg     : constant Aqua.Address :=
                       Aquarius.Programs.Aqua_Driver.To_Address (Tree);
   begin
      Executor.Plugin.Executor.Execute
        (Tree.Local_Environment_Name,
         Executor.Start,
         (0, Top_Arg, Tree_Arg));
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Aquarius.Source.Show (Tree.Get_Location)
            & "caught exception while executing action for "
            & Tree.Name);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Message (E));
         raise;
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
--        Komnenos_Arg : constant Aqua.Word := 0;
--                     Executor.Plugin.Executor.To_Word
--                       (Komnenos.Entities.Aqua_Entities.Get_Aqua_Object);
      Top_Arg      : constant Aqua.Word :=
                       Aquarius.Programs.Aqua_Driver.To_Address
                         (Parent_Tree.Program_Root);
      Parent_Arg   : constant Aqua.Address :=
                       Aquarius.Programs.Aqua_Driver.To_Address (Parent_Tree);
      Child_Arg    : constant Aqua.Word :=
                       Aquarius.Programs.Aqua_Driver.To_Address (Child_Tree);
   begin
      Executor.Plugin.Executor.Execute
        (Parent_Tree.Local_Environment_Name,
         Executor.Start,
         (0, Top_Arg, Parent_Arg, Child_Arg));
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Aquarius.Source.Show (Child_Tree.Get_Location)
            & ": "
            & Parent_Tree.Name & "/" & Child_Tree.Name
            & ": "
            & Ada.Exceptions.Exception_Message (E));
         raise;
   end Execute;

   -----------------------------
   -- Group_Declaration_After --
   -----------------------------

   procedure Group_Declaration_After
     (Item : Aquarius.Programs.Program_Tree)
   is
   begin
      if Item.Program_Child ("group_body") = null then
         declare
            New_Plugin : constant Aquarius_Plugin :=
                           Aquarius_Plugin
                             (Item.Property (Plugin.Property_Plugin));
            Child      : constant Program_Tree :=
                      Item.Program_Child ("group_header");
            Ids        : constant Array_Of_Program_Trees :=
                           Child.Direct_Children ("identifier");
            Group_Name : constant String :=
                           Ids (1).Text;
            Group      : constant Aquarius.Actions.Action_Group :=
                           Get_Plugin (Item).Grammar.Group (Group_Name);
            Grammar    : constant Aquarius.Grammars.Aquarius_Grammar :=
                           New_Plugin.Grammar;
            Action_File_Path : constant String :=
                                 Aquarius.Paths.Scratch_File
                                   (Grammar.Name & "-"
                                    & Group_Name
                                    & "-action_bindings",
                                    "aqua");
            Base_Aqua_Path   : constant String :=
                                 Ada.Directories.Compose
                                   (Aquarius.Configuration.Get_Grammar_Path
                                      (Grammar.Name),
                                    "aqua");

         begin
            if Ack.Bindings.Load_Ack_Binding
              (Binding_File_Path => Action_File_Path,
               Base_Aqua_Path    => Base_Aqua_Path,
               Image             => Dynamic.Dynamic_Plugin (New_Plugin).Image,
               Grammar           => Grammar,
               Group             => Group)
            then
               if False then
                  Load_Action_File
                    (Full_Path => Action_File_Path,
                     Group     => Group,
                     Image     => Dynamic.Dynamic_Plugin (New_Plugin).Image);
               end if;
            end if;
         end;
      end if;

   end Group_Declaration_After;

   ------------------------------------------
   -- Group_Declaration_After_Group_Header --
   ------------------------------------------

   procedure Group_Declaration_After_Group_Header
     (Parent : Aquarius.Programs.Program_Tree;
      Child  : Aquarius.Programs.Program_Tree)
   is
      New_Plugin : constant Aquarius_Plugin :=
                     Aquarius_Plugin
                       (Parent.Property (Plugin.Property_Plugin));
      Ids        : constant Array_Of_Program_Trees :=
                     Child.Direct_Children ("identifier");
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

   end Group_Declaration_After_Group_Header;

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

   ----------------------
   -- Load_Action_File --
   ----------------------

   procedure Load_Action_File
     (Full_Path : String;
      Group     : Aquarius.Actions.Action_Group;
      Image     : Aqua.Images.Image_Type)
   is
      pragma Unreferenced (Group);
      Base_Name      : constant String :=
                         Ada.Directories.Base_Name (Full_Path);
      Assembly_Path  : constant String :=
                         Aquarius.Paths.Scratch_File
                           (Base_Name, "m32");
      Object_Path    : constant String :=
                         Aquarius.Paths.Scratch_File
                           (Base_Name, "o32");
--        Action_Program : constant Aquarius.Programs.Program_Tree :=
--                           Aquarius.Loader.Load_From_File (Full_Path);
   begin

--        if Aquarius.Paths.Is_Newer (Full_Path, Assembly_Path) then
--           declare
--              use Aquarius.Messages;
--              Processor : Aquarius.Actions.Tagatha_Scanner.Tagatha_Scanner;
--           begin
--              Aquarius.Actions.Scanner.Scan_Actions
--                (Processor, Action_Program, Group);
--              if Console.Check_Messages (Action_Program.all) >= Warning then
--                 Ada.Text_IO.Put_Line
--                   (Ada.Text_IO.Standard_Error,
--                    "errors detected");
--                 return;
--              end if;
--              Processor.Write;
--           end;
--        end if;

      if Aquarius.Paths.Is_Newer (Assembly_Path, Object_Path)
      then
         Process_Compiled_Plugin (Base_Name);
      end if;

      Image.Load (Base_Name & ".o32");

   end Load_Action_File;

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
         use type Aquarius.Syntax.Syntax_Tree;
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
         if Parent_Node = null then
            raise Constraint_Error
              with "no such node: " & Parent_Name;
         end if;

         if Child_Name = "" then
            Parent_Node.Set_Action
              (Group    => Group,
               Position => Position,
               Action   => Executor);
         else
            if Child_Node = null then
               raise Constraint_Error
                 with "no such node: " & Child_Name;
            end if;

            Parent_Node.Set_Action
              (Child      => Child_Node,
               Group      => Group,
               Position   => Position,
               Action     => Executor);
         end if;
      end Bind_To_Grammar;

   begin
      Ack.Compile.Load_Link_Config (Image);
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
     (Assembly_Name : String)
   is
      use type Aquarius.Messages.Message_Level;
      Path : constant String :=
               Aquarius.Paths.Scratch_File (Assembly_Name, "m32");
      Output_Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                         Aquarius.Grammars.Manager.Get_Grammar_For_File
                           (Path);
      Output_Program : constant Aquarius.Programs.Program_Tree :=
                         Aquarius.Loader.Load_From_File
                           (Grammar => Output_Grammar,
                            Path    => Path);
   begin
      if Aquarius.Messages.Console.Check_Messages (Output_Program.all)
        >= Aquarius.Messages.Error
      then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "errors detected; canceling plugin");
      else
         Output_Grammar.Run_Action_Trigger
           (Output_Program, Aquarius.Actions.Semantic_Trigger);
         if Aquarius.Messages.Console.Check_Messages (Output_Program.all)
           >= Aquarius.Messages.Error
         then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "errors detected; canceling plugin");
         end if;
      end if;
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
