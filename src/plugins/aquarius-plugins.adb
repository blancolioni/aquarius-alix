with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

with Aquarius.Buffers;
with Aquarius.Syntax;
with Aquarius.Trees.Properties;
with Aquarius.UI.Menus;

with Aquarius.Loader;

with Aquarius.VM.Library;

with Aquarius.Config_Paths;

package body Aquarius.Plugins is

   function To_Plugin_Name (Name : String) return Plugin_Map_Name;
   function To_Plugin_Group_Name (Name : String) return Plugin_Group_Name;

   type Plugin_Menu_Command is
     new Aquarius.UI.Menus.Root_Menu_Command with
      record
         Plugin  : Aquarius_Plugin;
         Command : Aquarius.VM.VM_Value;
      end record;

   overriding
   procedure Execute (Item : not null access Plugin_Menu_Command);

   ----------------------
   -- Add_Action_Group --
   ----------------------

   procedure Add_Action_Group (Plugin : in out Aquarius_Plugin_Type;
                               Group  : Aquarius.Actions.Action_Group)
   is
   begin
      Plugin.Action_Groups.Insert
        (To_Plugin_Group_Name (Aquarius.Actions.Action_Group_Name (Group)),
         Group);
   end Add_Action_Group;

   ------------------------
   -- Add_Standard_Entry --
   ------------------------

   procedure Add_Standard_Entry
     (Plugin : not null access Aquarius_Plugin_Type;
      Item   : in     Aquarius.Entries.Table_Entry)
   is
   begin
      Plugin.Standard.Insert (Item);
   end Add_Standard_Entry;

   -------------------------
   -- Create_Action_Group --
   -------------------------

   procedure Create_Action_Group
     (Plugin     : in out Aquarius_Plugin_Type;
      Group_Name : in     String;
      Trigger    : in     Aquarius.Actions.Action_Execution_Trigger;
      Group      :    out Aquarius.Actions.Action_Group)
   is
   begin
      Aquarius.Actions.Create_Action_Group
        (Plugin.Group_List, Group_Name, Trigger, Group);
   end Create_Action_Group;

   -----------------
   -- Environment --
   -----------------

   function Environment
     (Plugin : not null access Aquarius_Plugin_Type'Class)
     return Aquarius.VM.VM_Environment
   is
   begin
      return Plugin.VM_Env;
   end Environment;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Item : not null access Plugin_Menu_Command) is
      Result : constant VM.VM_Value :=
                 Aquarius.VM.Evaluate (Item.Command,
                                              Item.Plugin.VM_Env);
   begin
      if VM.Has_Tree (Result) then
         declare
            Buffer : constant Aquarius.Buffers.Aquarius_Buffer :=
                       Aquarius.Buffers.New_Buffer_From_Tree
              (Item.Command_UI, "new buffer",
               Aquarius.Programs.Program_Tree
                 (VM.To_Tree (Result)));
         begin
            Item.Command_UI.Show_Interactor (Buffer);
         end;
      end if;
   end Execute;

   ----------------------
   -- Get_Action_Group --
   ----------------------

   function Get_Action_Group (Plugin : Aquarius_Plugin_Type;
                              Name   : String)
                             return Aquarius.Actions.Action_Group
   is
   begin
      return Plugin.Action_Groups.Element (To_Plugin_Group_Name (Name));
   end Get_Action_Group;

   -----------------
   -- Get_Program --
   -----------------

   overriding function Get_Program
     (Plugin    : not null access Aquarius_Plugin_Type;
      File_Name : String)
      return Aquarius.Programs.Program_Tree
   is
      use Ada.Directories;
      Result : Aquarius.Programs.Program_Tree;
   begin
      if Plugin.Loaded_Programs.Contains (File_Name) then
         Result := Plugin.Loaded_Programs (File_Name);
      else
         declare
            use Ada.Characters.Handling;
            use Aquarius.Programs;
            Standard_Path : constant String :=
                              Aquarius.Config_Paths.Config_Path
                              & "/"
                              & To_Lower
                                (Aquarius_Plugin_Type'Class (Plugin.all).Name)
                              & "/"
                              & File_Name;
         begin
            if Exists (File_Name)
              and then Kind (File_Name) = Ordinary_File
            then
               Result := Plugin.Load_Program_Tree (File_Name);
            elsif Exists (Standard_Path)
              and then Kind (Standard_Path) = Ordinary_File
            then
               Result := Plugin.Load_Program_Tree (Standard_Path);
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "not found: " & File_Name);
               Result := null;
            end if;

            Plugin.Loaded_Programs.Insert (File_Name, Result);

            if Result /= null then
               Plugin.Grammar.Run_Action_Trigger
                 (Result, Aquarius.Actions.Semantic_Trigger);
            end if;

         end;

      end if;

      return Result;

   end Get_Program;

   ------------------------
   -- Get_Standard_Entry --
   ------------------------

   function Get_Standard_Entry (Plugin : access Aquarius_Plugin_Type;
                                Name   : in     String)
                               return Aquarius.Entries.Table_Entry
   is
   begin
      return Plugin.Standard.Retrieve (Name);
   end Get_Standard_Entry;

   -----------------------
   -- Get_Standard_Type --
   -----------------------

   function Get_Standard_Type (Plugin : access Aquarius_Plugin_Type;
                               Name   : in     String)
                              return Aquarius.Types.Aquarius_Type
   is
      pragma Unreferenced (Plugin);
      pragma Unreferenced (Name);
   begin
      return null;
   end Get_Standard_Type;

   -------------
   -- Grammar --
   -------------

   function Grammar
     (Plugin : Aquarius_Plugin_Type'Class)
      return Aquarius.Grammars.Aquarius_Grammar
   is
   begin
      return Plugin.Grammar;
   end Grammar;

   -----------------------
   -- Have_Action_Group --
   -----------------------

   function Have_Action_Group (Plugin : Aquarius_Plugin_Type;
                               Name   : String)
                              return Boolean
   is
   begin
      return Plugin.Action_Groups.Contains (To_Plugin_Group_Name (Name));
   end Have_Action_Group;

   ----------
   -- Load --
   ----------

   procedure Load (Plugin  : not null access Aquarius_Plugin_Type;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar)
   is
   begin
      Plugin.Grammar  := Grammar;
      Plugin.Standard := Aquarius.Entries.New_Symbol_Table ("standard");
      Aquarius.Properties.Create_Property
        (Grammar.all, Plugin.Change_Flag, "plugin-change-flag",
         Inherited => False,
         Has_Value => False);
      Plugin.VM_Env :=
        Aquarius.VM.New_Environment
          ("plugin-" & Aquarius_Plugin_Type'Class (Plugin.all).Name,
           Aquarius.VM.Library.Standard_Library);
      Aquarius.VM.Insert (Plugin.VM_Env, "grammar",
                                 Aquarius.VM.To_Value (Grammar));
   end Load;

   -----------------------
   -- Load_Program_Tree --
   -----------------------

   function Load_Program_Tree
     (Plugin : Aquarius_Plugin_Type'Class;
      Path   : String)
      return Aquarius.Programs.Program_Tree
   is
   begin
      return Aquarius.Loader.Load_From_File
        (Plugin.Grammar, Path);
   end Load_Program_Tree;

   -----------------
   -- New_Command --
   -----------------

   procedure New_Command
     (Plugin        : not null access Aquarius_Plugin_Type'Class;
      Internal_Name : String;
      External_Name : String;
      Menu_Path     : String;
      Description   : String;
      Definition    : Aquarius.VM.VM_Value)
   is
      use Aquarius.VM;
   begin
      Insert (Plugin.VM_Env, Internal_Name, Definition);
      Set_Property (Plugin.VM_Env, Internal_Name, "external_name",
                    To_Value (External_Name));
      Set_Property (Plugin.VM_Env, Internal_Name, "menu_path",
                    To_Value (Menu_Path));
      Set_Property (Plugin.VM_Env, Internal_Name, "description",
                    To_Value (Description));

--        declare
--           Cmd : constant Aquarius.UI.Menus.Menu_Command :=
--                   new Plugin_Menu_Command'
--                     (Aquarius.UI.Menus.Root_Menu_Command with
--                      Plugin    => Aquarius_Plugin (Plugin),
--                      Command   => Definition);
--        begin
--           Aquarius.UI.Menus.Add_Submenu
--             (Plugin.Grammar.Get_Menu,
--              Aquarius.UI.Menus.New_Menu
--                (External_Name, Cmd));
--        end;
   end New_Command;

   --------------------
   -- Object_Changed --
   --------------------

   overriding
   procedure Object_Changed
     (W       : in out Aquarius_Plugin_Type;
      Item    : not null access Aquarius.Root_Aquarius_Object'Class;
      Context : not null access Aquarius.Root_Aquarius_Object'Class)
   is
      Tree : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree (Context);
      Name : constant Plugin_Map_Name := To_Plugin_Name (Tree.Name);
   begin
      if W.Change_Handlers.Contains (Name) and then
        not Tree.Has_Property (W.Change_Flag)
      then
         Tree.Set_Property (W.Change_Flag);
         declare
            Handler : constant Change_Handler :=
              W.Change_Handlers.Element (Name);
         begin
            Handler (Tree, Item);
            Aquarius.Trees.Properties.Get_Interactor (Tree.all).Update (Tree);
         end;
         Tree.Clear_Property (W.Change_Flag);
      end if;
   end Object_Changed;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : in     String;
      Group       : in     Aquarius.Actions.Action_Group;
      Position    : in     Rule_Position;
      Action      : in     Aquarius.Actions.Node_Action)
   is
      Defn : constant Aquarius.Syntax.Syntax_Tree :=
        Plugin.Grammar.Get_Definition (Syntax_Name);
   begin
      Aquarius.Actions.Set_Action
        (Defn, Group, Position,
         Aquarius.Actions.Create_Action_Execution (Action));
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "register action: " & Plugin.Grammar.Name &
           ": no such syntax rule: " &
           Syntax_Name;
   end Register_Action;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (Plugin      : not null access Aquarius_Plugin_Type;
      Parent_Name : in     String;
      Child_Name  : in     String;
      Group       : in     Aquarius.Actions.Action_Group;
      Position    : in     Rule_Position;
      Action      : in     Aquarius.Actions.Parent_Action)
   is
   begin
      Aquarius.Actions.Set_Action
        (Source   => Plugin.Grammar.Get_Definition (Parent_Name),
         Child    => Plugin.Grammar.Get_Definition (Child_Name),
         Group    => Group,
         Position => Position,
         Action   => Aquarius.Actions.Create_Action_Execution (Action));
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "register parent/child action: " & Plugin.Grammar.Name &
           ": no such syntax rule: " &
           Parent_Name;
   end Register_Action;

   -----------------------------
   -- Register_Change_Handler --
   -----------------------------

   procedure Register_Change_Handler
     (Plugin      : not null access Aquarius_Plugin_Type'Class;
      Syntax_Name : in              String;
      Handler     : in              Change_Handler)
   is
   begin

      Plugin.Change_Handlers.Insert (To_Plugin_Name (Syntax_Name),
                                     Handler);

   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "grammar " & Plugin.Grammar.Name & ": no such syntax rule: " &
           Syntax_Name;
   end Register_Change_Handler;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : in     String;
      Rule        : in     Aquarius.Formats.Aquarius_Format)
   is
   begin

      Plugin.Grammar.Get_Definition (Syntax_Name).Set_Format (Rule);
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "grammar " & Plugin.Grammar.Name & ": no such syntax rule: " &
           Syntax_Name;
   end Register_Rule;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : in     String;
      Rule        : in     Aquarius.Formats.Format_Rule)
   is
   begin
      Plugin.Register_Rule (Syntax_Name, Aquarius.Formats.Make_Format (Rule));
   end Register_Rule;

   -------------------
   -- Register_Rule --
   -------------------

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : in     String;
      Rule_1      : in     Aquarius.Formats.Format_Rule;
      Rule_2      : in     Aquarius.Formats.Format_Rule)
   is
   begin
      Plugin.Register_Rule (Syntax_Name,
                            Aquarius.Formats.Make_Format ((Rule_1, Rule_2)));
   end Register_Rule;

   --------------------------
   -- To_Plugin_Group_Name --
   --------------------------

   function To_Plugin_Group_Name (Name : String) return Plugin_Group_Name is
      Result : Plugin_Group_Name;
   begin
      Ada.Strings.Fixed.Move (Name, Result,
                              Drop => Ada.Strings.Right);
      return Result;
   end To_Plugin_Group_Name;

   --------------------
   -- To_Plugin_Name --
   --------------------

   function To_Plugin_Name (Name : String) return Plugin_Map_Name is
      Result : Plugin_Map_Name;
   begin
      Ada.Strings.Fixed.Move (Name, Result,
                              Drop => Ada.Strings.Right);
      return Result;
   end To_Plugin_Name;

end Aquarius.Plugins;
