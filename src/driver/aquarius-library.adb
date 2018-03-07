with Aquarius.Configuration;
with Aquarius.File_System_Stores;
with Aquarius.Grammars;
with Aquarius.Paths;
with Aquarius.Plugins.Manager;
with Aquarius.Programs;
with Aquarius.Trees.Properties;

with Komnenos.Connectors;
with Komnenos.Entities.Tables;
with Komnenos.Fragments;

with Aqua.Execution;
with Aqua.IO;
with Aqua.Primitives.Init;

package body Aquarius.Library is

   type Option_Type is (Plugins_Enabled, Show_Paths_In_Messages_Enabled);

   Local_Options : array (Option_Type) of Boolean :=
                     (Plugins_Enabled => True,
                      Show_Paths_In_Messages_Enabled => False);

   function Load_File
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   --------------------
   -- Enable_Plugins --
   --------------------

   procedure Enable_Plugins (Enabled : Boolean) is
   begin
      Local_Options (Plugins_Enabled) := Enabled;
   end Enable_Plugins;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Enable_Plugins         : Boolean := True;
      Show_Paths_In_Messages : Boolean := False)
   is
   begin
      Aquarius.Configuration.Load_Configuration;
      Aqua.IO.Set_IO_Path (Aquarius.Paths.Scratch_Path);
      Aqua.Primitives.Init.Create_Primitives;
      Local_Options (Plugins_Enabled) := Enable_Plugins;
      Local_Options (Show_Paths_In_Messages_Enabled) := Show_Paths_In_Messages;
      Aquarius.File_System_Stores.Register;
      Komnenos.Fragments.Register;
      Komnenos.Connectors.Register;

      Komnenos.Entities.Tables.New_Table
        (Name  => "/",
         Store => Komnenos.Entities.Null_Program_Store);

      Aqua.Primitives.New_Primitive_Function
        (Name           => "tree__load_file",
         Argument_Count => 2,
         Handler        => Load_File'Access);

   end Initialise;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Path    : constant String :=
                  Context.To_String (Arguments (2));
      Program : constant Aquarius.Programs.Program_Tree :=
                  Aquarius.Programs.Program_Tree
                    (Context.To_External_Object (Arguments (1)));
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                  Aquarius.Trees.Properties.Get_Grammar (Program);
      Plugin  : constant Aquarius.Plugins.Aquarius_Plugin :=
                  Aquarius.Plugins.Manager.Get_Plugin
                    (Grammar.Name);
      Tree    : constant Aquarius.Programs.Program_Tree :=
                  Plugin.Get_Program (Path);
   begin
      return Context.To_Word (Tree);
   end Load_File;

   ---------------------
   -- Plugins_Enabled --
   ---------------------

   function Plugins_Enabled return Boolean is
   begin
      return Local_Options (Plugins_Enabled);
   end Plugins_Enabled;

   ----------------------------
   -- Show_Paths_In_Messages --
   ----------------------------

   function Show_Paths_In_Messages return Boolean is
   begin
      return Local_Options (Show_Paths_In_Messages_Enabled);
   end Show_Paths_In_Messages;

end Aquarius.Library;
