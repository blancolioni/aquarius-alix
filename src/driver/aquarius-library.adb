with Aqua.Drivers;

with Aquarius.Command_Line;
with Aquarius.Configuration;
with Aquarius.File_System_Stores;
with Aquarius.Paths;
with Aquarius.Programs;
with Aquarius.Programs.Aqua_Driver;

with Komnenos.Connectors;
with Komnenos.Entities.Tables;
with Komnenos.Fragments;

with Aqua.IO;

package body Aquarius.Library is

   type Option_Type is (Plugins_Enabled, Show_Paths_In_Messages_Enabled);

   Local_Options : array (Option_Type) of Boolean :=
                     (Plugins_Enabled => True,
                      Show_Paths_In_Messages_Enabled => False);

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
      Plugin_Path : constant String := Aquarius.Command_Line.Plugin_Path;
   begin

      Aqua.Drivers.Register
        ("aquarius-program-tree",
         Aquarius.Programs.Aqua_Driver.Aquarius_Tree_Driver'Access);

      Aquarius.Configuration.Load_Configuration;
      if Plugin_Path /= "" then
         Aquarius.Configuration.Add_Grammar_Path (Plugin_Path);
      end if;
      Aqua.IO.Set_IO_Path (Aquarius.Paths.Scratch_Path);

      Local_Options (Plugins_Enabled) := Enable_Plugins;
      Local_Options (Show_Paths_In_Messages_Enabled) := Show_Paths_In_Messages;
      Aquarius.File_System_Stores.Register;
      Komnenos.Fragments.Register;
      Komnenos.Connectors.Register;

      Komnenos.Entities.Tables.New_Table
        (Name  => "/",
         Store => Komnenos.Entities.Null_Program_Store);

   end Initialise;

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
