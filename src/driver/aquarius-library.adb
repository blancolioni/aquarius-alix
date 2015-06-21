with Aquarius.Configuration;
with Aquarius.File_System_Stores;

with Komnenos.Fragments;

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
   begin
      Aquarius.Configuration.Load_Configuration;
      Local_Options (Plugins_Enabled) := Enable_Plugins;
      Local_Options (Show_Paths_In_Messages_Enabled) := Show_Paths_In_Messages;
      Aquarius.File_System_Stores.Register;
      Komnenos.Fragments.Register;
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
