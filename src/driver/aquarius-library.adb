with Aquarius.Configuration;

package body Aquarius.Library is

   type Option_Type is (Plugins_Enabled);

   Local_Options : array (Option_Type) of Boolean :=
                     (Plugins_Enabled => True);

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Enable_Plugins : Boolean := True)
   is
   begin
      Aquarius.Configuration.Load_Configuration;
      Local_Options (Plugins_Enabled) := Enable_Plugins;
   end Initialise;

   ---------------------
   -- Plugins_Enabled --
   ---------------------

   function Plugins_Enabled return Boolean is
   begin
      return Local_Options (Plugins_Enabled);
   end Plugins_Enabled;

end Aquarius.Library;
