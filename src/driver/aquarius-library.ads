package Aquarius.Library is

   procedure Initialise
     (Enable_Plugins : Boolean := True);

   function Plugins_Enabled return Boolean;

   procedure Enable_Plugins (Enabled : Boolean);

end Aquarius.Library;
