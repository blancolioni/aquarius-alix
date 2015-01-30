package Aquarius.Library is

   procedure Initialise
     (Enable_Plugins         : Boolean := True;
      Show_Paths_In_Messages : Boolean := False);

   function Plugins_Enabled return Boolean;
   function Show_Paths_In_Messages return Boolean;

   procedure Enable_Plugins (Enabled : Boolean);

end Aquarius.Library;
