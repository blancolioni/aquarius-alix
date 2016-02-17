with Tropos.Reader;

with Aquarius.Config_Paths;

package body Komnenos.Configuration is

   Komnenos_Config : Tropos.Configuration;
   Have_Config     : Boolean := False;

   procedure Check_Configuration;

   -------------------------
   -- Check_Configuration --
   -------------------------

   procedure Check_Configuration is
   begin
      if not Have_Config then
         Komnenos_Config :=
           Tropos.Reader.Read_Config
             (Aquarius.Config_Paths.Config_File
                ("komnenos/komnenos.config"));
         Have_Config := True;
      end if;
   end Check_Configuration;

   -------------
   -- Enabled --
   -------------

   function Enabled (Setting_Name : String) return Boolean is
   begin
      Check_Configuration;
      if Komnenos_Config.Contains ("settings") then
         return Komnenos_Config.Child ("settings").Get (Setting_Name);
      else
         return False;
      end if;
   end Enabled;

   ----------------
   -- Get_Colour --
   ----------------

   function Get_Colour
     (Principle_Name : String;
      Secondary_Name : String := "")
      return Aquarius.Colours.Aquarius_Colour
   is
   begin
      Check_Configuration;

      declare
         Colour_Config : constant Tropos.Configuration :=
                           Komnenos_Config.Child ("colours");
         Colour        : constant String :=
                           (if Colour_Config.Contains (Principle_Name)
                            then Colour_Config.Get (Principle_Name)
                            else Colour_Config.Get (Secondary_Name, "purple"));
      begin
         return Aquarius.Colours.From_String (Colour);
      end;

   end Get_Colour;

end Komnenos.Configuration;
