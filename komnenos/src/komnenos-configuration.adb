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

   ---------------------------
   -- Get_Connector_Metrics --
   ---------------------------

   procedure Get_Connector_Metrics
     (Class_Name   : String;
      Colour       : out Aquarius.Colours.Aquarius_Colour;
      Line_Width   : out Positive;
      Arrow_Length : out Positive;
      Arrow_Width  : out Positive)
   is
   begin
      Check_Configuration;

      if Komnenos_Config.Contains ("connectors") then
         declare
            Connector_Config : constant Tropos.Configuration :=
                                 Komnenos_Config.Child ("connectors");
            Config           : constant Tropos.Configuration :=
                                 (if Connector_Config.Contains (Class_Name)
                                  then Connector_Config.Child (Class_Name)
                                  else Connector_Config.Child ("default"));
         begin
            Colour :=
              Aquarius.Colours.From_String
                (Config.Get ("colour", "pink"));
            Line_Width := Config.Get ("line_width", 2);
            Arrow_Length := Config.Get ("arrow_length", 8);
            Arrow_Width := Config.Get ("arrow_width", 4);
         end;
      else
         Colour :=
           Aquarius.Colours.From_String ("pink");
         Line_Width := 2;
         Arrow_Length := 8;
         Arrow_Width := 4;
      end if;
   end Get_Connector_Metrics;

end Komnenos.Configuration;
