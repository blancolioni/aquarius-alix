with Aquarius.Colours;

package Komnenos.Configuration is

   function Get_Colour
     (Principle_Name : String;
      Secondary_Name : String := "")
      return Aquarius.Colours.Aquarius_Colour;

   function Enabled (Setting_Name : String) return Boolean;

   procedure Get_Connector_Metrics
     (Class_Name   : String;
      Colour       : out Aquarius.Colours.Aquarius_Colour;
      Line_Width   : out Positive;
      Arrow_Length : out Positive;
      Arrow_Width  : out Positive);

end Komnenos.Configuration;
