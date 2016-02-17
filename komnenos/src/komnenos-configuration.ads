with Aquarius.Colours;

package Komnenos.Configuration is

   function Get_Colour
     (Principle_Name : String;
      Secondary_Name : String := "")
      return Aquarius.Colours.Aquarius_Colour;

   function Enabled (Setting_Name : String) return Boolean;

end Komnenos.Configuration;
