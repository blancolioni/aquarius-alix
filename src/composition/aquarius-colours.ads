package Aquarius.Colours is

   type Aquarius_Colour is private;
   type Colour_Range is range 0 .. 255;

   function Parse_Colour (Colour_Spec : String) return Aquarius_Colour;

   function Red   (Colour : Aquarius_Colour) return Colour_Range;
   function Green (Colour : Aquarius_Colour) return Colour_Range;
   function Blue  (Colour : Aquarius_Colour) return Colour_Range;

   function Hex_Colour (Item : Colour_Range) return String;

   function Black return Aquarius_Colour;
   function White return Aquarius_Colour;

   function From_RGB
     (R, G, B : Aquarius.Colours.Colour_Range)
      return Aquarius.Colours.Aquarius_Colour;

private

   type Aquarius_Colour is
      record
         Red, Green, Blue : Colour_Range;
      end record;

end Aquarius.Colours;
