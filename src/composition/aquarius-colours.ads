private with Aquarius.Names;

package Aquarius.Colours is

   type Aquarius_Colour is private;

   function From_String
     (String_Spec : String)
      return Aquarius_Colour;

   function From_RGB
     (R, G, B     : Natural)
      return Aquarius_Colour;

   function From_RGBA
     (R, G, B, A  : Natural)
      return Aquarius_Colour;

   function Black return Aquarius_Colour;
   function White return Aquarius_Colour;

   function To_String
     (Colour : Aquarius_Colour)
      return String;

private

   type Aquarius_Colour is
      record
         Text : Aquarius.Names.Aquarius_Name;
      end record;

end Aquarius.Colours;
