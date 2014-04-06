with Aquarius.Configuration;

package body Aquarius.Colours is

   -----------
   -- Black --
   -----------

   function Black return Aquarius_Colour is
   begin
      return Parse_Colour ("black");
   end Black;

   ----------
   -- Blue --
   ----------

   function Blue  (Colour : Aquarius_Colour) return Colour_Range is
   begin
      return Colour.Blue;
   end Blue;

   --------------
   -- From_RGB --
   --------------

   function From_RGB (R, G, B : Colour_Range) return Aquarius_Colour is
   begin
      return (R, G, B);
   end From_RGB;

   -----------
   -- Green --
   -----------

   function Green (Colour : Aquarius_Colour) return Colour_Range is
   begin
      return Colour.Green;
   end Green;

   ----------------
   -- Hex_Colour --
   ----------------

   function Hex_Colour (Item : Colour_Range) return String is
      Hex_Digit : constant array (Colour_Range range 0 .. 15) of Character :=
        "0123456789ABCDEF";

   begin
      return (Hex_Digit (Item / 16), Hex_Digit (Item mod 16));
   end Hex_Colour;

   ------------------
   -- Parse_Colour --
   ------------------

   function Parse_Colour (Colour_Spec : String) return Aquarius_Colour is
      use Aquarius.Configuration;
      Position : constant Cursor := Get_Cursor ("/colours/" & Colour_Spec);
      R, G, B  : Integer;
   begin
      R := Get_Value (Position, "r");
      G := Get_Value (Position, "g");
      B := Get_Value (Position, "b");
      return (Colour_Range (R), Colour_Range (G), Colour_Range (B));
   end Parse_Colour;

   ---------
   -- Red --
   ---------

   function Red   (Colour : Aquarius_Colour) return Colour_Range is
   begin
      return Colour.Red;
   end Red;

   -----------
   -- White --
   -----------

   function White return Aquarius_Colour is
   begin
      return Parse_Colour ("white");
   end White;

end Aquarius.Colours;
