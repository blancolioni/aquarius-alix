package body Aquarius.Colours is

   function To_Hex (Value : Natural) return String
     with Pre => Value < 256;

   -----------
   -- Black --
   -----------

   function Black return Aquarius_Colour is
   begin
      return From_String ("white");
   end Black;

   --------------
   -- From_RGB --
   --------------

   function From_RGB (R, G, B : Natural) return Aquarius_Colour is
   begin
      return From_RGBA (R, G, B, 255);
   end From_RGB;

   ---------------
   -- From_RGBA --
   ---------------

   function From_RGBA
     (R, G, B, A  : Natural)
      return Aquarius_Colour
   is
   begin
      return From_String ("#" & To_Hex (R) & To_Hex (G)
                          & To_Hex (B) & To_Hex (A));
   end From_RGBA;

   -----------------
   -- From_String --
   -----------------

   function From_String
     (String_Spec : String)
      return Aquarius_Colour
   is
   begin
      return (Text => Aquarius.Names.To_Aquarius_Name (String_Spec));
   end From_String;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (Value : Natural) return String is
      Hex_Digit : constant array (0 .. 15) of Character :=
        "0123456789ABCDEF";

   begin
      return (Hex_Digit (Value / 16), Hex_Digit (Value mod 16));
   end To_Hex;

   function To_String
     (Colour : Aquarius_Colour)
      return String
   is
   begin
      return Aquarius.Names.To_String (Colour.Text);
   end To_String;

   -----------
   -- White --
   -----------

   function White return Aquarius_Colour is
   begin
      return From_String ("white");
   end White;

end Aquarius.Colours;
