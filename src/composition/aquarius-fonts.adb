with Aquarius.Configuration;

package body Aquarius.Fonts is

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

   -----------------
   -- Create_Font --
   -----------------

   function Create_Font (Foreground : Aquarius_Colour;
                         Bold       : Boolean         := False;
                         Italic     : Boolean         := False;
                         Underlined : Boolean         := False)
                        return Aquarius_Font
   is
   begin
      return (Foreground, White, Bold, Italic, Underlined, True, False);
   end Create_Font;

   -----------------
   -- Create_Font --
   -----------------

   function Create_Font (Bold       : Boolean         := False;
                         Italic     : Boolean         := False;
                         Underlined : Boolean         := False)
                        return Aquarius_Font
   is
   begin
      return (Black, White, Bold, Italic, Underlined, False, False);
   end Create_Font;

   --------------------
   -- Get_Background --
   --------------------

   function Get_Background (Font : Aquarius_Font) return Aquarius_Colour is
   begin
      if Font.Have_Background then
         return Font.Background;
      else
         return White;
      end if;
   end Get_Background;

   --------------------
   -- Get_Foreground --
   --------------------

   function Get_Foreground (Font : Aquarius_Font) return Aquarius_Colour is
   begin
      if Font.Have_Foreground then
         return Font.Foreground;
      else
         return Black;
      end if;
   end Get_Foreground;

   -----------
   -- Green --
   -----------

   function Green (Colour : Aquarius_Colour) return Colour_Range is
   begin
      return Colour.Green;
   end Green;

   --------------------
   -- Has_Background --
   --------------------

   function Has_Background (Font : Aquarius_Font) return Boolean is
   begin
      return Font.Have_Background;
   end Has_Background;

   --------------------
   -- Has_Foreground --
   --------------------

   function Has_Foreground (Font : Aquarius_Font) return Boolean is
   begin
      return Font.Have_Foreground;
   end Has_Foreground;

   ----------------
   -- Hex_Colour --
   ----------------

   function Hex_Colour (Item : Colour_Range) return String is
      Hex_Digit : constant array (Colour_Range range 0 .. 15) of Character :=
        "0123456789ABCDEF";

   begin
      return (Hex_Digit (Item / 16), Hex_Digit (Item mod 16));
   end Hex_Colour;

   -------------
   -- Is_Bold --
   -------------

   function Is_Bold        (Font : Aquarius_Font) return Boolean is
   begin
      return Font.Bold;
   end Is_Bold;

   ---------------
   -- Is_Italic --
   ---------------

   function Is_Italic (Font : Aquarius_Font) return Boolean is
   begin
      return Font.Italic;
   end Is_Italic;

   -------------------
   -- Is_Underlined --
   -------------------

   function Is_Underlined (Font : Aquarius_Font) return Boolean is
   begin
      return Font.Underlined;
   end Is_Underlined;

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

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Font   : in out Aquarius_Font;
                             Colour : in     Aquarius_Colour)
   is
   begin
      Font.Background := Colour;
      Font.Have_Background := True;
   end Set_Background;

   --------------
   -- Set_Bold --
   --------------

   procedure Set_Bold (Font  : in out Aquarius_Font;
                       Value : in     Boolean)
   is
   begin
      Font.Bold := Value;
   end Set_Bold;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground (Font   : in out Aquarius_Font;
                             Colour : in     Aquarius_Colour)
   is
   begin
      Font.Foreground := Colour;
      Font.Have_Foreground := True;
   end Set_Foreground;

   ----------------
   -- Set_Italic --
   ----------------

   procedure Set_Italic (Font  : in out Aquarius_Font;
                         Value : in     Boolean)
   is
   begin
      Font.Italic := Value;
   end Set_Italic;

   --------------------
   -- Set_Underlined --
   --------------------

   procedure Set_Underlined (Font  : in out Aquarius_Font;
                             Value : in     Boolean)
   is
   begin
      Font.Underlined := Value;
   end Set_Underlined;

   -----------
   -- White --
   -----------

   function White return Aquarius_Colour is
   begin
      return Parse_Colour ("white");
   end White;

end Aquarius.Fonts;
