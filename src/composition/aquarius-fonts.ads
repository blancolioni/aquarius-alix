package Aquarius.Fonts is

   type Aquarius_Font is private;

   function Is_Bold        (Font : Aquarius_Font) return Boolean;
   function Is_Italic      (Font : Aquarius_Font) return Boolean;
   function Is_Underlined  (Font : Aquarius_Font) return Boolean;
   function Has_Foreground (Font : Aquarius_Font) return Boolean;
   function Has_Background (Font : Aquarius_Font) return Boolean;

   type Aquarius_Colour is private;
   type Colour_Range is range 0 .. 255;

   function Get_Foreground (Font : Aquarius_Font) return Aquarius_Colour;
   function Get_Background (Font : Aquarius_Font) return Aquarius_Colour;

   function Parse_Colour (Colour_Spec : String) return Aquarius_Colour;

   function Create_Font (Foreground : Aquarius_Colour;
                         Bold       : Boolean         := False;
                         Italic     : Boolean         := False;
                         Underlined : Boolean         := False)
                        return Aquarius_Font;

   function Create_Font (Bold       : Boolean         := False;
                         Italic     : Boolean         := False;
                         Underlined : Boolean         := False)
                        return Aquarius_Font;

   procedure Set_Bold (Font  : in out Aquarius_Font;
                       Value : in     Boolean);

   procedure Set_Italic (Font  : in out Aquarius_Font;
                         Value : in     Boolean);

   procedure Set_Underlined (Font  : in out Aquarius_Font;
                             Value : in     Boolean);

   procedure Set_Foreground (Font   : in out Aquarius_Font;
                             Colour : in     Aquarius_Colour);

   procedure Set_Background (Font   : in out Aquarius_Font;
                             Colour : in     Aquarius_Colour);

   function Black return Aquarius_Colour;
   function White return Aquarius_Colour;

   function Red   (Colour : Aquarius_Colour) return Colour_Range;
   function Green (Colour : Aquarius_Colour) return Colour_Range;
   function Blue  (Colour : Aquarius_Colour) return Colour_Range;

   function Hex_Colour (Item : Colour_Range) return String;

   function From_RGB (R, G, B : Colour_Range) return Aquarius_Colour;

private

   type Aquarius_Colour is
      record
         Red, Green, Blue : Colour_Range;
      end record;

   type Aquarius_Font is
      record
         Foreground               : Aquarius_Colour;
         Background               : Aquarius_Colour;
         Bold, Italic, Underlined : Boolean           := False;
         Have_Foreground          : Boolean           := False;
         Have_Background          : Boolean           := False;
      end record;

end Aquarius.Fonts;
