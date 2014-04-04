with Aquarius.Colours;

package Aquarius.Fonts is

   type Aquarius_Font is private;

   function Is_Bold        (Font : Aquarius_Font) return Boolean;
   function Is_Italic      (Font : Aquarius_Font) return Boolean;
   function Is_Underlined  (Font : Aquarius_Font) return Boolean;
   function Has_Foreground (Font : Aquarius_Font) return Boolean;
   function Has_Background (Font : Aquarius_Font) return Boolean;

   function Get_Foreground (Font : Aquarius_Font)
                            return Aquarius.Colours.Aquarius_Colour;
   function Get_Background (Font : Aquarius_Font)
                            return Aquarius.Colours.Aquarius_Colour;

   function Create_Font (Foreground : Aquarius.Colours.Aquarius_Colour;
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
                             Colour : in     Aquarius.Colours.Aquarius_Colour);

   procedure Set_Background (Font   : in out Aquarius_Font;
                             Colour : in     Aquarius.Colours.Aquarius_Colour);

private

   type Aquarius_Font is
      record
         Foreground               : Aquarius.Colours.Aquarius_Colour;
         Background               : Aquarius.Colours.Aquarius_Colour;
         Bold, Italic, Underlined : Boolean           := False;
         Have_Foreground          : Boolean           := False;
         Have_Background          : Boolean           := False;
      end record;

end Aquarius.Fonts;
