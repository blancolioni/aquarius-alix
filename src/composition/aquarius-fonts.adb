package body Aquarius.Fonts is

   -----------------
   -- Create_Font --
   -----------------

   function Create_Font
     (Foreground : Aquarius.Colours.Aquarius_Colour;
      Bold       : Boolean         := False;
      Italic     : Boolean         := False;
      Underlined : Boolean         := False)
      return Aquarius_Font
   is
   begin
      return (Foreground, Aquarius.Colours.White,
              Bold, Italic, Underlined, True, False);
   end Create_Font;

   -----------------
   -- Create_Font --
   -----------------

   function Create_Font
     (Bold       : Boolean         := False;
      Italic     : Boolean         := False;
      Underlined : Boolean         := False)
      return Aquarius_Font
   is
   begin
      return (Aquarius.Colours.Black,
              Aquarius.Colours.White,
              Bold, Italic, Underlined, False, False);
   end Create_Font;

   --------------------
   -- Get_Background --
   --------------------

   function Get_Background
     (Font : Aquarius_Font)
      return Aquarius.Colours.Aquarius_Colour
   is
   begin
      if Font.Have_Background then
         return Font.Background;
      else
         return Aquarius.Colours.White;
      end if;
   end Get_Background;

   --------------------
   -- Get_Foreground --
   --------------------

   function Get_Foreground
     (Font : Aquarius_Font)
      return Aquarius.Colours.Aquarius_Colour
   is
   begin
      if Font.Have_Foreground then
         return Font.Foreground;
      else
         return Aquarius.Colours.Black;
      end if;
   end Get_Foreground;

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

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Font   : in out Aquarius_Font;
      Colour : in     Aquarius.Colours.Aquarius_Colour)
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

   procedure Set_Foreground
     (Font   : in out Aquarius_Font;
      Colour : in     Aquarius.Colours.Aquarius_Colour)
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

end Aquarius.Fonts;
