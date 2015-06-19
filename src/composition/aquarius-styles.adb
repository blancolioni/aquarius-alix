package body Aquarius.Styles is

   ------------------
   -- Create_Style --
   ------------------

   function Create_Style
     (Name         : String;
      Font         : Aquarius.Fonts.Aquarius_Font;
      Mouse_Cursor : Mouse_Cursor_Type := Default)
      return Aquarius_Style
   is
   begin
      return new Aquarius_Root_Style'
        (Name         => Aquarius.Names.To_Aquarius_Name (Name),
         Font         => Font,
         Mouse_Cursor => Mouse_Cursor);
   end Create_Style;

   ----------
   -- Font --
   ----------

   function Font (Style : access Aquarius_Root_Style)
                  return Aquarius.Fonts.Aquarius_Font
   is
   begin
      return Style.Font;
   end Font;

   ------------------
   -- Mouse_Cursor --
   ------------------

   function Mouse_Cursor (Style : Aquarius_Style) return Mouse_Cursor_Type is
   begin
      return Style.Mouse_Cursor;
   end Mouse_Cursor;

   ----------
   -- Name --
   ----------

   function Name (Style : access Aquarius_Root_Style)
                  return String
   is
   begin
      return Aquarius.Names.To_String (Style.Name);
   end Name;

end Aquarius.Styles;
