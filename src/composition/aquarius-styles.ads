with Aquarius.Fonts;
private with Aquarius.Names;

package Aquarius.Styles is

   type Mouse_Cursor_Type is
     (Default,
      Hand);

   type Aquarius_Root_Style is tagged private;

   type Aquarius_Style is access all Aquarius_Root_Style'Class;

   Null_Style : constant Aquarius_Style := null;

   function Name (Style : access Aquarius_Root_Style)
                  return String;

   function Font (Style : access Aquarius_Root_Style)
                  return Aquarius.Fonts.Aquarius_Font;

   function Mouse_Cursor (Style : Aquarius_Style) return Mouse_Cursor_Type;

   function Create_Style
     (Name         : String;
      Font         : Aquarius.Fonts.Aquarius_Font;
      Mouse_Cursor : Mouse_Cursor_Type := Default)
      return Aquarius_Style;

private

   type Aquarius_Root_Style is tagged
      record
         Name         : Aquarius.Names.Aquarius_Name;
         Font         : Aquarius.Fonts.Aquarius_Font;
         Mouse_Cursor : Mouse_Cursor_Type;
      end record;

end Aquarius.Styles;
