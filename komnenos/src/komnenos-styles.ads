package Komnenos.Styles is

   type Komnenos_Style is private;

   type Mouse_Cursor_Type is
     (Default,
      Hand);

   function Name (Style : Komnenos_Style) return String;
   function Foreground_Colour (Style : Komnenos_Style) return String;
   function Background_Colour (Style : Komnenos_Style) return String;
   function Font_Name (Style : Komnenos_Style) return String;
   function Font_Size (Style : Komnenos_Style) return Natural;
   function Bold (Style : Komnenos_Style) return Boolean;
   function Italic (Style : Komnenos_Style) return Boolean;
   function Underlined (Style : Komnenos_Style) return Boolean;
   function Strike_Through (Style : Komnenos_Style) return Boolean;
   function Mouse_Cursor (Style : Komnenos_Style) return Mouse_Cursor_Type;

   function New_Style
     (Name           : String;
      Foreground     : String := "";
      Background     : String := "";
      Font_Name      : String := "";
      Font_Size      : Natural := 0;
      Bold           : Boolean := False;
      Italic         : Boolean := False;
      Underlined     : Boolean := False;
      Strike_Through : Boolean := False;
      Mouse_Cursor   : Mouse_Cursor_Type := Default)
      return Komnenos_Style;

   function New_Style
     (Name           : String;
      Base           : Komnenos_Style;
      Foreground     : String := "";
      Background     : String := "";
      Font_Name      : String := "";
      Font_Size      : Natural := 0;
      Bold           : Boolean := False;
      Italic         : Boolean := False;
      Underlined     : Boolean := False;
      Strike_Through : Boolean := False;
      Mouse_Cursor   : Mouse_Cursor_Type := Default)
      return Komnenos_Style;

   function Find_Style
     (Name : String)
      return Komnenos_Style;

   function Default_Style return Komnenos_Style;

   function Default_Fixed_Style return Komnenos_Style;

   Null_Style : constant Komnenos_Style;

   type Element_State is (Normal, Hover, Selected, Disabled);

   type Style_Collection is array (Element_State) of Komnenos_Style;

private

   type Komnenos_Style_Record;

   type Komnenos_Style is access Komnenos_Style_Record;

   Null_Style : constant Komnenos_Style := null;

end Komnenos.Styles;
