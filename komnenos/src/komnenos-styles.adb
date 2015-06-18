with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package body Komnenos.Styles is

   type Komnenos_Style_Record is
      record
         Name              : Unbounded_String;
         Foreground_Colour : Unbounded_String;
         Background_Colour : Unbounded_String;
         Font_Name         : Unbounded_String;
         Font_Size         : Natural := 0;
         Bold, Italic      : Boolean        := False;
         Underlined        : Boolean        := False;
         Strike_Through    : Boolean        := False;
         Mouse_Cursor      : Mouse_Cursor_Type := Default;
      end record;

   package Style_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Komnenos_Style,
        Hash            => Hash,
        Equivalent_Keys => "=");

   Style_Map : Style_Maps.Map;

   -----------------------
   -- Background_Colour --
   -----------------------

   function Background_Colour (Style : Komnenos_Style) return String is
   begin
      return To_String (Style.Background_Colour);
   end Background_Colour;

   ----------
   -- Bold --
   ----------

   function Bold (Style : Komnenos_Style) return Boolean is
   begin
      return Style.Bold;
   end Bold;

   -------------------------
   -- Default_Fixed_Style --
   -------------------------

   function Default_Fixed_Style return Komnenos_Style is
   begin
      return Find_Style ("default_fixed");
   end Default_Fixed_Style;

   -------------------
   -- Default_Style --
   -------------------

   function Default_Style return Komnenos_Style is
   begin
      return Find_Style ("default");
   end Default_Style;

   ----------------
   -- Find_Style --
   ----------------

   function Find_Style
     (Name : String)
      return Komnenos_Style
   is
      Position : constant Style_Maps.Cursor :=
                   Style_Map.Find (To_Unbounded_String (Name));
   begin
      if Style_Maps.Has_Element (Position) then
         return Style_Maps.Element (Position);
      else
         return Null_Style;
      end if;
   end Find_Style;

   ---------------
   -- Font_Name --
   ---------------

   function Font_Name (Style : Komnenos_Style) return String is
   begin
      return To_String (Style.Font_Name);
   end Font_Name;

   ---------------
   -- Font_Size --
   ---------------

   function Font_Size (Style : Komnenos_Style) return Natural is
   begin
      return Style.Font_Size;
   end Font_Size;

   -----------------------
   -- Foreground_Colour --
   -----------------------

   function Foreground_Colour (Style : Komnenos_Style) return String is
   begin
      return To_String (Style.Foreground_Colour);
   end Foreground_Colour;

   ------------
   -- Italic --
   ------------

   function Italic (Style : Komnenos_Style) return Boolean is
   begin
      return Style.Italic;
   end Italic;

   ------------------
   -- Mouse_Cursor --
   ------------------

   function Mouse_Cursor (Style : Komnenos_Style) return Mouse_Cursor_Type is
   begin
      if Style = null then
         return Default;
      else
         return Style.Mouse_Cursor;
      end if;
   end Mouse_Cursor;

   ----------
   -- Name --
   ----------

   function Name (Style : Komnenos_Style) return String is
   begin
      return To_String (Style.Name);
   end Name;

   ---------------
   -- New_Style --
   ---------------

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
      return Komnenos_Style
   is
   begin
      return New_Style
        (Name           => Name,
         Base           => Default_Style,
         Foreground     => Foreground,
         Background     => Background,
         Font_Name      => Font_Name,
         Font_Size      => Font_Size,
         Bold           => Bold,
         Italic         => Italic,
         Underlined     => Underlined,
         Strike_Through => Strike_Through,
         Mouse_Cursor   => Mouse_Cursor);
   end New_Style;

   ---------------
   -- New_Style --
   ---------------

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
      return Komnenos_Style
   is
      function "+" (Text : String) return Unbounded_String
      is (To_Unbounded_String (Text));

      Result : constant Komnenos_Style :=
                 new Komnenos_Style_Record;
   begin
      if Base /= null then
         Result.all := Base.all;
      end if;

      Result.Name := +Name;

      if Foreground /= "" then
         Result.Foreground_Colour := +Foreground;
      end if;

      if Background /= "" then
         Result.Background_Colour := +Background;
      end if;

      if Font_Name /= "" then
         Result.Font_Name := +Font_Name;
      end if;

      if Font_Size /= 0 then
         Result.Font_Size := Font_Size;
      end if;

      if Bold then
         Result.Bold := Bold;
      end if;

      if Italic then
         Result.Italic := Italic;
      end if;

      if Underlined then
         Result.Underlined := Underlined;
      end if;

      if Strike_Through then
         Result.Strike_Through := Strike_Through;
      end if;

      if Mouse_Cursor /= Default then
         Result.Mouse_Cursor := Mouse_Cursor;
      end if;

      Style_Map.Insert (Result.Name, Result);

      return Result;

   end New_Style;

   --------------------
   -- Strike_Through --
   --------------------

   function Strike_Through (Style : Komnenos_Style) return Boolean is
   begin
      return Style.Strike_Through;
   end Strike_Through;

   ----------------
   -- Underlined --
   ----------------

   function Underlined (Style : Komnenos_Style) return Boolean is
   begin
      return Style.Underlined;
   end Underlined;

end Komnenos.Styles;
