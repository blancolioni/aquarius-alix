with Ada.Strings.Fixed;

with Aquarius.Colours;
with Aquarius.Configuration;

package body Aquarius.Styles is

   Single_Default_Style : Aquarius_Style;

   function To_Entry_Name (Name : String) return Style_Entry_Name;

   function Get_Colour
     (Config     : Aquarius.Configuration.Cursor;
      Child_Name : String;
      Default_Colour : Aquarius.Fonts.Aquarius_Colour)
      return Aquarius.Fonts.Aquarius_Colour;

   -------------------
   -- Default_Style --
   -------------------

   function Default_Style return Aquarius_Style is
   begin
      if Single_Default_Style = null then
         Single_Default_Style := Load_Style ("default");
      end if;
      return Single_Default_Style;
   end Default_Style;

   ----------
   -- Font --
   ----------

   function Font (Style : access Aquarius_Root_Style;
                  Class : in     String)
                 return Aquarius.Fonts.Aquarius_Font
   is
      Class_Name : constant Style_Entry_Name := To_Entry_Name (Class);
   begin
      for I in 1 .. Style.Entries.Last_Index loop
         if Style.Entries.Element (I).Name = Class_Name then
            return Style.Entries.Element (I).Font;
         end if;
      end loop;
      return Style.Default_Font;
   end Font;

   ----------------
   -- Get_Colour --
   ----------------

   function Get_Colour
     (Config     : Aquarius.Configuration.Cursor;
      Child_Name : String;
      Default_Colour : Aquarius.Fonts.Aquarius_Colour)
      return Aquarius.Fonts.Aquarius_Colour
   is
      use Aquarius.Configuration;
      Child : constant Cursor := Find_Child (Config, Child_Name);
   begin
      if not Has_Element (Child) then
         return Default_Colour;
      elsif Child_Count (Child) = 0 then
         return Aquarius.Fonts.Parse_Colour
           (Get_Value (Config, Child_Name));
      else
         declare
            R : constant Integer := Get_Value (Child, "r");
            G : constant Integer := Get_Value (Child, "g");
            B : constant Integer := Get_Value (Child, "b");
         begin
            return Aquarius.Fonts.From_RGB
              (Aquarius.Fonts.Colour_Range (R),
               Aquarius.Fonts.Colour_Range (G),
               Aquarius.Fonts.Colour_Range (B));
         end;
      end if;
   end Get_Colour;

   ----------------
   -- Load_Style --
   ----------------

   function Load_Style (Name : String) return Aquarius_Style is
      use Aquarius.Configuration;
      Style  : constant Cursor := Get_Cursor ("/styles/" & Name);
      Result : Aquarius_Style;
   begin
      if Has_Element (Style) then
         Result := new Aquarius_Root_Style;
         for I in 1 .. Child_Count (Style) loop
            declare
               Child      : constant Cursor  := Get_Child (Style, I);
               Class      : constant String  := Get_Name (Child);
               Bold       : constant Boolean := Get_Value (Child, "bold");
               Italic     : constant Boolean := Get_Value (Child, "italic");
               Underlined : constant Boolean :=
                              Get_Value (Child, "underline");
               Foreground : constant Aquarius.Fonts.Aquarius_Colour :=
                              Get_Colour (Child, "foreground",
                                          Aquarius.Fonts.Black);
               Background : constant String  :=
                              Get_Value (Child, "background", "");
               Font       : Aquarius.Fonts.Aquarius_Font :=
                 Aquarius.Fonts.Create_Font
                 (Foreground => Foreground,
                  Bold       => Bold,
                  Italic     => Italic,
                  Underlined => Underlined);
            begin
               if Background /= "" then
                  Aquarius.Fonts.Set_Background
                    (Font,
                     Aquarius.Colours.Parse_Colour (Background));
               end if;
               if Class = "default" then
                  Result.Default_Font := Font;
               end if;
               Result.Entries.Append ((To_Entry_Name (Class), Font));
            end;
         end loop;
         return Result;
      elsif Name = "default" then
         Aquarius.Configuration.Error ("no default style defined");
         return null;
      else
         Aquarius.Configuration.Warning ("no style name '" & Name &
                                           "'; trying default ...");
         return Load_Style ("default");
      end if;
   end Load_Style;

   -------------------
   -- To_Entry_Name --
   -------------------

   function To_Entry_Name (Name : String) return Style_Entry_Name is
      Class_Name : Style_Entry_Name;
   begin
      Ada.Strings.Fixed.Move (Name, Class_Name,
                              Drop => Ada.Strings.Right);
      return Class_Name;
   end To_Entry_Name;

end Aquarius.Styles;
