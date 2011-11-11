with Ada.Strings.Fixed;

with Aquarius.Configuration;

package body Aquarius.Styles is

   Single_Default_Style : Aquarius_Style;

   function To_Entry_Name (Name : String) return Style_Entry_Name;

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
               Foreground : constant String  :=
                              Get_Value (Child, "foreground", "black");
               Background : constant String  :=
                              Get_Value (Child, "background", "");
               Font       : Aquarius.Fonts.Aquarius_Font :=
                 Aquarius.Fonts.Create_Font
                 (Aquarius.Fonts.Parse_Colour (Foreground),
                  Bold       => Bold,
                  Italic     => Italic,
                  Underlined => Underlined);
            begin
               if Background /= "" then
                  Aquarius.Fonts.Set_Background
                    (Font,
                     Aquarius.Fonts.Parse_Colour (Background));
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
