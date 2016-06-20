with Aquarius.Colours;
with Aquarius.Configuration;
with Aquarius.Fonts;

package body Aquarius.Themes is

   Current_Active_Theme : Aquarius_Theme;

   function Get_Colour
     (Config     : Aquarius.Configuration.Cursor;
      Child_Name : String;
      Default_Colour : Aquarius.Colours.Aquarius_Colour)
      return Aquarius.Colours.Aquarius_Colour with Unreferenced;

   ------------------
   -- Active_Theme --
   ------------------

   function Active_Theme return Aquarius_Theme is
   begin
      if Current_Active_Theme = null then
         Current_Active_Theme := Load_Theme ("default");
      end if;
      return Current_Active_Theme;
   end Active_Theme;

   -----------------------
   -- Default_Font_Name --
   -----------------------

   function Default_Font_Name
     (Theme : Aquarius_Root_Theme)
      return String
   is
   begin
      return Aquarius.Names.To_String (Theme.Default_Font_Name);
   end Default_Font_Name;

   -----------------------
   -- Default_Font_Size --
   -----------------------

   function Default_Font_Size
     (Theme : Aquarius_Root_Theme)
      return Natural
   is
   begin
      return Theme.Default_Font_Size;
   end Default_Font_Size;

   ------------------------
   -- Default_Link_Style --
   ------------------------

   function Default_Link_Style
     (Theme : Aquarius_Root_Theme)
      return Aquarius.Styles.Aquarius_Style
   is
   begin
      return Theme.Default_Link_Style;
   end Default_Link_Style;

   -------------------
   -- Default_Style --
   -------------------

   function Default_Style
     (Theme : Aquarius_Root_Theme)
      return Aquarius.Styles.Aquarius_Style
   is
   begin
      return Theme.Default_Style;
   end Default_Style;

   ----------------
   -- Get_Colour --
   ----------------

   function Get_Colour
     (Config     : Aquarius.Configuration.Cursor;
      Child_Name : String;
      Default_Colour : Aquarius.Colours.Aquarius_Colour)
      return Aquarius.Colours.Aquarius_Colour
   is
      use Aquarius.Configuration;
      Child : constant Cursor := Find_Child (Config, Child_Name);
   begin
      if not Has_Element (Child) then
         return Default_Colour;
      elsif Child_Count (Child) = 0 then
         return Aquarius.Colours.From_String
           (Get_Value (Config, Child_Name));
      else
         declare
            R : constant Integer := Get_Value (Child, "r");
            G : constant Integer := Get_Value (Child, "g");
            B : constant Integer := Get_Value (Child, "b");
         begin
            return Aquarius.Colours.From_RGB (R, G, B);
         end;
      end if;
   end Get_Colour;

   ----------------
   -- Load_Theme --
   ----------------

   function Load_Theme (Name : String) return Aquarius_Theme is
      use Aquarius.Configuration;
      Theme  : constant Cursor := Get_Cursor ("/themes/" & Name);
      Classes : constant Cursor := Get_Cursor ("/themes/" & Name & "/classes");
      Fixed_Font : constant Cursor :=
                     Get_Cursor ("/themes/" & Name & "/fixed_font");
      Result : Aquarius_Theme;
   begin
      if Has_Element (Theme) then
         Result := new Aquarius_Root_Theme;
         for I in 1 .. Child_Count (Classes) loop
            declare
               use Aquarius.Styles;

               Child       : constant Cursor  := Get_Child (Classes, I);
               Class       : constant String  := Get_Name (Child);
               Font_Name   : constant String :=
                               Get_Value (Child, "font_family",
                                          Get_Value
                                            (Fixed_Font, "font_family",
                                             "courier"));
               Font_Size   : constant String :=
                               Get_Value (Child, "font_size",
                                          Get_Value
                                            (Fixed_Font, "font_size",
                                             "10"));
               State_Text  : constant String :=
                               Get_Value (Child, "state", "normal");
               Bold        : constant Boolean := Get_Value (Child, "bold");
               Italic      : constant Boolean := Get_Value (Child, "italic");
               Underlined  : constant Boolean :=
                               Get_Value (Child, "underline");
               Foreground  : constant String :=
                               Get_Value (Child, "foreground", "");
               Background  : constant String  :=
                               Get_Value (Child, "background", "");
               Cursor_Text : constant String :=
                               Get_Value (Child, "mouse_cursor", "default");
               Font        : Aquarius.Fonts.Aquarius_Font :=
                               Aquarius.Fonts.Create_Font
                                 (Name       => Font_Name,
                                  Size       => Natural'Value (Font_Size),
                                  Bold       => Bold,
                                  Italic     => Italic,
                                  Underlined => Underlined);
            begin
               if Foreground /= "" then
                  Aquarius.Fonts.Set_Foreground
                    (Font,
                     Aquarius.Colours.From_String (Foreground));
               end if;

               if Background /= "" then
                  Aquarius.Fonts.Set_Background
                    (Font,
                     Aquarius.Colours.From_String (Background));
               end if;

               declare
                  Style       : constant Aquarius.Styles.Aquarius_Style :=
                                  Aquarius.Styles.Create_Style
                                    (Name         => Class & "-" & State_Text,
                                     Font         => Font,
                                     Mouse_Cursor =>
                                       Mouse_Cursor_Type'Value (Cursor_Text));
                  State       : constant Element_State :=
                                  Element_State'Value (State_Text);
               begin
                  if Class = "default" then
                     Result.Default_Style := Style;
                     Result.Default_Link_Style := Style;
                  end if;
                  if Class = "link" then
                     Result.Default_Link_Style := Style;
                  end if;
                  Result.Entries.Append
                    ((Class => Aquarius.Names.To_Aquarius_Name (Class),
                      State => State,
                      Style => Style));
               end;
            end;
         end loop;
         Current_Active_Theme := Result;
         return Result;
      elsif Name = "default" then
         Aquarius.Configuration.Error ("no default theme defined");
         return null;
      else
         Aquarius.Configuration.Warning ("no theme name '" & Name &
                                           "'; trying default ...");
         return Load_Theme ("default");
      end if;
   end Load_Theme;

   -----------
   -- Style --
   -----------

   function Style (Theme : access Aquarius_Root_Theme;
                   Class : in     String;
                   State :        Element_State := Normal)
                   return Aquarius.Styles.Aquarius_Style
   is
      use type Aquarius.Names.Aquarius_Name;
   begin
      for T of Theme.Entries loop
         if T.Class = Class and then T.State = State then
            return T.Style;
         end if;
      end loop;
      return Theme.Default_Style;
   end Style;

end Aquarius.Themes;
