with Komnenos.Configuration;

package body Komnenos.Fragments.Source is

   -----------------------
   -- New_Source_Fragment --
   -----------------------

   function New_Source_Fragment
     (Title : String;
      Path  : String)
      return Fragment_Type
   is
      Result : constant Fragment_Type := new Root_Fragment_Type;
   begin
      Result.Default_Style := Aquarius.Themes.Active_Theme.Default_Style;
      Result.Editable := True;
      Result.Background_Colour :=
        Komnenos.Configuration.Get_Colour
          ("source_background", "default_background");
      Result.Foreground_Colour :=
        Komnenos.Configuration.Get_Colour
          ("source_foreground", "default_foreground");
      Result.Border_Colour     :=
        Komnenos.Configuration.Get_Colour
          ("source_fragment_border", "default_border");

      Result.Set_Position (100, 100);
      Result.Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
      Result.Title := Ada.Strings.Unbounded.To_Unbounded_String (Title);
      return Result;
   end New_Source_Fragment;

end Komnenos.Fragments.Source;
