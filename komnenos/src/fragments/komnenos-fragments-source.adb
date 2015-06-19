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
      Result.Background_Colour := new String'("seashell");
      Result.Foreground_Colour := new String'("black");
      Result.Border_Colour     := new String'("#8C001A");
      Result.Set_Position (100, 100);
      Result.Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
      Result.Title := Ada.Strings.Unbounded.To_Unbounded_String (Title);
      return Result;
   end New_Source_Fragment;

end Komnenos.Fragments.Source;
