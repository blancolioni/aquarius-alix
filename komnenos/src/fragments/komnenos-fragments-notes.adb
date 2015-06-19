package body Komnenos.Fragments.Notes is

   -----------------------
   -- New_Note_Fragment --
   -----------------------

   function New_Note_Fragment
     (Initial_Text : String := "")
      return Fragment_Type
   is
      Result : constant Fragment_Type := new Root_Fragment_Type;
   begin
      Result.Editable := True;
      Result.Background_Colour := new String'("seashell");
      Result.Foreground_Colour := new String'("black");
      Result.Border_Colour     := new String'("lightblue");
      if Initial_Text /= "" then
         Result.Put_Line (Initial_Text,
                          Aquarius.Themes.Active_Theme.Default_Style);
      end if;
      Result.Set_Position (100, 100);
      return Result;
   end New_Note_Fragment;

end Komnenos.Fragments.Notes;
