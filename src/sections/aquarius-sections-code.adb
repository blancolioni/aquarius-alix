package body Aquarius.Sections.Code is

   ----------------------
   -- New_Code_Section --
   ----------------------

   function New_Code_Section (Id : String) return Aquarius_Section is
      Result : constant Aquarius_Section :=
                 new Root_Aquarius_Section;
   begin
      Result.Create (Id);
      Result.Background :=
        Aquarius.Colours.White;
      return Result;
   end New_Code_Section;

end Aquarius.Sections.Code;
