package Aquarius.Sections.Sources is

   type Root_Section_Source is tagged private;

   function Get_Section (Source : in out Root_Section_Source;
                         Id     : String)
                         return Aquarius_Section;

   function Load_Section (Source : in out Root_Section_Source;
                          Id     : String)
                          return Aquarius_Section
                          is (null);

   procedure Append_Section (Source  : in out Root_Section_Source;
                             Section : Aquarius_Section);

   procedure On_Change (Source  : Root_Section_Source;
                        Section : Aquarius_Section)
   is null;

   type Aquarius_Section_Source is
     access all Root_Section_Source'Class;

   function File_Source
     (Path : String)
      return Aquarius_Section_Source;

private

   type Root_Section_Source is tagged
      record
         Map  : Section_Maps.Map;
         List : Section_Lists.List;
      end record;

end Aquarius.Sections.Sources;
