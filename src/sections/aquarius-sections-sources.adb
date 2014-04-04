with Aquarius.Grammars.Manager;
with Aquarius.Loader;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering.Sections;
with Aquarius.Sections.Code;
with Aquarius.Trees.Cursors;

package body Aquarius.Sections.Sources is

   --------------------
   -- Append_Section --
   --------------------

   procedure Append_Section (Source  : in out Root_Section_Source;
                             Section : Aquarius_Section)
   is
   begin
      Source.List.Append (Section);
      Source.Map.Insert (Section.Id, Section);
   end Append_Section;

   -----------------
   -- File_Source --
   -----------------

   function File_Source
     (Path : String)
      return Aquarius_Section_Source
   is
      Grammar   : constant Aquarius.Grammars.Aquarius_Grammar :=
                    Aquarius.Grammars.Manager.Get_Grammar_For_File
                      (Path);
      Code      : constant Aquarius_Section :=
                    Aquarius.Sections.Code.New_Code_Section
                      (Path);
      Result    : constant Aquarius_Section_Source :=
                    new Root_Section_Source;
      Program   : constant Aquarius.Programs.Program_Tree :=
                    Aquarius.Loader.Load_From_File
                      (Grammar => Grammar,
                       Path    => Path);
      Renderer  : constant Aquarius.Rendering.Aquarius_Renderer :=
                    Aquarius.Rendering.Sections.New_Section_Renderer
                      (Code);
   begin
      Aquarius.Programs.Arrangements.Arrange
        (Item        => Program,
         Line_Length => 40);
      Aquarius.Programs.Arrangements.Render
        (Program  => Program,
         Renderer => Renderer,
         Point    => Aquarius.Trees.Cursors.Left_Of_Tree (Program),
         Partial  => "");
      Result.Append_Section (Code);

      return Result;
   end File_Source;

   -----------------
   -- Get_Section --
   -----------------

   function Get_Section
     (Source : in out Root_Section_Source;
      Id     : String)
      return Aquarius_Section
   is
      use Ada.Strings.Unbounded;
      Key : constant Unbounded_String := To_Unbounded_String (Id);
   begin
      if Source.Map.Contains (Key) then
         return Source.Map.Element (Key);
      else
         declare
            Result : constant Aquarius_Section :=
                       Root_Section_Source'Class (Source).Load_Section (Id);
         begin
            Source.Map.Insert (Key, Result);
            return Result;
         end;
      end if;
   end Get_Section;

end Aquarius.Sections.Sources;
