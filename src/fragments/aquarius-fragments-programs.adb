with Aquarius.Programs.Arrangements;
with Aquarius.Trees.Cursors;

package body Aquarius.Fragments.Programs is

   --------------------
   -- Create_Program --
   --------------------

   function Create_Program
     (Width, Height : Positive;
      Program       : Aquarius.Programs.Program_Tree)
      return Aquarius_Fragment
   is
      use Aquarius.Fragments.Text;
      Result : constant Aquarius_Fragment :=
                 new Program_Fragment'(Text_Fragment with
                                       Program => Program);
   begin
      Result.Initialise (Width, Height);
      return Result;
   end Create_Program;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Fragment : in out Program_Fragment)
   is
      Cursor : constant Aquarius.Trees.Cursors.Cursor :=
                 Aquarius.Trees.Cursors.Left_Of_Tree
                   (Fragment.Program);
   begin
      Aquarius.Programs.Arrangements.Arrange (Fragment.Program,
                                              Cursor,
                                              0);
      Aquarius.Programs.Arrangements.Render
        (Fragment.Program, Fragment.Renderer, Cursor, "");
   end Render;

end Aquarius.Fragments.Programs;
