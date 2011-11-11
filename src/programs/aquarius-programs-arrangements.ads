with Aquarius.Rendering;
with Aquarius.Trees.Cursors;

package Aquarius.Programs.Arrangements is

   procedure Arrange (Item        : in Program_Tree;
                      Line_Length : in Positive      := 72);

   procedure Arrange (Item           : in Program_Tree;
                      Point          : in Aquarius.Trees.Cursors.Cursor;
                      Partial_Length : in Natural;
                      Line_Length    : in Positive      := 72);

   procedure Render
     (Program     : in Program_Tree;
      Renderer    : in Aquarius.Rendering.Aquarius_Renderer;
      Point       : in Aquarius.Trees.Cursors.Cursor;
      Partial     : in String);

end Aquarius.Programs.Arrangements;
