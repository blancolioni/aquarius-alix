with Aquarius.Rendering;
with Aquarius.Trees.Cursors;

package Aquarius.Programs.Arrangements is

   procedure Arrange (Item        : in Program_Tree;
                      Line_Length : in Positive      := 72);

   procedure Arrange (Item             : in Program_Tree;
                      Point            : in Aquarius.Trees.Cursors.Cursor;
                      Partial_Length   : in Natural;
                      New_Line_Partial : in Boolean;
                      Partial_Start    : out Aquarius.Layout.Position;
                      Line_Length      : in Positive      := 72);

   procedure Render
     (Program          : in Program_Tree;
      Renderer         : in out Rendering.Root_Aquarius_Renderer'Class;
      Point            : in Aquarius.Trees.Cursors.Cursor;
      Partial          : in String;
      Partial_Start    : in Aquarius.Layout.Position);
   --  Render Program using the given Renderer, and the Partial
   --  edit at Point, at the position given by Partial_Start

   procedure Render
     (Program     : in Program_Tree;
      Renderer    : in out Aquarius.Rendering.Root_Aquarius_Renderer'Class);

end Aquarius.Programs.Arrangements;
