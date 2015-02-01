with Ada.Text_IO;

package body Aquarius.Rendering.Text is

   type Text_Renderer is new Root_Aquarius_Renderer
     with null record;

   overriding
   procedure Set_Text (Renderer  : access Text_Renderer;
                       Terminal  : Aquarius.Programs.Program_Tree;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String);

   -----------------------
   -- New_Text_Renderer --
   -----------------------

   function New_Text_Renderer return Aquarius_Renderer is
   begin
      return new Text_Renderer;
   end New_Text_Renderer;

   --------------
   -- Set_Text --
   --------------

   overriding
   procedure Set_Text (Renderer  : access Text_Renderer;
                       Terminal  : Aquarius.Programs.Program_Tree;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String)
   is
      pragma Unreferenced (Terminal);
      pragma Unreferenced (Class);
      use Ada.Text_IO;
      use type Aquarius.Layout.Positive_Count;
      Render_Pos : constant Aquarius.Layout.Position :=
        Renderer.Current_Position;
   begin
      if Render_Pos.Line < Position.Line then
         New_Line (Positive_Count (Position.Line - Render_Pos.Line));
      end if;
      Set_Col (Positive_Count (Position.Column));
      Put (Text);
      Renderer.Set_Current_Position ((Position.Line,
                                      Position.Column +
                                        Aquarius.Layout.Count (Text'Length)));
   end Set_Text;

end Aquarius.Rendering.Text;
