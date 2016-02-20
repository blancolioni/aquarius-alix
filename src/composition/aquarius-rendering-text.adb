with Ada.Text_IO;

package body Aquarius.Rendering.Text is

   type Root_Text_Renderer is new Root_Aquarius_Renderer
     with null record;

   overriding
   procedure Set_Text (Renderer  : in out Root_Text_Renderer;
                       Terminal  : Aquarius.Programs.Program_Tree;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String);

   --------------
   -- Set_Text --
   --------------

   overriding
   procedure Set_Text (Renderer  : in out Root_Text_Renderer;
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

   -------------------
   -- Text_Renderer --
   -------------------

   function Text_Renderer return Aquarius_Renderer is
   begin
      return Result : Root_Text_Renderer do
         null;
      end return;
   end Text_Renderer;

end Aquarius.Rendering.Text;
