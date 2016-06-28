with Ada.Text_IO;

package body Aquarius.Rendering.Text is

   type Root_Text_Renderer is new Root_Aquarius_Renderer
     with null record;

   overriding procedure Set_Text
     (Renderer    : in out Root_Text_Renderer;
      Terminal    : Aquarius.Programs.Program_Tree;
      Line        : in     Aquarius.Layout.Line_Number;
      Column      : in     Aquarius.Layout.Column_Number;
      Class       : in     String;
      Text        : in     String);

   --------------
   -- Set_Text --
   --------------

   overriding procedure Set_Text
     (Renderer    : in out Root_Text_Renderer;
      Terminal    : Aquarius.Programs.Program_Tree;
      Line        : in     Aquarius.Layout.Line_Number;
      Column      : in     Aquarius.Layout.Column_Number;
      Class       : in     String;
      Text        : in     String)
   is
      pragma Unreferenced (Terminal);
      pragma Unreferenced (Class);
      use Ada.Text_IO;
      use Aquarius.Layout;
   begin
      if Renderer.Line < Line then
         New_Line (Ada.Text_IO.Positive_Count (Line - Renderer.Line));
         Renderer.Set_Current_Position (Line, 1);
      end if;

      Set_Col (Ada.Text_IO.Positive_Count (Column));

      Put (Text);

      Renderer.Set_Current_Position
        (Line, Column + Column_Offset (Text'Length));
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
