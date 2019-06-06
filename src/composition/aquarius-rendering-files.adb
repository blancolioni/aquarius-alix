with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Aquarius.Rendering.Files is

   type File_Access is access Ada.Text_IO.File_Type;

   type Root_File_Renderer is
     new Root_Aquarius_Renderer with
      record
         Path : Ada.Strings.Unbounded.Unbounded_String;
         File : File_Access;
      end record;

   overriding procedure Set_Text
     (Renderer  : in out Root_File_Renderer;
      Terminal    : Aquarius.Programs.Program_Tree;
      Line        : in     Aquarius.Layout.Line_Number;
      Column      : in     Aquarius.Layout.Column_Number;
      Class       : in     String;
      Text        : in     String);

   overriding procedure Begin_Render
     (Renderer : in out Root_File_Renderer);

   overriding procedure End_Render
     (Renderer : in out Root_File_Renderer);

   ------------------
   -- Begin_Render --
   ------------------

   overriding procedure Begin_Render
     (Renderer : in out Root_File_Renderer)
   is
   begin
      Renderer.File := new Ada.Text_IO.File_Type;
      Ada.Text_IO.Create (Renderer.File.all, Ada.Text_IO.Out_File,
                          Ada.Strings.Unbounded.To_String (Renderer.Path));
   end Begin_Render;

   ----------------
   -- End_Render --
   ----------------

   overriding procedure End_Render
     (Renderer : in out Root_File_Renderer)
   is
   begin
      Ada.Text_IO.Close (Renderer.File.all);
   end End_Render;

   -------------------
   -- File_Renderer --
   -------------------

   function File_Renderer
     (Path : String)
      return Aquarius_Renderer
   is
   begin
      return Result : Root_File_Renderer do
         Result.Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
      end return;
   end File_Renderer;

   --------------
   -- Set_Text --
   --------------

   overriding procedure Set_Text
     (Renderer  : in out Root_File_Renderer;
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
         New_Line (Renderer.File.all,
                   Ada.Text_IO.Positive_Count (Line - Renderer.Line));
         Renderer.Set_Current_Position (Line, 1);
      end if;

      Set_Col (Renderer.File.all, Ada.Text_IO.Positive_Count (Column));

      Put (Renderer.File.all, Text);

   end Set_Text;

end Aquarius.Rendering.Files;
