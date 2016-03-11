with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Aquarius.Rendering.Files is

   type Root_File_Renderer is
     new Root_Aquarius_Renderer with
      record
         Path : Ada.Strings.Unbounded.Unbounded_String;
         File : access Ada.Text_IO.File_Type;
      end record;

   overriding procedure Set_Text
     (Renderer  : in out Root_File_Renderer;
      Terminal  : Aquarius.Programs.Program_Tree;
      Position  : in     Aquarius.Layout.Position;
      Class     : in     String;
      Text      : in     String);

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
         New_Line (Renderer.File.all,
                   Positive_Count (Position.Line - Render_Pos.Line));
      end if;
      Set_Col (Renderer.File.all, Positive_Count (Position.Column));
      Put (Renderer.File.all, Text);
      Renderer.Set_Current_Position
        ((Position.Line,
         Position.Column + Aquarius.Layout.Count (Text'Length)));
   end Set_Text;

end Aquarius.Rendering.Files;