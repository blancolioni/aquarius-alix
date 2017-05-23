with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Komnenos.Colours.Html;
with Komnenos.Fonts;

package body Aquarius.Rendering.Html is

   type Root_Html_Renderer is new Root_Aquarius_Renderer
   with null record;

   overriding procedure Set_Text
     (Renderer  : in out Root_Html_Renderer;
      Terminal  : Aquarius.Programs.Program_Tree;
      Line      : Aquarius.Layout.Line_Number;
      Column    : Aquarius.Layout.Column_Number;
      Class     : String;
      Text      : String);

   overriding
   procedure Begin_Render (Renderer : in out Root_Html_Renderer);

   overriding
   procedure End_Render (Renderer : in out Root_Html_Renderer);

   function With_Font (Font : Komnenos.Fonts.Komnenos_Font;
                       Text : String) return String;

   ------------------
   -- Begin_Render --
   ------------------

   overriding
   procedure Begin_Render (Renderer : in out Root_Html_Renderer) is
      pragma Unreferenced (Renderer);
   begin
      Ada.Text_IO.Put_Line ("<font face=""courier"">");
   end Begin_Render;

   ----------------
   -- End_Render --
   ----------------

   overriding
   procedure End_Render (Renderer : in out Root_Html_Renderer) is
      pragma Unreferenced (Renderer);
   begin
      Ada.Text_IO.Put_Line ("</font>");
   end End_Render;

   -----------------------
   -- New_Html_Renderer --
   -----------------------

   function Html_Renderer return Aquarius_Renderer is
   begin
      return Result : Root_Html_Renderer do
         null;
      end return;
   end Html_Renderer;

   --------------
   -- Set_Text --
   --------------

   overriding
   procedure Set_Text
     (Renderer  : in out Root_Html_Renderer;
      Terminal  : Aquarius.Programs.Program_Tree;
      Line      : Aquarius.Layout.Line_Number;
      Column    : Aquarius.Layout.Column_Number;
      Class     : String;
      Text      : String)
   is
      pragma Unreferenced (Terminal);
      use Ada.Text_IO;
      use Aquarius.Layout;
      Font : constant Komnenos.Fonts.Komnenos_Font :=
        Renderer.Theme.Style (Class).Font;
   begin
      if Renderer.Line < Line then
         for I in Renderer.Line .. Line - 1 loop
            Put_Line ("<br>");
         end loop;
         Renderer.Set_Current_Position (Line, 1);
      end if;

      if Renderer.Column < Column then
         for I in Renderer.Column .. Column - 1 loop
            Put ("&nbsp;");
         end loop;
      end if;

      Put (With_Font (Font, Text));

      Renderer.Set_Current_Position
        (Line, Column + Column_Offset (Text'Length));
   end Set_Text;

   ---------------
   -- With_Font --
   ---------------

   function With_Font (Font : Komnenos.Fonts.Komnenos_Font;
                       Text : String) return String
   is
      use Komnenos.Colours;
      use Komnenos.Fonts;

      use Ada.Strings.Unbounded;
      Result : Unbounded_String := To_Unbounded_String (Text);

      function To_Html_Colour
        (Colour : Komnenos_Colour)
         return String
         renames Komnenos.Colours.Html.To_Html_Colour;

   begin
      if Font.Is_Bold then
         Result := "<b>" & Result & "</b>";
      end if;
      if Font.Is_Italic then
         Result := "<i>" & Result & "</i>";
      end if;
      if Font.Is_Underlined then
         Result := "<ul>" & Result & "</ul>";
      end if;
      Result := ">" & Result & "</font>";
      if Font.Has_Foreground_Color then
         Result := " color="""
           & To_Html_Colour (Font.Foreground_Color)
           & """" & Result;
      end if;
      if Font.Has_Background_Color then
         Result :=
           " bgcolor="""
           & To_Html_Colour (Font.Background_Color)
           & """" & Result;
      end if;
      return To_String ("<font " & Result);
   end With_Font;

end Aquarius.Rendering.Html;
