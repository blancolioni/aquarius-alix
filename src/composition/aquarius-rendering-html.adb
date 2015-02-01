with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aquarius.Colours;
with Aquarius.Fonts;

package body Aquarius.Rendering.Html is

   type Html_Renderer is new Root_Aquarius_Renderer
     with null record;

   overriding
   procedure Set_Text (Renderer  : access Html_Renderer;
                       Terminal  : Aquarius.Programs.Program_Tree;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String);

   overriding
   procedure Begin_Render (Renderer : access Html_Renderer);

   overriding
   procedure End_Render (Renderer : access Html_Renderer);

   function With_Font (Font : Aquarius.Fonts.Aquarius_Font;
                       Text : String) return String;

   ------------------
   -- Begin_Render --
   ------------------

   overriding
   procedure Begin_Render (Renderer : access Html_Renderer) is
      pragma Unreferenced (Renderer);
   begin
      Ada.Text_IO.Put_Line ("<font face=""courier"">");
   end Begin_Render;

   ----------------
   -- End_Render --
   ----------------

   overriding
   procedure End_Render (Renderer : access Html_Renderer) is
      pragma Unreferenced (Renderer);
   begin
      Ada.Text_IO.Put_Line ("</font>");
   end End_Render;

   -----------------------
   -- New_Html_Renderer --
   -----------------------

   function New_Html_Renderer return Aquarius_Renderer is
   begin
      return new Html_Renderer;
   end New_Html_Renderer;

   --------------
   -- Set_Text --
   --------------

   overriding
   procedure Set_Text (Renderer  : access Html_Renderer;
                       Terminal  : Aquarius.Programs.Program_Tree;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String)
   is
      pragma Unreferenced (Terminal);
      use Ada.Text_IO;
      use type Aquarius.Layout.Positive_Count;
      Font : constant Aquarius.Fonts.Aquarius_Font :=
        Renderer.Style.Font (Class);
      Render_Pos : constant Aquarius.Layout.Position :=
        Renderer.Current_Position;
   begin
      if Render_Pos.Line < Position.Line then
         for I in Render_Pos.Line .. Position.Line - 1 loop
            Put_Line ("<br>");
         end loop;
      elsif Render_Pos.Column < Position.Column then
         for I in Render_Pos.Line .. Position.Line - 1 loop
            Put ("&nbsp;");
         end loop;
      end if;

      Put (With_Font (Font, Text));
      Renderer.Set_Current_Position ((Position.Line,
                                      Position.Column +
                                        Aquarius.Layout.Count (Text'Length)));
   end Set_Text;

   ---------------
   -- With_Font --
   ---------------

   function With_Font (Font : Aquarius.Fonts.Aquarius_Font;
                       Text : String) return String
   is
      use Aquarius.Colours;
      use Aquarius.Fonts;

      function Html_Colour (Colour : Aquarius_Colour) return String;

      ------------------
      -- Html_Colour --
      ------------------

      function Html_Colour (Colour : Aquarius_Colour) return String is
      begin
         return "#" & Hex_Colour (Red (Colour)) &
           Hex_Colour (Green (Colour)) & Hex_Colour (Blue (Colour)) &
           """";
      end Html_Colour;

      use Ada.Strings.Unbounded;
      Result : Unbounded_String := To_Unbounded_String (Text);
   begin
      if Is_Bold (Font) then
         Result := "<b>" & Result & "</b>";
      end if;
      if Is_Italic (Font) then
         Result := "<i>" & Result & "</i>";
      end if;
      if Is_Underlined (Font) then
         Result := "<ul>" & Result & "</ul>";
      end if;
      Result := ">" & Result & "</font>";
      if Has_Foreground (Font) then
         Result := " color=""" & Html_Colour (Get_Foreground (Font)) &
           """" & Result;
      end if;
      if Has_Background (Font) then
         Result := " bgcolor=""" & Html_Colour (Get_Background (Font)) &
           """" & Result;
      end if;
      return To_String ("<font " & Result);
   end With_Font;

end Aquarius.Rendering.Html;
