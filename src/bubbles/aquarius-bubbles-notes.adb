with Ada.Strings.Unbounded;

with Aquarius.Fonts;

package body Aquarius.Bubbles.Notes is

   type Note_Bubble is new Root_Aquarius_Bubble with
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding
   procedure Render (Bubble : not null access Note_Bubble);

   -----------------
   -- Create_Note --
   -----------------

   function Create_Note return Aquarius_Bubble is
      use Ada.Strings.Unbounded;
      Result : constant Note_Bubble :=
        (Current_Renderer   => null,
         Current_Position   => (0, 0, 100, 200),
         Current_Background =>
           Aquarius.Fonts.Parse_Colour ("yellow"),
         Text => Null_Unbounded_String);
   begin
      return new Note_Bubble'(Result);
   end Create_Note;

   ------------
   -- Render --
   ------------

   overriding
   procedure Render (Bubble : not null access Note_Bubble) is
   begin
      Bubble.Current_Renderer.Begin_Render;
      Bubble.Current_Renderer.Set_Text
        ((1, 1), "note", Ada.Strings.Unbounded.To_String (Bubble.Text));
      Bubble.Current_Renderer.End_Render;
   end Render;

end Aquarius.Bubbles.Notes;
