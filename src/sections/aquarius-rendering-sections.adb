package body Aquarius.Rendering.Sections is

   type Section_Renderer is new Root_Aquarius_Renderer with
      record
         Section      : Aquarius.Sections.Aquarius_Section;
      end record;

   overriding
   procedure Set_Text (Renderer  : access Section_Renderer;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String);

   overriding
   procedure Begin_Render (Renderer : access Section_Renderer);

   overriding
   procedure End_Render (Renderer : access Section_Renderer);

   overriding
   procedure Set_Point (Renderer : access Section_Renderer;
                        Point    : in     Aquarius.Layout.Position);

   ------------------
   -- Begin_Render --
   ------------------

   overriding
   procedure Begin_Render (Renderer : access Section_Renderer) is
   begin
      Renderer.Section.Clear;
      Renderer.Pos := (1, 1);
   end Begin_Render;

   ----------------
   -- End_Render --
   ----------------

   overriding
   procedure End_Render (Renderer : access Section_Renderer) is
   begin
      Renderer.Section.Display.Update;
      Renderer.Section.Display.Set_Point (Renderer.Pos);
   end End_Render;

   --------------------------
   -- New_Section_Renderer --
   --------------------------

   function New_Section_Renderer
     (Target : Aquarius.Sections.Aquarius_Section)
      return Aquarius_Renderer
   is
      Result : Section_Renderer;
   begin
      Result.Section := Target;
      return new Section_Renderer'(Result);
   end New_Section_Renderer;

   ---------------
   -- Set_Point --
   ---------------

   overriding
   procedure Set_Point (Renderer : access Section_Renderer;
                        Point    : in     Aquarius.Layout.Position)
   is
   begin
      Renderer.Pos := Point;
   end Set_Point;

   --------------
   -- Set_Text --
   --------------

   overriding
   procedure Set_Text (Renderer  : access Section_Renderer;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String)
   is
   begin
      Renderer.Section.Put (Position, Text, Class);
   end Set_Text;

end Aquarius.Rendering.Sections;
