package body Aquarius.Rendering is

   ----------------------
   -- Current_Position --
   ----------------------

   function Current_Position
     (Renderer : access Root_Aquarius_Renderer'Class)
     return Aquarius.Layout.Position
   is
   begin
      return Renderer.Pos;
   end Current_Position;

   --------------------------
   -- Set_Current_Position --
   --------------------------

   procedure Set_Current_Position
     (Renderer : access Root_Aquarius_Renderer'Class;
      Position : in     Aquarius.Layout.Position)
   is
   begin
      Renderer.Pos := Position;
   end Set_Current_Position;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style (Renderer : access Root_Aquarius_Renderer;
                        Style    : in     Aquarius.Styles.Aquarius_Style)
   is
   begin
      Renderer.Style := Style;
   end Set_Style;

end Aquarius.Rendering;
