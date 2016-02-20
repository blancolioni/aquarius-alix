package body Aquarius.Rendering is

   ----------------------
   -- Current_Position --
   ----------------------

   function Current_Position
     (Renderer : Root_Aquarius_Renderer'Class)
     return Aquarius.Layout.Position
   is
   begin
      return Renderer.Pos;
   end Current_Position;

   --------------------------
   -- Set_Current_Position --
   --------------------------

   procedure Set_Current_Position
     (Renderer : in out Root_Aquarius_Renderer'Class;
      Position : in     Aquarius.Layout.Position)
   is
   begin
      Renderer.Pos := Position;
   end Set_Current_Position;

   ---------------
   -- Set_Theme --
   ---------------

   procedure Set_Theme (Renderer : in out Root_Aquarius_Renderer'Class;
                        Theme    : in     Aquarius.Themes.Aquarius_Theme)
   is
   begin
      Renderer.Theme := Theme;
   end Set_Theme;

end Aquarius.Rendering;
