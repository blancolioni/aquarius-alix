package body Aquarius.Rendering is

   --------------------
   -- Current_Column --
   --------------------

   function Current_Column
     (Renderer : Root_Aquarius_Renderer'Class)
      return Aquarius.Layout.Column_Number
   is
   begin
      return Renderer.Column;
   end Current_Column;

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line
     (Renderer : Root_Aquarius_Renderer'Class)
      return Aquarius.Layout.Line_Number
   is
   begin
      return Renderer.Line;
   end Current_Line;

   --------------------------
   -- Set_Current_Position --
   --------------------------

   procedure Set_Current_Position
     (Renderer : in out Root_Aquarius_Renderer'Class;
      Line     : Aquarius.Layout.Line_Number;
      Column   : Aquarius.Layout.Column_Number)
   is
   begin
      Renderer.Line := Line;
      Renderer.Column := Column;
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
