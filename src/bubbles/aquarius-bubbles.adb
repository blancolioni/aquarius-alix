package body Aquarius.Bubbles is

   ----------------
   -- Background --
   ----------------

   function Background
     (Bubble : not null access Root_Aquarius_Bubble'Class)
     return Aquarius.Fonts.Aquarius_Colour
   is
   begin
      return Bubble.Current_Background;
   end Background;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
     (Bubble    : not null access Root_Aquarius_Bubble'Class)
     return Position
   is
   begin
      return Bubble.Current_Position;
   end Get_Position;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Bubble        : not null access Root_Aquarius_Bubble'Class;
      New_Background : in Aquarius.Fonts.Aquarius_Colour)
   is
   begin
      Bubble.Current_Background := New_Background;
   end Set_Background;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Bubble       : not null access Root_Aquarius_Bubble'Class;
      New_Position : in Position)
   is
   begin
      Bubble.Current_Position := New_Position;
   end Set_Position;

   ------------------
   -- Set_Renderer --
   ------------------

   procedure Set_Renderer
     (Bubble       : not null access Root_Aquarius_Bubble'Class;
      New_Renderer : not null access
      Aquarius.Rendering.Root_Aquarius_Renderer'Class)
   is
   begin
      Bubble.Current_Renderer :=
        Aquarius.Rendering.Aquarius_Renderer (New_Renderer);
   end Set_Renderer;

end Aquarius.Bubbles;
