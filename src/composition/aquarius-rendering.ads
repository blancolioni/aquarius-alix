with Aquarius.Layout;
with Aquarius.Styles;

package Aquarius.Rendering is

   type Root_Aquarius_Renderer is abstract tagged private;

   function Current_Position
     (Renderer : access Root_Aquarius_Renderer'Class)
     return Aquarius.Layout.Position;

   procedure Set_Current_Position
     (Renderer : access Root_Aquarius_Renderer'Class;
      Position : in     Aquarius.Layout.Position);

   procedure Set_Text (Renderer  : access Root_Aquarius_Renderer;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String)
      is abstract;

   procedure Set_Style (Renderer : access Root_Aquarius_Renderer;
                        Style    : in     Aquarius.Styles.Aquarius_Style);

   procedure Begin_Render (Renderer : access Root_Aquarius_Renderer)
     is null;
   procedure End_Render (Renderer : access Root_Aquarius_Renderer)
     is null;

   procedure Set_Point (Renderer : access Root_Aquarius_Renderer;
                        Point    : in     Aquarius.Layout.Position)
   is null;

   type Aquarius_Renderer is access all Root_Aquarius_Renderer'Class;

private

   type Root_Aquarius_Renderer is abstract tagged
      record
         Pos     : Aquarius.Layout.Position          := (1, 1);
         Style   : Aquarius.Styles.Aquarius_Style    := null;
      end record;

end Aquarius.Rendering;
