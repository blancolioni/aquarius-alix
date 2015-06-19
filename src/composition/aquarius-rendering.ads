with Aquarius.Layout;
with Aquarius.Programs;
with Aquarius.Themes;

package Aquarius.Rendering is

   type Root_Aquarius_Renderer is abstract tagged private;

   function Current_Position
     (Renderer : access Root_Aquarius_Renderer'Class)
     return Aquarius.Layout.Position;

   procedure Set_Current_Position
     (Renderer : access Root_Aquarius_Renderer'Class;
      Position : in     Aquarius.Layout.Position);

   procedure Set_Text (Renderer  : access Root_Aquarius_Renderer;
                       Terminal  : in     Aquarius.Programs.Program_Tree;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String)
      is abstract;

   procedure Set_Theme (Renderer : access Root_Aquarius_Renderer;
                        Theme    : in     Aquarius.Themes.Aquarius_Theme);

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
         Theme   : Aquarius.Themes.Aquarius_Theme    := null;
      end record;

end Aquarius.Rendering;
