with Aquarius.Layout;
with Aquarius.Programs;
with Aquarius.Themes;

package Aquarius.Rendering is

   type Root_Aquarius_Renderer is abstract tagged private;

   function Current_Position
     (Renderer : Root_Aquarius_Renderer'Class)
     return Aquarius.Layout.Position;

   procedure Set_Current_Position
     (Renderer : in out Root_Aquarius_Renderer'Class;
      Position : Aquarius.Layout.Position);

   procedure Set_Text (Renderer  : in out Root_Aquarius_Renderer;
                       Terminal  : Aquarius.Programs.Program_Tree;
                       Position  : Aquarius.Layout.Position;
                       Class     : String;
                       Text      : String)
      is abstract;

   procedure Set_Theme (Renderer : in out Root_Aquarius_Renderer'Class;
                        Theme    : Aquarius.Themes.Aquarius_Theme);

   procedure Begin_Render (Renderer : in out Root_Aquarius_Renderer)
     is null;
   procedure End_Render (Renderer : in out Root_Aquarius_Renderer)
     is null;

   procedure Set_Point (Renderer : in out Root_Aquarius_Renderer;
                        Point    : Aquarius.Layout.Position)
   is null;

   subtype Aquarius_Renderer is Root_Aquarius_Renderer'Class;

private

   type Root_Aquarius_Renderer is abstract tagged
      record
         Pos     : Aquarius.Layout.Position          := (1, 1);
         Theme   : Aquarius.Themes.Aquarius_Theme    := null;
      end record;

end Aquarius.Rendering;
