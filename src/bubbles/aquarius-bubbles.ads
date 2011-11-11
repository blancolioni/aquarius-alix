with Aquarius.Fonts;
with Aquarius.Rendering;

package Aquarius.Bubbles is

   Max_X : constant := 10_000;
   Max_Y : constant := 800;

   type Root_Aquarius_Bubble is abstract tagged private;

   type Position is
      record
         X, Y     : Natural;
         Width    : Positive;
         Height   : Positive;
      end record;

   procedure Set_Position
     (Bubble       : not null access Root_Aquarius_Bubble'Class;
      New_Position : in Position);

   function Get_Position
     (Bubble    : not null access Root_Aquarius_Bubble'Class)
     return Position;

   procedure Set_Background
     (Bubble         : not null access Root_Aquarius_Bubble'Class;
      New_Background : in Aquarius.Fonts.Aquarius_Colour);

   procedure Set_Renderer
     (Bubble       : not null access Root_Aquarius_Bubble'Class;
      New_Renderer : not null access
      Aquarius.Rendering.Root_Aquarius_Renderer'Class);

   function Background
     (Bubble : not null access Root_Aquarius_Bubble'Class)
     return Aquarius.Fonts.Aquarius_Colour;

   procedure Render (Bubble : not null access Root_Aquarius_Bubble)
      is abstract;

   type Aquarius_Bubble is access all Root_Aquarius_Bubble'Class;

private

   type Root_Aquarius_Bubble is abstract tagged
      record
         Current_Position   : Position;
         Current_Renderer   : Aquarius.Rendering.Aquarius_Renderer;
         Current_Background : Aquarius.Fonts.Aquarius_Colour;
      end record;

end Aquarius.Bubbles;
