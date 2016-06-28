with Aquarius.Layout;
with Aquarius.Programs;
with Aquarius.Themes;

package Aquarius.Rendering is

   type Root_Aquarius_Renderer is abstract tagged private;

   function Current_Line
     (Renderer : Root_Aquarius_Renderer'Class)
     return Aquarius.Layout.Line_Number;

   function Current_Column
     (Renderer : Root_Aquarius_Renderer'Class)
      return Aquarius.Layout.Column_Number;

   procedure Set_Current_Position
     (Renderer : in out Root_Aquarius_Renderer'Class;
      Line     : Aquarius.Layout.Line_Number;
      Column   : Aquarius.Layout.Column_Number);

   procedure Set_Text (Renderer  : in out Root_Aquarius_Renderer;
                       Terminal  : Aquarius.Programs.Program_Tree;
                       Line      : Aquarius.Layout.Line_Number;
                       Column    : Aquarius.Layout.Column_Number;
                       Class     : String;
                       Text      : String)
      is abstract;

   procedure Set_Theme (Renderer : in out Root_Aquarius_Renderer'Class;
                        Theme    : Aquarius.Themes.Aquarius_Theme);

   procedure Begin_Render (Renderer : in out Root_Aquarius_Renderer)
     is null;
   procedure End_Render (Renderer : in out Root_Aquarius_Renderer)
     is null;

   procedure Set_Point
     (Renderer : in out Root_Aquarius_Renderer;
      Line     : Aquarius.Layout.Line_Number;
      Column   : Aquarius.Layout.Column_Number)
   is null;

   subtype Aquarius_Renderer is Root_Aquarius_Renderer'Class;

private

   type Root_Aquarius_Renderer is abstract tagged
      record
         Line    : Aquarius.Layout.Line_Number := 1;
         Column  : Aquarius.Layout.Column_Number := 1;
         Theme   : Aquarius.Themes.Aquarius_Theme    := null;
      end record;

end Aquarius.Rendering;
