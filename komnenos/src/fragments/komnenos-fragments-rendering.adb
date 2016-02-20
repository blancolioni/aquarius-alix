with Aquarius.Layout;
with Aquarius.Programs;

package body Komnenos.Fragments.Rendering is

   use Aquarius.Rendering;

   type Root_Fragment_Renderer is new Root_Aquarius_Renderer with
      record
         Fragment      : Fragment_Type;
         Entity_Table  : access Komnenos.Entities.Entity_Table_Interface'Class;
      end record;

   overriding
   procedure Set_Text (Renderer  : in out Root_Fragment_Renderer;
                       Terminal  : in     Aquarius.Programs.Program_Tree;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String);

   overriding
   procedure Begin_Render (Renderer : in out Root_Fragment_Renderer);

   overriding
   procedure End_Render (Renderer : in out Root_Fragment_Renderer);

   overriding
   procedure Set_Point (Renderer : in out Root_Fragment_Renderer;
                        Point    : in     Aquarius.Layout.Position);

   ------------------
   -- Begin_Render --
   ------------------

   overriding
   procedure Begin_Render (Renderer : in out Root_Fragment_Renderer) is
   begin
      Renderer.Fragment.Clear;
      Renderer.Set_Current_Position ((1, 1));
   end Begin_Render;

   ----------------
   -- End_Render --
   ----------------

   overriding
   procedure End_Render (Renderer : in out Root_Fragment_Renderer) is
   begin
      null;
--        Renderer.Fragment.Display.Update;
--        Renderer.Fragment.Display.Set_Point (Renderer.Pos);
   end End_Render;

   --------------------------
   -- New_Fragment_Renderer --
   --------------------------

   function Fragment_Renderer
     (Target : Fragment_Type;
      Entity_Table : access Komnenos.Entities.Entity_Table_Interface'Class)
      return Aquarius_Renderer
   is
   begin
      return Result : Root_Fragment_Renderer do
         Result.Fragment := Target;
         Result.Entity_Table := Entity_Table;
      end return;
   end Fragment_Renderer;

   ---------------
   -- Set_Point --
   ---------------

   overriding
   procedure Set_Point (Renderer : in out Root_Fragment_Renderer;
                        Point    : in     Aquarius.Layout.Position)
   is
   begin
      Renderer.Set_Current_Position (Point);
   end Set_Point;

   --------------
   -- Set_Text --
   --------------

   overriding
   procedure Set_Text (Renderer  : in out Root_Fragment_Renderer;
                       Terminal  : in     Aquarius.Programs.Program_Tree;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String)
   is
      use Aquarius.Styles;
      Style : constant Aquarius_Style :=
                Aquarius.Themes.Active_Theme.Style
                  (Class, Aquarius.Themes.Normal);
      use type Aquarius.Layout.Positive_Count;
      Render_Pos : Aquarius.Layout.Position :=
                     Renderer.Current_Position;
      Reference  : Komnenos.Entities.Entity_Reference := null;
   begin
      while Position.Column < Render_Pos.Column
        or else Render_Pos.Line < Position.Line
      loop
         Renderer.Fragment.New_Line;
         Render_Pos.Line := Render_Pos.Line + 1;
         Render_Pos.Column := 1;
      end loop;

      declare
         Space_Count : constant Natural :=
                         Natural (Position.Column - Render_Pos.Column);
         Spaces      : constant String (1 .. Space_Count) :=
                         (others => ' ');
      begin
         if Space_Count > 0 then
            Renderer.Fragment.Put (Spaces, Style);
         end if;
      end;

      if Renderer.Entity_Table /= null
        and then Terminal.Location_Line > 0
        and then Terminal.Location_Column > 0
      then
         declare
            References : constant Komnenos.Entities.Array_Of_Entities :=
                           Renderer.Entity_Table.Cross_References
                             (Renderer.Fragment.File_Name,
                              Terminal.Location_Line,
                              Terminal.Location_Column);
         begin
            if References'Length > 0 then
               Reference := References (References'First);
--                 Styles (Hover) := Find_Style ("entity_reference_hover");
--                 Ada.Text_IO.Put_Line
--                   (Terminal.Show_Location & " references "
--                      & Reference.Display_Text);
            end if;
         end;
      end if;

      Renderer.Fragment.Put (Text, Style, Reference);
      Renderer.Set_Current_Position ((Position.Line,
                                      Position.Column +
                                        Aquarius.Layout.Count (Text'Length)));
   end Set_Text;

end Komnenos.Fragments.Rendering;
