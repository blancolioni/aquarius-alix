with Glib;                              use Glib;
with Gtk.Text_Iter;                     use Gtk.Text_Iter;
--  with Gtk.Text_Mark;                     use Gtk.Text_Mark;
with Gtk.Text_Tag;                      use Gtk.Text_Tag;

with Pango.Font;

with Aquarius.GUI.Text;

package body Aquarius.Rendering.GUI is

   use Gtk.Text_Buffer;

   type GUI_Renderer is new Root_Aquarius_Renderer with
      record
         Text_Buffer  : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Default_Tag  : Gtk.Text_Tag.Gtk_Text_Tag;
      end record;

   procedure Add_Text (Renderer : access GUI_Renderer'Class;
                       Position : in out Aquarius.Layout.Position;
                       Class    : in     String;
                       Text     : in     String);

   procedure Set_Line (Renderer : access GUI_Renderer'Class;
                       Line     : in     Aquarius.Layout.Positive_Count);

   overriding
   procedure Set_Text (Renderer  : access GUI_Renderer;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String);

   overriding
   procedure Begin_Render (Renderer : access GUI_Renderer);

   overriding
   procedure End_Render (Renderer : access GUI_Renderer);

   overriding
   procedure Set_Point (Renderer : access GUI_Renderer;
                        Point    : in     Aquarius.Layout.Position);

   procedure Ensure_Line (TB       : Gtk_Text_Buffer;
                          Line     : Aquarius.Layout.Positive_Count);

   --------------
   -- Add_Text --
   --------------

   procedure Add_Text (Renderer : access GUI_Renderer'Class;
                       Position : in out Aquarius.Layout.Position;
                       Class    : in     String;
                       Text     : in     String)
   is
      use type Aquarius.Layout.Count;
      TB   : constant Gtk_Text_Buffer := Renderer.Text_Buffer;
      Tag  : constant Gtk_Text_Tag :=
        Aquarius.GUI.Text.Get_Tag_Entry (TB, Class, Renderer.Style);
      Iter : Gtk_Text_Iter;
   begin
      TB.Get_Iter_At_Line_Offset (Iter,
                                  Gint (Position.Line) - 1,
                                  Gint (Position.Column) - 1);
      TB.Insert_With_Tags (Iter, Text, Tag);

      Position.Column := Position.Column +
        Aquarius.Layout.Count (Text'Length);
   end Add_Text;

   ------------------
   -- Begin_Render --
   ------------------

   overriding
   procedure Begin_Render (Renderer : access GUI_Renderer) is
      TB : constant Gtk_Text_Buffer := Renderer.Text_Buffer;
      Start, Finish : Gtk_Text_Iter;
   begin
      TB.Begin_User_Action;
      TB.Get_Start_Iter (Start);
      TB.Get_End_Iter (Finish);
      TB.Delete (Start, Finish);
      Renderer.Pos := (1, 1);
   end Begin_Render;

   ----------------
   -- End_Render --
   ----------------

   overriding
   procedure End_Render (Renderer : access GUI_Renderer) is
      TB : constant Gtk_Text_Buffer := Renderer.Text_Buffer;
   begin
      TB.End_User_Action;
   end End_Render;

   -----------------
   -- Ensure_Line --
   -----------------

   procedure Ensure_Line (TB       : Gtk_Text_Buffer;
                          Line     : Aquarius.Layout.Positive_Count)
   is
      TB_Line   : constant Gint := Gint (Line) - 1;
      New_Lines : constant Gint := TB_Line - TB.Get_Line_Count + 1;
   begin
      if New_Lines > 0 then
         declare
            End_Iter : Gtk_Text_Iter;
            S        : constant String (1 .. Positive (New_Lines)) :=
              (others => Character'Val (10));
         begin
            TB.Get_End_Iter (End_Iter);
            TB.Insert_At_Cursor (S);
         end;
      end if;
   end Ensure_Line;

   ----------------------
   -- New_GUI_Renderer --
   ----------------------

   function New_GUI_Renderer (Target : Gtk.Text_Buffer.Gtk_Text_Buffer)
                             return Aquarius_Renderer
   is
      Result : GUI_Renderer;
   begin
      Result.Text_Buffer := Target;
      Result.Default_Tag := Target.Create_Tag ("default");
      Pango.Font.Set_Property (Result.Default_Tag,
                               Gtk.Text_Tag.Font_Desc_Property,
                               Aquarius.GUI.Text.Default_Font);
      return new GUI_Renderer'(Result);
   end New_GUI_Renderer;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line (Renderer : access GUI_Renderer'Class;
                       Line     : in     Aquarius.Layout.Positive_Count)
   is
      TB : constant Gtk_Text_Buffer := Renderer.Text_Buffer;
      Line_Iter : Gtk_Text_Iter;
   begin
      Ensure_Line (TB, Line);
      TB.Get_Iter_At_Line (Line_Iter, Gint (Line) - 1);
      TB.Place_Cursor (Line_Iter);
   end Set_Line;

   ---------------
   -- Set_Point --
   ---------------

   overriding
   procedure Set_Point (Renderer : access GUI_Renderer;
                        Point    : in     Aquarius.Layout.Position)
   is
      TB   : constant Gtk_Text_Buffer := Renderer.Text_Buffer;
      Iter : Gtk_Text_Iter;
   begin
      TB.Get_Iter_At_Line_Offset (Iter,
                                  Gint (Point.Line) - 1,
                                  Gint (Point.Column) - 1);
      TB.Place_Cursor (Iter);
   end Set_Point;

   --------------
   -- Set_Text --
   --------------

   overriding
   procedure Set_Text (Renderer  : access GUI_Renderer;
                       Position  : in     Aquarius.Layout.Position;
                       Class     : in     String;
                       Text      : in     String)
   is
      use type Aquarius.Layout.Positive_Count;
      Render_Pos : Aquarius.Layout.Position := Renderer.Current_Position;
   begin

      if Render_Pos.Line < Position.Line then
         Set_Line (Renderer, Position.Line);
         Render_Pos.Line := Position.Line;
         Render_Pos.Column := 1;
      end if;

      if Render_Pos.Column < Position.Column then
         declare
            Spaces : constant String (Positive (Render_Pos.Column) ..
                                        Positive (Position.Column - 1)) :=
              (others => ' ');
         begin
            Add_Text (Renderer, Render_Pos, "", Spaces);
         end;
         Render_Pos.Column := Position.Column;
      end if;

      Add_Text (Renderer, Render_Pos, Class, Text);
      Renderer.Set_Current_Position (Render_Pos);

   end Set_Text;

end Aquarius.Rendering.GUI;
