with Glib;
with Gtk.Enums;
with Gtk.Text_View;

with Aquarius.GUI.Text;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering.GUI;
with Aquarius.Styles;
with Aquarius.Trees;
with Aquarius.Trees.Cursors;

package body Aquarius.GUI.Fragments is

   type Note_Fragment_Type is
     new Root_Fragment_Type with
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding
   procedure Render (Fragment : in out Note_Fragment_Type);

   type Program_Fragment_Type is
     new Root_Fragment_Type with
      record
         Program : Aquarius.Programs.Program_Tree;
      end record;

   overriding
   procedure Render (Fragment : in out Program_Fragment_Type);

   ----------------
   -- Background --
   ----------------

   function Background
     (Fragment : Root_Fragment_Type'Class)
      return Aquarius.Fonts.Aquarius_Colour
   is
   begin
      return Fragment.Background;
   end Background;

   ------------
   -- Bottom --
   ------------

   function Bottom (Fragment : Root_Fragment_Type'Class) return Integer is
   begin
      return Fragment.Y + Fragment.Height;
   end Bottom;

   --------------------------
   -- Create_Note_Fragment --
   --------------------------

   function Create_Note_Fragment
     (Width, Height : in     Positive;
      Initial_Text  : in     String)
      return Aquarius_Fragment
   is
      Fragment : constant Aquarius_Fragment :=
                   new Note_Fragment_Type
                     '(Root_Fragment_Type with
                       Text => Ada.Strings.Unbounded.To_Unbounded_String
                         (Initial_Text));
   begin
      Gtk.Text_Buffer.Gtk_New (Fragment.Text_Buffer);
      Fragment.Width := Width;
      Fragment.Height := Height;
      Fragment.Set_Background
        (Aquarius.Fonts.Parse_Colour ("yellow"));
      Fragment.Renderer :=
        Aquarius.Rendering.GUI.New_GUI_Renderer
          (Fragment.Text_Buffer);
      Fragment.Renderer.Set_Style
        (Aquarius.Styles.Default_Style);
      return Fragment;
   end Create_Note_Fragment;

   -----------------------------
   -- Create_Program_Fragment --
   -----------------------------

   function Create_Program_Fragment
     (Program       : in     Aquarius.Programs.Program_Tree)
      return Aquarius_Fragment
   is
      Fragment : constant Aquarius_Fragment :=
                   new Program_Fragment_Type
                     '(Root_Fragment_Type with
                       Program => Program);
   begin
      Gtk.Text_Buffer.Gtk_New (Fragment.Text_Buffer);
      Fragment.Width  := 300;
      Fragment.Height := 400;
      Fragment.Renderer :=
        Aquarius.Rendering.GUI.New_GUI_Renderer
          (Fragment.Text_Buffer);
      Fragment.Renderer.Set_Style
        (Aquarius.Styles.Default_Style);
      return Fragment;
   end Create_Program_Fragment;

   -------------------
   -- Create_Widget --
   -------------------

   function Create_Widget
     (Fragment : Root_Fragment_Type)
      return Gtk.Widget.Gtk_Widget
   is
      Text_View : Gtk.Text_View.Gtk_Text_View;
   begin
      Gtk.Text_View.Gtk_New (Text_View);
      Text_View.Set_Size_Request (Width  => Glib.Gint (Fragment.Width),
                                  Height => Glib.Gint (Fragment.Height));
      Text_View.Modify_Font (Aquarius.GUI.Text.Default_Font);
      Text_View.Modify_Bg
        (Gtk.Enums.State_Normal,
         Aquarius.GUI.Text.Get_Gdk_Colour
           (Gtk.Widget.Get_Default_Colormap,
            Fragment.Background));

      Text_View.Set_Buffer (Fragment.Text_Buffer);
      Text_View.Set_Wrap_Mode (Wrap_Mode => Gtk.Enums.Wrap_Word);
      Text_View.Show;
      return Gtk.Widget.Gtk_Widget (Text_View);
   end Create_Widget;

   ------------
   -- Height --
   ------------

   function Height (Fragment : Root_Fragment_Type'Class) return Positive is
   begin
      return Fragment.Height;
   end Height;

   ----------
   -- Left --
   ----------

   function Left (Fragment : Root_Fragment_Type'Class) return Integer is
   begin
      return Fragment.X;
   end Left;

   ------------
   -- Render --
   ------------

   overriding
   procedure Render (Fragment : in out Note_Fragment_Type) is
      R : constant Aquarius.Rendering.Aquarius_Renderer := Fragment.Renderer;
   begin
      R.Begin_Render;
      R.Set_Text
        (Position => (1, 1),
         Class    => "normal",
         Text     => Ada.Strings.Unbounded.To_String (Fragment.Text));
      R.End_Render;
   end Render;

   ------------
   -- Render --
   ------------

   overriding
   procedure Render (Fragment : in out Program_Fragment_Type) is
      Cursor : constant Aquarius.Trees.Cursors.Cursor :=
                 Aquarius.Trees.Cursors.Left_Of_Tree
                   (Fragment.Program);
   begin
      Aquarius.Programs.Arrangements.Arrange (Fragment.Program,
                                              Cursor,
                                              0);
      Aquarius.Programs.Arrangements.Render
        (Fragment.Program, Fragment.Renderer, Cursor, "");
   end Render;

   -----------
   -- Right --
   -----------

   function Right (Fragment : Root_Fragment_Type'Class) return Integer is
   begin
      return Fragment.X + Fragment.Width;
   end Right;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Fragment : in out Root_Fragment_Type'Class;
                             Background : Aquarius.Fonts.Aquarius_Colour)
   is
   begin
      Fragment.Background := Background;
   end Set_Background;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Fragment : in out Root_Fragment_Type'Class;
                           X, Y     : Integer)
   is
   begin
      Fragment.X := X;
      Fragment.Y := Y;
   end Set_Position;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Fragment : in out Root_Fragment_Type'Class;
      Text     : in     String)
   is
   begin
      Fragment.Title := Ada.Strings.Unbounded.To_Unbounded_String (Text);
   end Set_Title;

   -----------
   -- Title --
   -----------

   function Title (Fragment : Root_Fragment_Type'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Fragment.Title);
   end Title;

   ---------
   -- Top --
   ---------

   function Top (Fragment : Root_Fragment_Type'Class) return Integer is
   begin
      return Fragment.Y;
   end Top;

   -----------
   -- Width --
   -----------

   function Width (Fragment : Root_Fragment_Type'Class) return Positive is
   begin
      return Fragment.Width;
   end Width;

   -------
   -- X --
   -------

   function X (Fragment : Root_Fragment_Type'Class) return Integer is
   begin
      return Fragment.X;
   end X;

   -------
   -- Y --
   -------

   function Y (Fragment : Root_Fragment_Type'Class) return Integer is
   begin
      return Fragment.Y;
   end Y;

end Aquarius.GUI.Fragments;
