with Ada.Containers.Vectors;

with Glib;

with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Widget;

with Aquarius.GUI.Text;
with Aquarius.Rendering.GUI;
with Aquarius.Styles;

package body Aquarius.GUI.GUI_Bubbles is

   Local_Current_Position : Position;
   Bubble_Area : Gtk.Layout.Gtk_Layout;

   package Resize_Cb is
      new Gtk.Handlers.Callback (Gtk.Layout.Gtk_Layout_Record);

   package Bubble_Vector is
      new Ada.Containers.Vectors (Positive, GUI_Bubble, "=");

   Current_Bubbles : Bubble_Vector.Vector;

   procedure On_Resize
     (Layout : access Gtk.Layout.Gtk_Layout_Record'Class);

   ------------
   -- Create --
   ------------

   procedure Create (Bubble   : in out GUI_Bubble;
                     Layout   : in     Gtk.Layout.Gtk_Layout;
                     Width    : in     Positive;
                     Contents : in     Aquarius.Bubbles.Aquarius_Bubble)
   is
      Text_View : Gtk.Text_View.Gtk_Text_View;
      Renderer  : Aquarius.Rendering.Aquarius_Renderer;
   begin
      Gtk.Text_View.Gtk_New (Text_View);
      Text_View.Set_Size_Request (Width => Glib.Gint (Width * 8),
                                  Height => 256);
      Text_View.Modify_Bg (Gtk.Enums.State_Normal,
                           Aquarius.GUI.Text.Get_Gdk_Colour
                             (Gtk.Widget.Get_Default_Colormap,
                              Contents.Background));
      Text_View.Show;
      Layout.Put (Text_View, Glib.Gint (X), Glib.Gint (Y));
      Bubble := (X, Y, Width, Contents, Text_View);
      Contents.Set_Width (Width);
      Renderer :=
        Aquarius.Rendering.GUI.New_GUI_Renderer (Text_View.Get_Buffer);
      Renderer.Set_Style (Aquarius.Styles.Default_Style);
      Contents.Set_Renderer (Renderer);
      Contents.Render;
      Current_Bubbles.Append (Bubble);
   end Create;

   ----------------------
   -- Current_Position --
   ----------------------

   function Current_Position return Position is
   begin
      return (0, 0, 500, 800);
   end Current_Position;

   ----------------
   -- Get_Bubble --
   ----------------

   function Get_Bubble (Index : Positive) return GUI_Bubble is
   begin
      return Current_Bubbles.Element (Index);
   end Get_Bubble;

   ----------------
   -- Get_Bubble --
   ----------------

   function Get_Bubble (Index : Positive)
                       return Aquarius.Bubbles.Aquarius_Bubble
   is
   begin
      return Current_Bubbles.Element (Index).Contents;
   end Get_Bubble;

   ----------------
   -- Get_Bubble --
   ----------------

   function Get_Bubble (Bubble : GUI_Bubble)
                       return Aquarius.Bubbles.Aquarius_Bubble
   is
   begin
      return Bubble.Contents;
   end Get_Bubble;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position (Index : Positive) return Position is
      Result : Position;
      Item   : constant GUI_Bubble := Get_Bubble (Index);
      Width, Height : Glib.Gint;
   begin
      Result.Top  := Item.Y;
      Result.Left := Item.X;
      Width := Item.View.Get_Allocation_Width;
      Height := Item.View.Get_Allocation_Height;
      Result.Bottom := Result.Top + Integer (Height);
      Result.Right := Result.Left + Integer (Width);
      return Result;
   end Get_Position;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (XML : Glade.XML.Glade_XML) is
   begin

      Bubble_Area :=
        Gtk.Layout.Gtk_Layout
        (Glade.XML.Get_Widget (XML, "Bubble_Area"));

      Resize_Cb.Connect (Bubble_Area, "size_allocate",
                         On_Resize'Access);

   end Initialise;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
     (Layout : access Gtk.Layout.Gtk_Layout_Record'Class)
   is
      pragma Unreferenced (Layout);

      Width, Height : Glib.Gint;
   begin
      Width := Bubble_Area.Get_Allocation_Width;
      Height := Bubble_Area.Get_Allocation_Height;
      Local_Current_Position.Bottom :=
        Local_Current_Position.Top + Integer (Height);
      Local_Current_Position.Right :=
        Local_Current_Position.Left + Integer (Width);
   end On_Resize;

end Aquarius.GUI.GUI_Bubbles;
