with Glib;

with Gdk.Color;
with Gdk.Drawable;
with Gdk.GC;
with Gdk.Pixmap;

with Gtk.Drawing_Area;
with Gtk.Handlers;
with Gtk.Widget;

with Aquarius.Bubbles;

with Aquarius.GUI.GUI_Bubbles;
with Aquarius.GUI.Text;

package body Aquarius.GUI.Panning is

   Panning_Area   : Gtk.Drawing_Area.Gtk_Drawing_Area;

   Panning_Background : Gdk.Color.Gdk_Color;
   Panning_Cursor     : Gdk.Color.Gdk_Color;

   package Expose_Cb is
      new Gtk.Handlers.Return_Callback
     (Gtk.Drawing_Area.Gtk_Drawing_Area_Record,
      Boolean);

   procedure Create_Panner (GC     : Gdk.Gdk_GC;
                            Pixmap : Gdk.Pixmap.Gdk_Pixmap);

   function Expose
     (Draw : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class)
     return Boolean;

   procedure Get_Screen_Coordinates
     (Pos    : Aquarius.GUI.GUI_Bubbles.Position;
      Width  : in Glib.Gint;
      Height : in Glib.Gint;
      X, Y   : out Glib.Gint;
      W, H   : out Glib.Gint);

   procedure Update;

   -------------------
   -- Create_Panner --
   -------------------

   procedure Create_Panner (GC     : Gdk.Gdk_GC;
                            Pixmap : Gdk.Pixmap.Gdk_Pixmap)
   is
      Width, Height : Glib.Gint;
   begin
      Gdk.GC.Set_Foreground (GC, Panning_Background);
      Gdk.Drawable.Get_Size (Pixmap, Width, Height);
      Gdk.Drawable.Draw_Rectangle (Pixmap, GC, True, 0, 0, Width, Height);

      for I in 1 .. Aquarius.GUI.GUI_Bubbles.Count loop
         declare
            Pos : constant Aquarius.GUI.GUI_Bubbles.Position :=
              Aquarius.GUI.GUI_Bubbles.Get_Position (I);
            Bubble : constant Aquarius.Bubbles.Aquarius_Bubble :=
              Aquarius.GUI.GUI_Bubbles.Get_Bubble (I);
            X, Y, W, H : Glib.Gint;
            Colour     : Gdk.Color.Gdk_Color;
         begin
            Get_Screen_Coordinates (Pos, Width, Height,
                                    X, Y, W, H);
            Colour :=  Aquarius.GUI.Text.Get_Gdk_Colour
                             (Gtk.Widget.Get_Default_Colormap,
                              Bubble.Background);
            Gdk.GC.Set_Foreground (GC, Colour);

            Gdk.Drawable.Draw_Rectangle (Pixmap, GC, True,
                                         X, Y, W, H);
         end;
      end loop;

      declare
         Pos : constant Aquarius.GUI.GUI_Bubbles.Position :=
           Aquarius.GUI.GUI_Bubbles.Current_Position;
         X, Y, W, H : Glib.Gint;
      begin
         Get_Screen_Coordinates (Pos, Width, Height,
                                 X, Y, W, H);
         Gdk.GC.Set_Foreground (GC, Panning_Cursor);
         Gdk.Drawable.Draw_Rectangle (Pixmap, GC, False,
                                      X, Y, W, H);
      end;

   end Create_Panner;

   ------------
   -- Expose --
   ------------

   function Expose
     (Draw : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class)
     return Boolean
   is
      pragma Unreferenced (Draw);
   begin
      Update;
      return False;
   end Expose;

   ----------------------------
   -- Get_Screen_Coordinates --
   ----------------------------

   procedure Get_Screen_Coordinates
     (Pos    : Aquarius.GUI.GUI_Bubbles.Position;
      Width  : in Glib.Gint;
      Height : in Glib.Gint;
      X, Y   : out Glib.Gint;
      W, H   : out Glib.Gint)
   is
      Max_X      : constant Integer := Aquarius.Bubbles.Max_X;
      Max_Y      : constant Integer := Aquarius.Bubbles.Max_Y;
   begin
      X := Glib.Gint (Float (Width) * Float (Pos.Left) / Float (Max_X));
      Y := Glib.Gint (Float (Height) * Float (Pos.Top) / Float (Max_Y));
      W := Glib.Gint (Float (Width) * Float (Pos.Right - Pos.Left) /
                        Float (Max_X));
      H := Glib.Gint (Float (Height) * Float (Pos.Bottom - Pos.Top) /
                        Float (Max_Y));
   end Get_Screen_Coordinates;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (XML : Glade.XML.Glade_XML) is

      Success : Boolean;

   begin

      Panning_Area :=
        Gtk.Drawing_Area.Gtk_Drawing_Area
        (Glade.XML.Get_Widget (XML, "Panning_Area"));

      Expose_Cb.Object_Connect (Panning_Area, "expose_event",
                                Expose_Cb.To_Marshaller (Expose'Access),
                                Slot_Object => Panning_Area);

      Panning_Background := Gdk.Color.Parse ("lightgray");
      Gdk.Color.Alloc_Color (Colormap   => Gtk.Widget.Get_Default_Colormap,
                             Color      => Panning_Background,
                             Writeable  => False,
                             Best_Match => True,
                             Success    => Success);
      Panning_Cursor := Gdk.Color.Parse ("darkblue");
      Gdk.Color.Alloc_Color (Colormap   => Gtk.Widget.Get_Default_Colormap,
                             Color      => Panning_Cursor,
                             Writeable  => False,
                             Best_Match => True,
                             Success    => Success);
      if not Success then
         raise Constraint_Error with "could not allocate panning background";
      end if;

   end Initialise;

   ------------
   -- Update --
   ------------

   procedure Update is
      Panning_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Drawable       : Gdk.Drawable.Gdk_Drawable;
      GC : Gdk.Gdk_GC;
      Width, Height  : Glib.Gint;
   begin
      Drawable := Panning_Area.Get_Window;
      Gdk.GC.Gdk_New (GC, Drawable);
      Gdk.Drawable.Get_Size (Drawable, Width, Height);
      Gdk.Pixmap.Gdk_New (Panning_Pixmap, Drawable,
                          Width,
                          Height);
      Create_Panner (GC, Panning_Pixmap);
      Gdk.Drawable.Draw_Drawable (Drawable, GC, Panning_Pixmap,
                                  0, 0, 0, 0);
      Gdk.Pixmap.Unref (Panning_Pixmap);
      Gdk.GC.Unref (GC);
   end Update;

end Aquarius.GUI.Panning;
