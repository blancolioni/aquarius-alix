with Glib;

package body Aquarius.Colours.Gtk is

   ------------------
   -- To_Gdk_Color --
   ------------------

   function To_Gdk_Color
     (Colour : Aquarius_Colour)
      return Gdk.Color.Gdk_Color
   is
      use Glib, Gdk.Color;
      Result : Gdk_Color;
   begin
      Set_Rgb (Result,
               Red   => Guint16 (Colour.Red) * 256,
               Green => Guint16 (Colour.Green) * 256,
               Blue  => Guint16 (Colour.Blue) * 256);
      return Result;
   end To_Gdk_Color;

   -----------------
   -- To_Gdk_RGBA --
   -----------------

   function To_Gdk_RGBA
     (Colour : Aquarius_Colour)
      return Gdk.RGBA.Gdk_RGBA
   is
      use Glib;
   begin
      return (Red => Gdouble (Colour.Red) / 256.0,
              Green => Gdouble (Colour.Green) / 256.0,
              Blue  => Gdouble (Colour.Blue) / 256.0,
              Alpha => 1.0);
   end To_Gdk_RGBA;

end Aquarius.Colours.Gtk;
