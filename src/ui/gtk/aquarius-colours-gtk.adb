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

end Aquarius.Colours.Gtk;
