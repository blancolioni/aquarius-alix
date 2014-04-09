with Gdk.Color;
with Gdk.RGBA;

package Aquarius.Colours.Gtk is

   function To_Gdk_Color
     (Colour : Aquarius_Colour)
      return Gdk.Color.Gdk_Color;

   function To_Gdk_RGBA
     (Colour : Aquarius_Colour)
      return Gdk.RGBA.Gdk_RGBA;

end Aquarius.Colours.Gtk;
