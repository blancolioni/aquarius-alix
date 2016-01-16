private with Cairo;
private with Gtk.Drawing_Area;

with Gdk.RGBA;
with Gtk.Widget;

package Komnenos.UI.Gtk_UI.Borders is

   type UI_Fragment_Interface is interface;

   type Border_Corner is (Top_Left, Top_Right, Bottom_Right, Bottom_Left);
   type Border_Edge is (Top, Right, Bottom, Left);

   function Border_Colour
     (Fragment : UI_Fragment_Interface)
      return Gdk.RGBA.Gdk_RGBA
      is abstract;

   procedure Set_Corner_Widget
     (Fragment  : in out UI_Fragment_Interface;
      Corner    : Border_Corner;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is abstract;

   procedure Set_Side_Widget
     (Fragment  : in out UI_Fragment_Interface;
      Edge      : Border_Edge;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is abstract;

   procedure Add_Borders
     (Fragment : in out UI_Fragment_Interface'Class);

private

   type Border_Widget_Record is abstract
     new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         Surface : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Colour  : Gdk.RGBA.Gdk_RGBA;
      end record;

   procedure Draw_Border
     (Border : in out Border_Widget_Record)
   is abstract;

end Komnenos.UI.Gtk_UI.Borders;
