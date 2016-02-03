private with Glib;
private with Gtk.Grid;

with Gtk.Widget;

with Komnenos.Layouts;
with Komnenos.UI.Gtk_UI.Borders;

package Komnenos.UI.Gtk_UI.Frames is

   type Root_Gtk_Frame_Record is
     new Gtk.Widget.Gtk_Widget_Record
     and Borders.UI_Fragment_Interface
   with private;

   function Fragment
     (Frame : Root_Gtk_Frame_Record)
      return Komnenos.Fragments.Fragment_Type;

   type Gtk_Frame is access all Root_Gtk_Frame_Record'Class;

   function New_Frame
     (Layout   : not null access Komnenos.Layouts.Root_Layout_Type'Class;
      Fragment : Komnenos.Fragments.Fragment_Type)
      return Gtk_Frame;

private

   type Root_Gtk_Frame_Record is
     new Gtk.Grid.Gtk_Grid_Record
     and Borders.UI_Fragment_Interface with
      record
         Dragging   : Boolean := False;
         Layout     : Komnenos.Layouts.Layout_Type;
         Fragment   : Komnenos.Fragments.Fragment_Type;
         Start_X    : Glib.Gdouble;
         Start_Y    : Glib.Gdouble;
         Border     : Gdk.RGBA.Gdk_RGBA;
      end record;

   overriding function Border_Colour
     (Fragment : Root_Gtk_Frame_Record)
      return Gdk.RGBA.Gdk_RGBA;

   overriding procedure Set_Corner_Widget
     (Fragment  : in out Root_Gtk_Frame_Record;
      Corner    : Borders.Border_Corner;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);

   overriding procedure Set_Side_Widget
     (Fragment  : in out Root_Gtk_Frame_Record;
      Edge      : Borders.Border_Edge;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);

end Komnenos.UI.Gtk_UI.Frames;
