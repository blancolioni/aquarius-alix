with Gtk.Layout;
with Gtk.Scrolled_Window;

with Komnenos.Layouts;

with Komnenos.UI.Gtk_UI.Navigation;

private with Komnenos.UI.Gtk_UI.Frames.Maps;

package Komnenos.UI.Gtk_UI.Layout_Table is

   type Root_Gtk_Layout_Table is
     new Komnenos.Layouts.Root_Layout_Type with private;

   procedure Set_Top_Left
     (Table : in out Root_Gtk_Layout_Table;
      X, Y  : Natural);

   type Gtk_Layout_Table is access all Root_Gtk_Layout_Table'Class;

   function Create_Layout_Table
     (Navigation    : Komnenos.UI.Gtk_UI.Navigation.Gtk_Navigation_Panel;
      Main_Scroll   : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Main_Layout   : Gtk.Layout.Gtk_Layout)
      return Gtk_Layout_Table;

private

   type Root_Gtk_Layout_Table is
     new Komnenos.Layouts.Root_Layout_Type with
      record
         Self          : Komnenos.Layouts.Layout_Type;
         Navigation    : Komnenos.UI.Gtk_UI.Navigation.Gtk_Navigation_Panel;
         Layout_Widget : Gtk.Layout.Gtk_Layout;
         Frame_Map     : Frames.Maps.Map;
      end record;

   overriding procedure Item_Moved
     (Layout : in out Root_Gtk_Layout_Table;
      Item   : Komnenos.Fragments.Fragment_Type);

   overriding procedure Item_Placed
     (Layout : in out Root_Gtk_Layout_Table;
      Item   : Komnenos.Fragments.Fragment_Type);

   overriding procedure Set_Full_Size
     (Layout      : in out Root_Gtk_Layout_Table;
      Full_Width  : Natural;
      Full_Height : Natural);

   function Get_Frame
     (Layout   : Root_Gtk_Layout_Table'Class;
      Fragment : Komnenos.Fragments.Fragment_Type)
      return Komnenos.UI.Gtk_UI.Frames.Gtk_Frame;

end Komnenos.UI.Gtk_UI.Layout_Table;
