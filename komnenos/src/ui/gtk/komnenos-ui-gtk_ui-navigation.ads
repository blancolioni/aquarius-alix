private with Cairo;
private with Glib.Object;

with Gtk.Widget;

with Komnenos.Layouts;

package Komnenos.UI.Gtk_UI.Navigation is

   type Root_Gtk_Navigation_Panel is tagged private;

   type Navigation_Callback is access
     procedure (Layout   : Komnenos.Layouts.Layout_Type;
                New_Left : Natural;
                New_Top  : Natural);

   procedure Set_Layout
     (Panel  : in out Root_Gtk_Navigation_Panel'Class;
      Layout : not null access Komnenos.Layouts.Root_Layout_Type'Class);

   procedure On_Navigate
     (Panel    : in out Root_Gtk_Navigation_Panel'Class;
      Callback : Navigation_Callback);

   procedure Refresh
     (Panel : in out Root_Gtk_Navigation_Panel'Class);

   procedure On_Layout_Configured
     (Panel : in out Root_Gtk_Navigation_Panel'Class);

   type Gtk_Navigation_Panel is access all Root_Gtk_Navigation_Panel'Class;

   function Create_Navigation_Panel
     (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk_Navigation_Panel;

private

   type Root_Gtk_Navigation_Panel is new Glib.Object.GObject_Record with
      record
         Widget      : Gtk.Widget.Gtk_Widget;
         Surface     : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Layout      : Komnenos.Layouts.Layout_Type;
         Cb_Navigate : Navigation_Callback;
         Size        : Gtk.Widget.Gtk_Allocation;
         Scale       : Float;
         Left        : Glib.Gdouble;
         Top         : Glib.Gdouble;
      end record;

   procedure Calculate_Scale
     (Navigator : in out Root_Gtk_Navigation_Panel'Class);

end Komnenos.UI.Gtk_UI.Navigation;
