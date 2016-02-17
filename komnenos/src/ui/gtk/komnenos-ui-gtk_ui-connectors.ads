private with Gtk.Drawing_Area;
with Gtk.Widget;

private with Cairo;

with Komnenos.Connectors;

package Komnenos.UI.Gtk_UI.Connectors is

   type Root_Gtk_Connector_Record is
     new Gtk.Widget.Gtk_Widget_Record
     and Komnenos.Connectors.Connector_Display_Interface
   with private;

   function Connector
     (UI_Connector : Root_Gtk_Connector_Record'Class)
      return Komnenos.Connectors.Connector_Type;

   function Layout_Location
     (UI_Connector : Root_Gtk_Connector_Record'Class)
      return Layout_Point;

   type Gtk_Connector is access all Root_Gtk_Connector_Record'Class;

   function New_Connector
     (From : Komnenos.Connectors.Connector_Type)
      return Gtk_Connector;

private

   type Root_Gtk_Connector_Record is
     new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
     and Komnenos.Connectors.Connector_Display_Interface with
      record
         Connector : Komnenos.Connectors.Connector_Type;
         Surface   : Cairo.Cairo_Surface;
      end record;

   overriding procedure Remove
     (Con : in out Root_Gtk_Connector_Record)
   is null;

   overriding procedure Update
     (Con : in out Root_Gtk_Connector_Record);

   procedure Render
     (Con : in out Root_Gtk_Connector_Record'Class);

end Komnenos.UI.Gtk_UI.Connectors;
