with Gtk.Layout;
with Gtk.Text_View;

with Glade.XML;

with Aquarius.Bubbles;

package Aquarius.GUI.GUI_Bubbles is

   type GUI_Bubble is private;

   procedure Initialise (XML : Glade.XML.Glade_XML);

   procedure Create (Bubble   : in out GUI_Bubble;
                     Layout   : in     Gtk.Layout.Gtk_Layout;
                     X, Y     : in     Integer;
                     Width    : in     Positive;
                     Contents : in     Aquarius.Bubbles.Aquarius_Bubble);

   function Current_Position return Position;

private

   type GUI_Bubble is
      record
         X, Y     : Integer;
         Width    : Positive;
         Contents : Aquarius.Bubbles.Aquarius_Bubble;
         View     : Gtk.Text_View.Gtk_Text_View;
      end record;

end Aquarius.GUI.GUI_Bubbles;
