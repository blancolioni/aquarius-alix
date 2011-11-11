with Gtk; use Gtk;
with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Aquarius_Main_Window_Pkg; use Aquarius_Main_Window_Pkg;

procedure Aquarius_Main_Window is
   Aquarius_Main_Window : Aquarius_Main_Window_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Aquarius_Main_Window);
   Show_All (Aquarius_Main_Window);
   Gtk.Main.Main;
end Aquarius_Main_Window;
Exception = CONSTRAINT_ERROR
GATE: Internal error. Please send a bug report with the XML
file ..\..\src\glade\Aquarius.glade and the GtkAda version to gtkada@lists.act-europe.fr
