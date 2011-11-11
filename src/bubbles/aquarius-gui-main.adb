with Gtk.Handlers;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;

package body Aquarius.GUI.Main is

   package Main_Window_Callback is
      new Gtk.Handlers.Callback (Gtk.Window.Gtk_Window_Record);

   procedure Destroy_Handler (W : access Gtk.Window.Gtk_Window_Record'Class);

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Window.Gtk_Window_Record'Class)
   is
      pragma Unreferenced (W);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (XML : Glade.XML.Glade_XML) is
      Main_Window : constant Gtk.Widget.Gtk_Widget :=
        Glade.XML.Get_Widget (XML, "Aquarius_Main_Window");
   begin
      Main_Window_Callback.Connect
        (Gtk.Window.Gtk_Window (Main_Window),
         "destroy",
         Main_Window_Callback.To_Marshaller (Destroy_Handler'Access));
   end Initialise;

end Aquarius.GUI.Main;
