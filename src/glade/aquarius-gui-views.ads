with Gtk.Builder;
with Aquarius.Projects;

package Aquarius.GUI.Views is

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder);

   procedure Update_Views (Project : Aquarius.Projects.Aquarius_Project);

end Aquarius.GUI.Views;
