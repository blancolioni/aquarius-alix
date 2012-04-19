with Gtk.Builder;

with Aquarius.GUI.Fragments;

package Aquarius.GUI.Manager is

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder);

   procedure Add_Fragment (Item : Aquarius.GUI.Fragments.Aquarius_Fragment);

end Aquarius.GUI.Manager;
