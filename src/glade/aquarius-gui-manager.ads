with Gtk.Builder;

with Aquarius.Fragments;

package Aquarius.GUI.Manager is

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder);

   procedure Add_Fragment
     (Item      : Aquarius.Fragments.Aquarius_Fragment;
      Suggest_X : Integer;
      Suggest_Y : Integer);

end Aquarius.GUI.Manager;
