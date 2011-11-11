with Gtk.Builder;

with Aquarius.UI.Menus;

package Aquarius.GUI.Menu is

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder);

   procedure Show_Plugin_Menu (Menu : Aquarius.UI.Menus.Aquarius_Menu);

end Aquarius.GUI.Menu;
