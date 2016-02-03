private with Gdk.RGBA;
private with Komnenos.Fragments;

package Komnenos.UI.Gtk_UI is

   function Create_UI
     (Config_Folder_Path : String)
      return Komnenos_UI;

private

   function To_RGBA
     (Colour_Spec : String)
      return Gdk.RGBA.Gdk_RGBA;

end Komnenos.UI.Gtk_UI;
