private with Gdk.RGBA;
private with Komnenos.Fragments;

package Komnenos.UI.Gtk_UI is

   function Create_UI
     (Config_Folder_Path : String)
      return Komnenos_UI;

   type Display_Interface is interface;

   function Display_Grabs_Focus
     (Display : not null access Display_Interface'Class)
      return Boolean;

   type Komnenos_Display is access all Display_Interface'Class;

private

   function To_RGBA
     (Colour_Spec : String)
      return Gdk.RGBA.Gdk_RGBA;

   function Find_Fragment
     (Display : not null access Display_Interface'Class)
      return Komnenos.Fragments.Fragment_Type;

end Komnenos.UI.Gtk_UI;
