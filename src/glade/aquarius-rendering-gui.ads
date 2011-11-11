with Gtk.Text_Buffer;

package Aquarius.Rendering.GUI is

   function New_GUI_Renderer (Target : Gtk.Text_Buffer.Gtk_Text_Buffer)
                             return Aquarius_Renderer;

end Aquarius.Rendering.GUI;
