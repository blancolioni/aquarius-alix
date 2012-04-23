with Glib;

with Gtk.Enums;
with Gtk.Text_View;

with Aquarius.GUI.Text;
with Aquarius.Rendering.GUI;
with Aquarius.Styles;

package body Aquarius.Fragments.Text is

   -------------------
   -- Create_Widget --
   -------------------

   overriding
   procedure Create_Widget
     (Fragment : in out Text_Fragment)
   is
      Text_View : Gtk.Text_View.Gtk_Text_View;
   begin
      Gtk.Text_Buffer.Gtk_New (Fragment.Text_Buffer);
      Fragment.Renderer :=
        Aquarius.Rendering.GUI.New_GUI_Renderer
          (Fragment.Text_Buffer);
      Fragment.Renderer.Set_Style
        (Aquarius.Styles.Default_Style);
      Gtk.Text_View.Gtk_New (Text_View);
      Text_View.Set_Size_Request (Width  => Glib.Gint (Fragment.Width),
                                  Height => Glib.Gint (Fragment.Height));
      Text_View.Modify_Font (Aquarius.GUI.Text.Default_Font);
      Text_View.Modify_Bg
        (Gtk.Enums.State_Normal,
         Aquarius.GUI.Text.Get_Gdk_Colour
           (Gtk.Widget.Get_Default_Colormap,
            Fragment.Background));

      Text_View.Set_Buffer (Fragment.Text_Buffer);
      Text_View.Set_Wrap_Mode (Wrap_Mode => Gtk.Enums.Wrap_Word);
      Text_View.Set_Border_Width (5);
      Text_View.Show;
      Fragment.Widget := Gtk.Widget.Gtk_Widget (Text_View);
   end Create_Widget;

end Aquarius.Fragments.Text;
