private with Gtk.Text_Buffer;

package Aquarius.Fragments.Text is

   type Text_Fragment is abstract new Root_Fragment_Type with private;

   overriding
   procedure Create_Widget
     (Fragment : in out Text_Fragment);

private

   type Text_Fragment is
     abstract new Root_Fragment_Type with
      record
         Text_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      end record;

end Aquarius.Fragments.Text;
