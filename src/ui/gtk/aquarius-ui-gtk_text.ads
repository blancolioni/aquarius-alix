with Gtk.Text_Buffer;
with Gtk.Text_Tag;

with Pango.Font;

with Aquarius.Styles;

package Aquarius.UI.Gtk_Text is

   function Get_Tag_Entry (Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
                           Name   : String;
                           Style  : Aquarius.Styles.Aquarius_Style)
                          return Gtk.Text_Tag.Gtk_Text_Tag;

   function Default_Tag_Entry (TB    : Gtk.Text_Buffer.Gtk_Text_Buffer)
                              return Gtk.Text_Tag.Gtk_Text_Tag;

   function Default_Font return Pango.Font.Pango_Font_Description;

   procedure Initialise;

end Aquarius.UI.Gtk_Text;
