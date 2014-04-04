with Gdk.Color;

with Gtk.Text_Buffer;
with Gtk.Text_Tag;

with Pango.Font;

with Aquarius.Fonts;
with Aquarius.Styles;

package Aquarius.GUI.Text is

   function Get_Tag_Entry (Buffer     : Gtk.Text_Buffer.Gtk_Text_Buffer;
                           Name       : String;
                           Style      : Aquarius.Styles.Aquarius_Style)
                          return Gtk.Text_Tag.Gtk_Text_Tag;

   function Default_Tag_Entry (TB    : Gtk.Text_Buffer.Gtk_Text_Buffer)
                              return Gtk.Text_Tag.Gtk_Text_Tag;

   function Get_Gdk_Colour (Colour   : Aquarius.Fonts.Aquarius_Colour)
                           return Gdk.Color.Gdk_Color;

   function Default_Font return Pango.Font.Pango_Font_Description;

   procedure Initialise;

end Aquarius.GUI.Text;
