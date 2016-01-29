with Gtk.Widget;

private with Gtk.Scrolled_Window;
private with Gtk.Text_Buffer;
private with Gtk.Text_View;
private with Aquarius.Styles;

with Komnenos.Fragments;

package Komnenos.UI.Gtk_UI.Text is

   type Komnenos_Text_View_Record is
     new Gtk.Widget.Gtk_Widget_Record with private;

   type Komnenos_Text_View is access all Komnenos_Text_View_Record'Class;

   function Create_Text_View
     (Fragment : Komnenos.Fragments.Fragment_Type)
      return Komnenos_Text_View;

private

   type Komnenos_Text_View_Record is
     new Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
      record
         Text              : Gtk.Text_View.Gtk_Text_View;
         Buffer            : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Hover_Start       : Natural  := 0;
         Hover_Finish      : Natural := 0;
         Hover_Style       : Aquarius.Styles.Aquarius_Style;
         Fragment          : Komnenos.Fragments.Fragment_Type;
      end record;

end Komnenos.UI.Gtk_UI.Text;
