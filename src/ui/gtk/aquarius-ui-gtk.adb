with Aquarius.UI.Gtk_UI;

package body Aquarius.UI.Gtk is

   -------------------
   -- Create_Gtk_UI --
   -------------------

   function Create_Gtk_UI
      return Aquarius_UI
   is
   begin
      return Result : constant Aquarius_UI := new Aquarius.UI.Gtk_UI.Gtk_UI do
         null;
      end return;
   end Create_Gtk_UI;

end Aquarius.UI.Gtk;
