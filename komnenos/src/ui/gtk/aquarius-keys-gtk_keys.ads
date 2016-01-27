with Gdk.Types;

package Aquarius.Keys.Gtk_Keys is

   function To_Aquarius_Key
     (Key   : Gdk.Types.Gdk_Key_Type;
      State : Gdk.Types.Gdk_Modifier_Type)
      return Aquarius_Key;

end Aquarius.Keys.Gtk_Keys;
