with Gdk.Types.Keysyms;

package body Aquarius.Keys.Gtk_Keys is

   ---------------------
   -- To_Aquarius_Key --
   ---------------------

   function To_Aquarius_Key
     (Key   : Gdk.Types.Gdk_Key_Type;
      State : Gdk.Types.Gdk_Modifier_Type)
      return Aquarius_Key
   is
      use Gdk.Types;
      use Gdk.Types.Keysyms;
      Result : Aquarius_Key := Null_Key;
   begin
      case Key is
         when 20 .. 126 =>
            Result := Character_Key (Character'Val (Key));
         when GDK_BackSpace =>
            Result := Back_Space;
         when GDK_Linefeed =>
            Result := Line_Feed;
         when GDK_Tab =>
            Result := Tab;
         when GDK_Up =>
            Result := Up_Arrow;
         when GDK_Down =>
            Result := Down_Arrow;
         when GDK_Left =>
            Result := Left_Arrow;
         when GDK_Right =>
            Result := Right_Arrow;
         when others =>
            Result := Null_Key;
      end case;

      return Modify (Result,
                     Shift   => (State and Gdk.Types.Shift_Mask) /= 0,
                     Control => (State and Gdk.Types.Control_Mask) /= 0,
                     Mod1    => (State and Gdk.Types.Mod1_Mask) /= 0,
                     Mod2    => (State and Gdk.Types.Mod2_Mask) /= 0);

   end To_Aquarius_Key;

end Aquarius.Keys.Gtk_Keys;
