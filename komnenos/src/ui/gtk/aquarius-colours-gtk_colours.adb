package body Aquarius.Colours.Gtk_Colours is

   -----------------
   -- To_Gdk_RGBA --
   -----------------

   function To_Gdk_RGBA
     (Colour : Aquarius_Colour)
      return Gdk.RGBA.Gdk_RGBA
   is
      Result     : Gdk.RGBA.Gdk_RGBA;
      Got_Colour : Boolean;

   begin
      Gdk.RGBA.Parse
        (Result, To_String (Colour), Got_Colour);

      if not Got_Colour then
         Result := (1.0, 0.0, 1.0, 1.0);
      end if;

      return Result;
   end To_Gdk_RGBA;

end Aquarius.Colours.Gtk_Colours;
