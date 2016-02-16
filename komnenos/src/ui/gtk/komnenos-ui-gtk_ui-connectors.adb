with Glib;

with Gdk.Event;
with Gdk.Window;

package body Komnenos.UI.Gtk_UI.Connectors is

   Arrow_Length : constant := 8.0;
   Arrow_Width  : constant := 4.0;

   function Configure_Connector
     (Connector : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event     : Gdk.Event.Gdk_Event_Configure)
      return Boolean;

   function Draw_Connector
     (Connector : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr        : Cairo.Cairo_Context)
      return Boolean;

   -------------------------
   -- Configure_Connector --
   -------------------------

   function Configure_Connector
     (Connector : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event     : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      pragma Unreferenced (Event);
   begin

      Gtk_Connector (Connector).Render;

      return False;

   end Configure_Connector;

   ---------------
   -- Connector --
   ---------------

   function Connector
     (UI_Connector : Root_Gtk_Connector_Record'Class)
      return Komnenos.Connectors.Connector_Type
   is
   begin
      return UI_Connector.Connector;
   end Connector;

   --------------------
   -- Draw_Connector --
   --------------------

   function Draw_Connector
     (Connector : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr        : Cairo.Cairo_Context)
      return Boolean
   is
   begin
      Cairo.Set_Source_Surface
        (Cr, Gtk_Connector (Connector).Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      return True;
   end Draw_Connector;

   ---------------------
   -- Layout_Location --
   ---------------------

   function Layout_Location
     (UI_Connector : Root_Gtk_Connector_Record'Class)
      return Layout_Point
   is
   begin
      return Loc : Layout_Point :=
        (UI_Connector.Connector.Layout_Boundary.X,
         UI_Connector.Connector.Layout_Boundary.Y)
      do
         Loc.X := Loc.X - 8;
         Loc.Y := Loc.Y - 16;
      end return;
   end Layout_Location;

   -------------------
   -- New_Connector --
   -------------------

   function New_Connector
     (From : Komnenos.Connectors.Connector_Type)
      return Gtk_Connector
   is
      Result : constant Gtk_Connector := new Root_Gtk_Connector_Record;
   begin
      Gtk.Drawing_Area.Initialize (Result);
      From.Set_Display (Result);
      Result.On_Configure_Event (Configure_Connector'Access);
      Result.On_Draw (Draw_Connector'Access);
      Result.Connector := From;
      Result.Surface := Cairo.Null_Surface;
      Result.Set_Size_Request
        (Width  => Glib.Gint (From.Layout_Boundary.Width + 16),
         Height => Glib.Gint (From.Layout_Boundary.Height + 32));
      Result.Show_All;
      return Result;
   end New_Connector;

   ------------
   -- Render --
   ------------

   procedure Render
     (Con : in out Root_Gtk_Connector_Record'Class)
   is
      use type Cairo.Cairo_Surface;
   begin
      if Con.Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (Con.Surface);
      end if;

      Con.Surface :=
        Gdk.Window.Create_Similar_Surface
          (Self    => Con.Get_Window,
           Content => Cairo.Cairo_Content_Color_Alpha,
           Width   => Con.Get_Allocated_Width,
           Height  => Con.Get_Allocated_Height);

      declare
         Cr : constant Cairo.Cairo_Context :=
                Cairo.Create (Con.Surface);
      begin
         Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Clear);
         Cairo.Paint (Cr);
         Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Over);
         Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
         declare
            use Glib;
            Boundary : constant Layout_Rectangle :=
                         Con.Connector.Layout_Boundary;
            Line     : constant Layout_Line := Con.Connector.Layout_Path;
            PX, PY   : Gdouble := 0.0;
         begin
            Cairo.Set_Line_Width (Cr, 2.0);
            Cairo.Set_Line_Cap (Cr, Cairo.Cairo_Line_Cap_Butt);
            Cairo.Set_Line_Join (Cr, Cairo.Cairo_Line_Join_Round);

            for I in Line'Range loop
               declare
                  X : constant Glib.Gdouble :=
                        Glib.Gdouble (Line (I).X - Boundary.X + 8);
                  Y : constant Glib.Gdouble :=
                        Glib.Gdouble (Line (I).Y - Boundary.Y + 16);
               begin
                  if I = Line'First then
                     Cairo.Move_To (Cr, X, Y);
                  else
                     Cairo.Line_To (Cr, X, Y);

                     if I = Line'Last then
                        if PX < X then
                           Cairo.Line_To
                             (Cr, X - Arrow_Length, Y - Arrow_Width);
                           Cairo.Move_To (Cr, X, Y);
                           Cairo.Line_To
                             (Cr, X - Arrow_Length, Y + Arrow_Width);
                        elsif PY < X then
                           Cairo.Line_To
                             (Cr, X - Arrow_Width, Y - Arrow_Length);
                           Cairo.Move_To (Cr, X, Y);
                           Cairo.Line_To
                             (Cr, X + Arrow_Width, Y - Arrow_Length);
                        end if;
                     end if;
                  end if;
                  PX := X;
                  PY := Y;
               end;
            end loop;

            Cairo.Stroke (Cr);
         end;

         Cairo.Destroy (Cr);
      end;

   end Render;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Con : in out Root_Gtk_Connector_Record)
   is
   begin
      Con.Set_Size_Request
        (Width  => Glib.Gint (Con.Connector.Layout_Boundary.Width + 16),
         Height => Glib.Gint (Con.Connector.Layout_Boundary.Height + 32));
      Con.Show_All;
   end Update;

end Komnenos.UI.Gtk_UI.Connectors;
