with Gdk.Event;

with Cairo.Image_Surface;

with Aquarius.Colours.Gtk_Colours;

package body Komnenos.UI.Gtk_UI.Navigation is

   Vertical_Scale : constant := 3.0;
   --  three main screens fit vertical in navigator

   Maximum_Widget_Dimension : constant := 32767.0;

   function On_Configure_Navigation
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Configure)
      return Boolean;

   function On_Draw_Navigation
     (Object : access Glib.Object.GObject_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean;

   function On_Click_Navigation
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button)
      return Boolean;

   ---------------------
   -- Calculate_Scale --
   ---------------------

   procedure Calculate_Scale
     (Navigator : in out Root_Gtk_Navigation_Panel'Class)
   is
      use Glib;
      Nav_Width     : constant Gdouble :=
                        Gdouble (Navigator.Size.Width);
      Nav_Height    : constant Gdouble :=
                        Gdouble (Navigator.Size.Height);
      Layout_Height : constant Gdouble :=
                        Gdouble (Navigator.Layout.Visible_Height);
      Scale         : constant Gdouble :=
                        Gdouble'Min
                          (Vertical_Scale * Layout_Height / Nav_Height,
                           Maximum_Widget_Dimension / Nav_Width);
   begin
      Navigator.Scale := Float (Scale);
   end Calculate_Scale;

   -----------------------------
   -- Create_Navigation_Panel --
   -----------------------------

   function Create_Navigation_Panel
     (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk_Navigation_Panel
   is
      Result : constant Gtk_Navigation_Panel :=
                 new Root_Gtk_Navigation_Panel;
   begin
      Glib.Object.Initialize (Result);
      Result.Widget := Gtk.Widget.Gtk_Widget (Widget);
      Result.Layout := null;
      Result.Surface := Cairo.Null_Surface;

      Result.Widget.On_Configure_Event
        (On_Configure_Navigation'Access, Result);
      Result.Widget.On_Draw
        (On_Draw_Navigation'Access, Result);

      declare
         use Gdk.Event;
      begin
         Result.Widget.Add_Events
           (Button_Press_Mask or Button_Release_Mask);
      end;

      Result.Widget.On_Button_Release_Event
        (On_Click_Navigation'Access, Result);

      return Result;
   end Create_Navigation_Panel;

   -------------------------
   -- On_Click_Navigation --
   -------------------------

   function On_Click_Navigation
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      use Glib;
      Navigator     : constant Gtk_Navigation_Panel :=
                        Gtk_Navigation_Panel (Object);
      Layout_Width : constant Gdouble :=
                        Gdouble (Navigator.Layout.Visible_Width);
      Layout_Height : constant Gdouble :=
                        Gdouble (Navigator.Layout.Visible_Height);
      Scale         : constant Gdouble := Gdouble (Navigator.Scale);
      Scaled_Visible_Width : constant Gdouble :=
                               Layout_Width / Scale;
      Scaled_Visible_Height : constant Gdouble :=
                                Layout_Height / Scale;
      New_Left              : constant Gdouble :=
                                Event.X - Scaled_Visible_Width / 2.0;
      New_Top               : constant Gdouble :=
                                Event.Y - Scaled_Visible_Height / 2.0;
   begin
      Navigator.Cb_Navigate (Navigator.Layout,
                             Integer'Max (Integer (New_Left * Scale), 0),
                             Integer'Max (Integer (New_Top * Scale), 0));
      Navigator.Left := New_Left;
      Navigator.Top  := New_Top;
      Navigator.Refresh;

      return True;
   end On_Click_Navigation;

   -----------------------------
   -- On_Configure_Navigation --
   -----------------------------

   function On_Configure_Navigation
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      pragma Unreferenced (Event);
      use Glib;
      use type Cairo.Cairo_Surface;
      use type Komnenos.Layouts.Layout_Type;
      Navigator : constant Gtk_Navigation_Panel :=
                    Gtk_Navigation_Panel (Object);
   begin
      if Navigator.Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (Navigator.Surface);
      end if;

      Navigator.Widget.Get_Allocation (Navigator.Size);

      Navigator.Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32,
           Navigator.Size.Width, Navigator.Size.Height);

      Navigator.Calculate_Scale;

      if Navigator.Layout /= null then
         Navigator.On_Layout_Configured;
      end if;

      Navigator.Refresh;
      return False;
   end On_Configure_Navigation;

   ------------------------
   -- On_Draw_Navigation --
   ------------------------

   function On_Draw_Navigation
     (Object : access Glib.Object.GObject_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean
   is
      use Glib;
      Navigator : constant Gtk_Navigation_Panel :=
                    Gtk_Navigation_Panel (Object);
   begin
      Cairo.Set_Source_Surface (Cr, Navigator.Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      return True;
   end On_Draw_Navigation;

   --------------------------
   -- On_Layout_Configured --
   --------------------------

   procedure On_Layout_Configured
     (Panel : in out Root_Gtk_Navigation_Panel'Class)
   is
   begin
      Panel.Calculate_Scale;
      Panel.Layout.Set_Full_Size
        (Full_Width  =>
           Natural (Float (Panel.Size.Width) * Panel.Scale),
         Full_Height =>
           Natural (Float (Panel.Size.Height) * Panel.Scale));
   end On_Layout_Configured;

   -----------------
   -- On_Navigate --
   -----------------

   procedure On_Navigate
     (Panel    : in out Root_Gtk_Navigation_Panel'Class;
      Callback : Navigation_Callback)
   is
   begin
      Panel.Cb_Navigate := Callback;
   end On_Navigate;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Panel : in out Root_Gtk_Navigation_Panel'Class)
   is
      use Glib;
      Cr : constant Cairo.Cairo_Context :=
             Cairo.Create (Panel.Surface);

      Scale : constant Gdouble := Gdouble (1.0 / Panel.Scale);

      procedure Draw_Fragment
        (Fragment : Komnenos.Fragments.Fragment_Type);

      -------------------
      -- Draw_Fragment --
      -------------------

      procedure Draw_Fragment
        (Fragment : Komnenos.Fragments.Fragment_Type)
      is
         X        : constant Gdouble :=
                      Gdouble (Fragment.X) * Scale;
         Y        : constant Gdouble :=
                      Gdouble (Fragment.Y) * Scale;
         W        : constant Gdouble :=
                      Gdouble (Fragment.Width) * Scale;
         H        : constant Gdouble :=
                      Gdouble (Fragment.Height) * Scale;

         Colour   : constant Gdk.RGBA.Gdk_RGBA :=
                      Aquarius.Colours.Gtk_Colours.To_Gdk_RGBA
                        (Fragment.Background_Colour);
      begin
         Cairo.Set_Source_Rgb (Cr, Colour.Red, Colour.Green, Colour.Blue);
         Cairo.Rectangle (Cr, X, Y, W, H);
         Cairo.Fill (Cr);
      end Draw_Fragment;

   begin
      Cairo.Set_Source_Rgb (Cr, 0.8, 0.8, 0.8);
      Cairo.Rectangle
        (Cr, 0.0, 0.0,
         Gdouble (Panel.Size.Width),
         Gdouble (Panel.Size.Height));
      Cairo.Fill (Cr);

      Panel.Layout.Scan (Draw_Fragment'Access);

      Cairo.Rectangle
        (Cr, Panel.Left, Panel.Top,
         Gdouble (Panel.Layout.Visible_Width) * Scale,
         Gdouble (Panel.Layout.Visible_Height) * Scale);
      Cairo.Set_Source_Rgba (Cr, 0.1, 0.1, 0.6, 0.2);
      Cairo.Fill_Preserve (Cr);
      Cairo.Set_Source_Rgb (Cr, 0.1, 0.1, 0.8);
      Cairo.Stroke (Cr);

      Cairo.Destroy (Cr);

      Panel.Widget.Queue_Draw;
   end Refresh;

   ----------------
   -- Set_Layout --
   ----------------

   procedure Set_Layout
     (Panel  : in out Root_Gtk_Navigation_Panel'Class;
      Layout : not null access Komnenos.Layouts.Root_Layout_Type'Class)
   is
   begin
      Panel.Layout := Komnenos.Layouts.Layout_Type (Layout);
   end Set_Layout;

end Komnenos.UI.Gtk_UI.Navigation;
