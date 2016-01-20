with Ada.Numerics;

with Glib;
with Gdk.Event;
with Gdk.Window;

package body Komnenos.UI.Gtk_UI.Borders is

   procedure Clear
     (Surface       : Cairo.Cairo_Surface);

   function Configure_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Configure)
      return Boolean;

   function Draw_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean;

   type Border_Widget is access all Border_Widget_Record'Class;

   type Corner_Widget_Record is new Border_Widget_Record with
      record
         Corner : Border_Corner;
      end record;

   overriding procedure Draw_Border
     (Corner : in out Corner_Widget_Record);

   type Corner_Widget is access all Corner_Widget_Record'Class;

   type Edge_Widget_Record is new Border_Widget_Record with
      record
         Edge : Border_Edge;
      end record;

   overriding procedure Draw_Border
     (Edge : in out Edge_Widget_Record);

   type Edge_Widget is access all Edge_Widget_Record'Class;

   function New_Corner_Widget
     (Corner : Border_Corner;
      Colour : Gdk.RGBA.Gdk_RGBA)
     return Gtk.Widget.Gtk_Widget;

   function New_Edge_Widget
     (Edge   : Border_Edge;
      Colour : Gdk.RGBA.Gdk_RGBA)
     return Gtk.Widget.Gtk_Widget;

   -----------------
   -- Add_Borders --
   -----------------

   procedure Add_Borders
     (Fragment : in out UI_Fragment_Interface'Class)
   is
      use Gtk.Drawing_Area;
   begin
      for Corner in Border_Corner loop
         Fragment.Set_Corner_Widget
           (Corner, New_Corner_Widget (Corner, Fragment.Border_Colour));
      end loop;

      for Edge in Border_Edge loop
         Fragment.Set_Side_Widget
           (Edge, New_Edge_Widget (Edge, Fragment.Border_Colour));
      end loop;

   end Add_Borders;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Surface       : Cairo.Cairo_Surface)
   is
      Cr : constant Cairo.Cairo_Context := Cairo.Create (Surface);
   begin
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Clear);
      Cairo.Paint (Cr);
      Cairo.Destroy (Cr);
   end Clear;

   ------------------
   -- Configure_Cb --
   ------------------

   function Configure_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      pragma Unreferenced (Event);
      use type Cairo.Cairo_Surface;
      Border : constant Border_Widget := Border_Widget (Widget);
   begin
      if Border.Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (Border.Surface);
      end if;

      Border.Surface :=
        Gdk.Window.Create_Similar_Surface
          (Widget.Get_Window,
           Cairo.Cairo_Content_Color_Alpha,
           Widget.Get_Allocated_Width,
           Widget.Get_Allocated_Height);

      Clear (Border.Surface);

      Border.Draw_Border;

      return False;

   end Configure_Cb;

   -----------------
   -- Draw_Border --
   -----------------

   overriding procedure Draw_Border
     (Corner : in out Corner_Widget_Record)
   is
      use Glib;
      Width  : constant Gint := Corner.Get_Allocated_Width;
      Height : constant Gint := Corner.Get_Allocated_Height;
      Cr     : constant Cairo.Cairo_Context :=
                 Cairo.Create (Corner.Surface);
      Pi     : constant Gdouble := Ada.Numerics.Pi;
      X, Y   : Gdouble;
      A1, A2 : Gdouble;
      R      : constant Gdouble := Gdouble (Gint'Min (Width, Height));
   begin
      Cairo.Set_Source_Rgb
        (Cr, Corner.Colour.Red, Corner.Colour.Green, Corner.Colour.Blue);
      case Corner.Corner is
         when Top_Left =>
            X := Gdouble (Width);
            Y := Gdouble (Height);
            A1 := Pi / 2.0;
            A2 := 3.0 * Pi / 2.0;
         when Top_Right =>
            X := 0.0;
            Y := Gdouble (Height);
            A2 := 0.0;
            A1 := Pi / 2.0;
         when Bottom_Left =>
            X := Gdouble (Width);
            Y := 0.0;
            A1 := 3.0 * Pi / 2.0;
            A2 := Pi;
         when Bottom_Right =>
            X := 0.0;
            Y := 0.0;
            A1 := 2.0 * Pi;
            A2 := 3.0 * Pi / 2.0;
      end case;

      if False then
         Cairo.Rectangle (Cr, 0.0, 0.0, Gdouble (Width), Gdouble (Height));
      else
         Cairo.Move_To (Cr, X, Y);
         Cairo.Arc (Cr, X, Y, R, A1, A2);
      end if;
      Cairo.Fill (Cr);

      Cairo.Destroy (Cr);
   end Draw_Border;

   -----------------
   -- Draw_Border --
   -----------------

   overriding procedure Draw_Border
     (Edge : in out Edge_Widget_Record)
   is
      use Glib;
      Width  : constant Gint := Edge.Get_Allocated_Width;
      Height : constant Gint := Edge.Get_Allocated_Height;
      Cr     : constant Cairo.Cairo_Context :=
                 Cairo.Create (Edge.Surface);
   begin
      Cairo.Set_Source_Rgb
        (Cr, Edge.Colour.Red, Edge.Colour.Green, Edge.Colour.Blue);
      Cairo.Rectangle (Cr, 0.0, 0.0, Gdouble (Width), Gdouble (Height));
      Cairo.Fill (Cr);

      Cairo.Destroy (Cr);
   end Draw_Border;

   -------------
   -- Draw_Cb --
   -------------

   function Draw_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean
   is
      Border : constant Border_Widget := Border_Widget (Widget);
   begin
      Cairo.Set_Source_Surface (Cr, Border.Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      return False;
   end Draw_Cb;

   -----------------------
   -- New_Corner_Widget --
   -----------------------

   function New_Corner_Widget
     (Corner : Border_Corner;
      Colour : Gdk.RGBA.Gdk_RGBA)
      return Gtk.Widget.Gtk_Widget
   is
      Result : constant Corner_Widget := new Corner_Widget_Record;
   begin
      Gtk.Drawing_Area.Initialize (Result);
      Result.Corner := Corner;
      Result.Colour := Colour;
      Result.Surface := Cairo.Null_Surface;
      Result.On_Configure_Event (Configure_Cb'Access);
      Result.On_Draw (Draw_Cb'Access);
      return Gtk.Widget.Gtk_Widget (Result);
   end New_Corner_Widget;

   ---------------------
   -- New_Edge_Widget --
   ---------------------

   function New_Edge_Widget
     (Edge   : Border_Edge;
      Colour : Gdk.RGBA.Gdk_RGBA)
      return Gtk.Widget.Gtk_Widget
   is
      Result : constant Edge_Widget := new Edge_Widget_Record;
   begin
      Gtk.Drawing_Area.Initialize (Result);
      Result.Edge := Edge;
      Result.Colour := Colour;
      Result.Surface := Cairo.Null_Surface;
      Result.On_Configure_Event (Configure_Cb'Access);
      Result.On_Draw (Draw_Cb'Access);
      return Gtk.Widget.Gtk_Widget (Result);
   end New_Edge_Widget;

end Komnenos.UI.Gtk_UI.Borders;
