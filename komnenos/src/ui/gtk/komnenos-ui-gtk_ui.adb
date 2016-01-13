with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics;
--  with Ada.Text_IO;

with Tropos;

with Komnenos.Connectors;
with Komnenos.Layouts;

with Komnenos.UI.Gtk_UI.Entity_Lists;
with Komnenos.UI.Gtk_UI.Borders;
with Komnenos.UI.Gtk_UI.Text;

with Glib.Error;

with Gdk.Cairo;
with Gdk.Color;
with Gdk.Event;
with Gdk.Pixbuf;

with Gtk.Builder;
with Gtk.Drawing_Area;
with Gtk.Enums;
with Gtk.Event_Box;
with Gtk.Label;
with Gtk.Layout;
with Gtk.GEntry;
with Gtk.Grid;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Style_Provider;
with Gtk.Tree_View;
with Gtk.Widget;
with Gtk.Window;

with Gtkada.Style;

with Cairo;
with Cairo.Image_Surface;
with Cairo.Pattern;

package body Komnenos.UI.Gtk_UI is

   Title_Pixel_Height : constant := 10;

   type Root_Gtk_UI;

   type Root_Gtk_Layout is
     new Komnenos.Layouts.Root_Layout_Type with
      record
         UI : access Root_Gtk_UI'Class;
      end record;

   overriding procedure Item_Moved
     (Layout : in out Root_Gtk_Layout;
      Item   : Komnenos.Fragments.Fragment_Type);

   overriding procedure Item_Placed
     (Layout : in out Root_Gtk_Layout;
      Item   : Komnenos.Fragments.Fragment_Type);

   type Layout_Widget_Record is
     new Komnenos.UI.Gtk_UI.Borders.UI_Fragment_Interface with
      record
         Widget       : Gtk.Widget.Gtk_Widget;
         Grid         : Gtk.Grid.Gtk_Grid;
         Display      : access Display_Interface'Class;
         Title        : Gtk.Widget.Gtk_Widget;
         Fragment     : Komnenos.Fragments.Fragment_Type;
         Background   : Gdk.Color.Gdk_Color;
         Border       : Gdk.RGBA.Gdk_RGBA;
         Show_Border  : Boolean;
         Grab_Focus   : Boolean;
         Dragging     : Boolean := False;
         Start_X      : Glib.Gdouble;
         Start_Y      : Glib.Gdouble;
      end record;

   overriding function Border_Colour
     (Fragment : Layout_Widget_Record)
      return Gdk.RGBA.Gdk_RGBA
   is (Fragment.Border);

   overriding procedure Set_Corner_Widget
     (Fragment  : in out Layout_Widget_Record;
      Corner    : Borders.Border_Corner;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);

   overriding procedure Set_Side_Widget
     (Fragment  : in out Layout_Widget_Record;
      Edge      : Borders.Border_Edge;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);

   type Layout_Widget_Access is access Layout_Widget_Record;

   package Layout_Widget_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Layout_Widget_Access);

   package Connector_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Komnenos.Connectors.Connector_Type,
        Komnenos.Connectors."=");

   type Root_Gtk_UI is
     new Root_Komnenos_UI with
      record
         Top_Level       : Gtk.Window.Gtk_Window;
         Main_View       : Gtk.Layout.Gtk_Layout;
         Main_Surface    : Cairo.Cairo_Surface;
         Entity_Tree     : Gtk.Tree_View.Gtk_Tree_View;
         Entity_Filter   : Gtk.GEntry.Gtk_Entry;
         Navigation      : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Nav_Surface     : Cairo.Cairo_Surface;
         Layout          : access Root_Gtk_Layout'Class;
         Widgets         : Layout_Widget_Lists.List;
         Connectors      : Connector_Lists.List;
         Active          : Komnenos.Fragments.Fragment_Type := null;
         Dragging        : Layout_Widget_Access := null;
         Vertical_Scale  : Float := 3.0;  --  three main views fit vertically
                                          --  into the navigator
      end record;

   type Gtk_UI_Access is access all Root_Gtk_UI'Class;

   Current_UI : Gtk_UI_Access;

   overriding function Name (UI : Root_Gtk_UI) return String
   is ("Komnenos UI");

   overriding procedure Start (UI : in out Root_Gtk_UI);

   overriding procedure Place_Fragment
     (UI       : in out Root_Gtk_UI;
      Parent   : access Komnenos.Entities.Entity_Visual'Class;
      Offset   : Natural;
      Fragment : Komnenos.Fragments.Fragment_Type);

   overriding function Active_Fragment
     (UI : Root_Gtk_UI)
      return Komnenos.Fragments.Fragment_Type
   is (UI.Active);

   overriding procedure To_Config
     (UI     : Root_Gtk_UI;
      Config : in out Tropos.Configuration);

   overriding procedure From_Config
     (UI : not null access Root_Gtk_UI;
      Config : Tropos.Configuration);

   procedure Create_Fragment_Widget
     (Fragment : Komnenos.Fragments.Fragment_Type;
      UI       : not null access Root_Gtk_UI'Class;
      Top      : out Gtk.Widget.Gtk_Widget;
      Grid     : out Gtk.Grid.Gtk_Grid;
      Display  : out Komnenos_Display;
      Title    : out Gtk.Widget.Gtk_Widget);

   function Find_Fragment_Widget
     (Fragment : Komnenos.Fragments.Fragment_Type;
      UI       : not null access Root_Gtk_UI'Class)
      return Gtk.Widget.Gtk_Widget;

--     function Find_Layout_Record_By_Display
--       (Display : not null access Gtk.Widget.Gtk_Widget_Record'Class;
--        UI      : not null access Root_Gtk_UI'Class)
--        return Layout_Widget_Access;

   function Find_Layout_Record_By_Title
     (Title   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      UI      : not null access Root_Gtk_UI'Class)
      return Layout_Widget_Access;

--     package Fragment_Return_Callback is
--       new Gtk.Handlers.User_Return_Callback
--         (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
--          User_Type   => Komnenos.Fragments.Fragment_Type,
--          Return_Type => Boolean);
--
--     function Configure_Fragment_Handler
--       (W        : access Gtk.Widget.Gtk_Widget_Record'Class;
--        Fragment : Komnenos.Fragments.Fragment_Type)
--        return Boolean;

   package Window_Callback is
      new Gtk.Handlers.Callback (Gtk.Window.Gtk_Window_Record);

   procedure Destroy_Handler (W : access Gtk.Window.Gtk_Window_Record'Class);

   package UI_Return_Callback is
     new Gtk.Handlers.User_Return_Callback
       (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
        User_Type   => Gtk_UI_Access,
        Return_Type => Boolean);

   function Configure_Main_View_Handler
     (W  : access Gtk.Widget.Gtk_Widget_Record'Class;
      UI : Gtk_UI_Access)
      return Boolean;

   procedure Draw_Main_View
     (UI : Gtk_UI_Access);

   function Draw_Main_View_Handler
     (W  : access Gtk.Widget.Gtk_Widget_Record'Class;
      UI : Gtk_UI_Access)
      return Boolean;

   function Draw_Navigation_Handler
     (W  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr : Cairo.Cairo_Context)
      return Boolean;

   function Click_Navigation_Handler
     (W     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean;

   function Fragment_Title_Button_Press_Handler
     (W     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean;

   function Fragment_Title_Button_Release_Handler
     (W     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean;

   function Fragment_Title_Motion_Handler
     (W     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Motion)
      return Boolean;

   procedure Update_Fragment_Size
     (Widget   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Fragment : Komnenos.Fragments.Fragment_Type);

   procedure Update_Fragment_Positions
     (UI : Gtk_UI_Access);

   procedure Update_Fragment_Position
     (UI   : Gtk_UI_Access;
      Item : Komnenos.Fragments.Fragment_Type);

   procedure Draw_Rounded_Rectangle
     (Context       : Cairo.Cairo_Context;
      X, Y          : Glib.Gdouble;
      Width, Height : Glib.Gdouble;
      Radius        : Glib.Gdouble) with Unreferenced;

   ------------------------------
   -- Click_Navigation_Handler --
   ------------------------------

   function Click_Navigation_Handler
     (W      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      pragma Unreferenced (W);
      UI       : constant Gtk_UI_Access :=
                   Current_UI;
      Nav_Size : Gtk.Widget.Gtk_Allocation;
      Height : Glib.Gdouble;
   begin
      UI.Navigation.Get_Allocation (Nav_Size);
      Height := Glib.Gdouble (Nav_Size.Height);

      declare
         use Glib;
         Scale  : constant Gdouble :=
                    Height / Gdouble (UI.View_Height)
                    / Gdouble (UI.Vertical_Scale);
      begin
         UI.View_Left := Integer (Event.X / Scale) - UI.View_Width / 2;
         UI.View_Top  := Integer (Event.Y / Scale) - UI.View_Height / 2;
         if UI.View_Top < 0 then
            UI.View_Top := 0;
         end if;
      end;

      Update_Fragment_Positions (UI);

      UI.Navigation.Queue_Draw;

      return True;
   end Click_Navigation_Handler;

   --------------------------------
   -- Configure_Fragment_Handler --
   --------------------------------

--     function Configure_Fragment_Handler
--       (W        : access Gtk.Widget.Gtk_Widget_Record'Class;
--        Fragment : Komnenos.Fragments.Fragment_Type)
--        return Boolean
--     is
--     begin
--        Update_Fragment_Size (W, Fragment);
--        return True;
--     end Configure_Fragment_Handler;

   ---------------------------------
   -- Configure_Main_View_Handler --
   ---------------------------------

   function Configure_Main_View_Handler
     (W  : access Gtk.Widget.Gtk_Widget_Record'Class;
      UI : Gtk_UI_Access)
      return Boolean
   is
      Size : Gtk.Widget.Gtk_Allocation;
   begin
      W.Get_Allocation (Size);
      UI.View_Height := Positive (Size.Height);
      UI.View_Width := Positive (Size.Width);
      UI.Navigation.Queue_Draw;
      return True;
   end Configure_Main_View_Handler;

   ----------------------------
   -- Create_Fragment_Widget --
   ----------------------------

   procedure Create_Fragment_Widget
     (Fragment : Komnenos.Fragments.Fragment_Type;
      UI       : not null access Root_Gtk_UI'Class;
      Top      : out Gtk.Widget.Gtk_Widget;
      Grid     : out Gtk.Grid.Gtk_Grid;
      Display  : out Komnenos_Display;
      Title    : out Gtk.Widget.Gtk_Widget)
   is
      Text   : constant Komnenos.UI.Gtk_UI.Text.Komnenos_Text_View :=
                 Komnenos.UI.Gtk_UI.Text.Create_Text_View
                   (Fragment);
      Label  : Gtk.Label.Gtk_Label;
      Events : Gtk.Event_Box.Gtk_Event_Box;

   begin
      Gtk.Event_Box.Gtk_New (Events);
      Gtk.Label.Gtk_New (Label, Fragment.Title);
      Events.Add (Label);

      Label.Override_Background_Color
        (Gtk.Enums.Gtk_State_Flag_Normal,
         To_RGBA (Fragment.Background_Colour));

      declare
         use Gdk.Event;
      begin
         Label.Add_Events
           (Button_Press_Mask or Button_Release_Mask or Pointer_Motion_Mask);
      end;

      Events.On_Button_Press_Event
        (Fragment_Title_Button_Press_Handler'Access);

      Events.On_Button_Release_Event
        (Fragment_Title_Button_Release_Handler'Access);

      Events.On_Motion_Notify_Event
        (Fragment_Title_Motion_Handler'Access);

--        Gtk.Scrolled_Window.Gtk_New (Scroll);

      Gtk.Grid.Gtk_New (Grid);

      Grid.Attach (Events,
                   Left   => 1,
                   Top    => 1,
                   Width  => 1,
                   Height => 1);
      Grid.Attach (Text,
                   Left   => 1,
                   Top    => 2,
                   Width  => 1,
                   Height => 1);

      Top := Gtk.Widget.Gtk_Widget (Grid);
      Display := Komnenos_Display (Text);
      Title := Gtk.Widget.Gtk_Widget (Events);

      UI.Main_View.Put (Top,
                        Glib.Gint (Fragment.X - UI.View_Left),
                        Glib.Gint (Fragment.Y - UI.View_Top));

--        declare
--           Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
--        begin
--           Text.Get_Buffer.Get_Start_Iter (Start_Iter);
--           Text.Get_Buffer.Place_Cursor (Start_Iter);
--        end;

   end Create_Fragment_Widget;

   ---------------
   -- Create_UI --
   ---------------

   function Create_UI
     (Config_Folder_Path : String)
      return Komnenos_UI
   is
      Result : constant Gtk_UI_Access := new Root_Gtk_UI;
      Builder : Gtk.Builder.Gtk_Builder;
   begin

      Current_UI := Result;

      Result.View_Left := 0;
      Result.View_Top := 0;
      Result.View_Width := 400;
      Result.View_Height := 400;

      Result.Layout := new Root_Gtk_Layout;
      Result.Layout.UI := Result;

      Gtk.Main.Init;

      declare
         Path  : constant String :=
                   Config_Folder_Path & "/komnenos.css";

         procedure Error (Message : String);

         -----------
         -- Error --
         -----------

         procedure Error (Message : String) is
         begin
            raise Program_Error with "Could not load " & Path & ": " & Message;
         end Error;

      begin
         Gtkada.Style.Load_Css_File
           (Path     => Path,
            Error    => Error'Access,
            Priority => Gtk.Style_Provider.Priority_Application);
      end;

      Gtk.Builder.Gtk_New (Builder);

      declare
         use type Glib.Guint;
         Path  : constant String :=
                   Config_Folder_Path & "/codetable.ui";
         Error : aliased Glib.Error.GError;
         Result : constant Glib.Guint :=
                    Builder.Add_From_File
                      (Filename => Path,
                       Error    => Error'Access);
      begin
         if Result = 0 then
            raise Program_Error with
              "Error opening GUI definition: " & Path
              & ": " & Glib.Error.Get_Message (Error);
         end if;
      end;

      declare
         Main_Window : constant Gtk.Window.Gtk_Window :=
                         Gtk.Window.Gtk_Window
                           (Builder.Get_Object
                              ("Top_Window"));
         Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf;
         Error       : Glib.Error.GError;
      begin

         Gdk.Pixbuf.Gdk_New_From_File
           (Pixbuf,
            Config_Folder_Path & "/komnenos.png",
            Error);
         Main_Window.Set_Icon (Pixbuf);
         Result.Top_Level := Main_Window;
         Window_Callback.Connect
           (Main_Window,
            "destroy",
            Window_Callback.To_Marshaller (Destroy_Handler'Access));
         Main_Window.Maximize;
      end;

      Result.Entity_Filter :=
        Gtk.GEntry.Gtk_Entry
          (Builder.Get_Object ("Entity_Filter"));

      Result.Entity_Tree :=
        Gtk.Tree_View.Gtk_Tree_View
          (Builder.Get_Object ("Entity_Tree"));

      Result.Main_View :=
        Gtk.Layout.Gtk_Layout
          (Builder.Get_Object ("Main_View"));

      UI_Return_Callback.Connect
        (Result.Main_View, Gtk.Widget.Signal_Configure_Event,
         UI_Return_Callback.To_Marshaller (Configure_Main_View_Handler'Access),
         Result);

      UI_Return_Callback.Connect
        (Result.Main_View, Gtk.Widget.Signal_Draw,
         UI_Return_Callback.To_Marshaller (Draw_Main_View_Handler'Access),
         Result);

      Result.Navigation :=
        Gtk.Drawing_Area.Gtk_Drawing_Area
          (Builder.Get_Object ("Navigation_Pane"));

      declare
         use Gdk.Event;
      begin
         Result.Navigation.Add_Events
           (Button_Press_Mask or Button_Release_Mask);
      end;

      Result.Navigation.On_Draw
        (Draw_Navigation_Handler'Access);

      Result.Navigation.On_Button_Release_Event
        (Click_Navigation_Handler'Access);

      return Komnenos_UI (Result);

   end Create_UI;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Window.Gtk_Window_Record'Class)
   is
      pragma Unreferenced (W);
--        use type Glib.Main.G_Source_Id;
   begin
--        if Timeout_Id /= 0 then
--           Glib.Main.Remove (Timeout_Id);
--        end if;
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   -------------------------
   -- Display_Grabs_Focus --
   -------------------------

   function Display_Grabs_Focus
     (Display : not null access Display_Interface'Class)
      return Boolean
   is
   begin
      for LW of Current_UI.Widgets loop
         if LW.Display = Display then
            if LW.Grab_Focus then
               LW.Grab_Focus := False;
               return True;
            else
               return False;
            end if;
         end if;
      end loop;
      raise Constraint_Error with "orphaned display";
   end Display_Grabs_Focus;

   --------------------
   -- Draw_Main_View --
   --------------------

   procedure Draw_Main_View
     (UI : Gtk_UI_Access)
   is
      use Glib;
      View_Size     : Gtk.Widget.Gtk_Allocation;
      Context       : Cairo.Cairo_Context;
      Surface       : Cairo.Cairo_Surface;
      Pat           : Cairo.Cairo_Pattern;
      Width, Height : Glib.Gdouble;
   begin
      UI.Main_View.Get_Allocation (View_Size);
      UI.View_Width := Positive (View_Size.Width);
      UI.View_Height := Positive (View_Size.Height);

      Width := Glib.Gdouble (View_Size.Width);
      Height := Glib.Gdouble (View_Size.Height);

      Pat :=
        Cairo.Pattern.Create_Linear
          (0.0, 0.0, 0.0, Height);
      Cairo.Pattern.Add_Color_Stop_Rgb
        (Pattern => Pat,
         Offset  => 0.0,
         Red     => 0.95,
         Green   => 0.95,
         Blue    => 0.95);
      Cairo.Pattern.Add_Color_Stop_Rgb
        (Pattern => Pat,
         Offset  => 1.0,
         Red     => 0.7,
         Green   => 0.7,
         Blue    => 0.8);

      Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32,
           View_Size.Width, View_Size.Height);
      Context := Cairo.Create (Surface);

      Cairo.Set_Source (Context, Pat);

      Cairo.Rectangle (Context, 0.0, 0.0, Width, Height);
      Cairo.Fill (Context);

      for Connector of UI.Connectors loop
         declare
            Source : constant Komnenos.Fragments.Fragment_Type :=
                       Komnenos.Fragments.Fragment_Type (Connector.Source);
            Dest   : constant Komnenos.Fragments.Fragment_Type :=
                       Komnenos.Fragments.Fragment_Type
                         (Connector.Destination);
            Source_Offset : constant Integer := Connector.Source_Offset;
            Dest_Offset   : constant Integer :=
                              Connector.Destination_Offset;
            X1     : constant Gdouble :=
                       Gdouble (Source.X) + Gdouble (Source.Width)
                     - Gdouble (UI.View_Left);
            Y1     : constant Gdouble :=
                              Gdouble (Source.Y) + Gdouble (Source_Offset)
                            + Gdouble (Title_Pixel_Height)
                              - Gdouble (UI.View_Top);
            X2     : constant Gdouble :=
                       Gdouble (Dest.X) - Gdouble (UI.View_Left);
            Y2     : constant Gdouble :=
                              Gdouble (Dest.Y) + Gdouble (Dest_Offset)
                            + Gdouble (Title_Pixel_Height)
                            - Gdouble (UI.View_Top);
            Mid_X  : constant Gdouble := (X1 + X2) / 2.0;
--              Mid_Y  : constant Gdouble := (Y1 + Y2) / 2.0;
         begin
            Cairo.Set_Source_Rgb (Context, 0.0, 0.0, 0.0);
            Cairo.Move_To (Context, X1, Y1);
            Cairo.Line_To (Context, Mid_X, Y1);
            Cairo.Line_To (Context, Mid_X, Y2);
            Cairo.Line_To (Context, X2, Y2);
            Cairo.Line_To (Context, X2 - 6.0, Y2 - 2.0);
            Cairo.Move_To (Context, X2, Y2);
            Cairo.Line_To (Context, X2 - 6.0, Y2 + 2.0);
            Cairo.Stroke (Context);
         end;
      end loop;

      Cairo.Destroy (Context);
      Context := Gdk.Cairo.Create (UI.Main_View.Get_Bin_Window);
      Cairo.Set_Source_Surface (Context, Surface, 0.0, 0.0);
      Cairo.Paint (Context);
      Cairo.Destroy (Context);
      Cairo.Surface_Destroy (Surface);
      Cairo.Pattern.Destroy (Pat);

   end Draw_Main_View;

   ----------------------------
   -- Draw_Main_View_Handler --
   ----------------------------

   function Draw_Main_View_Handler
     (W  : access Gtk.Widget.Gtk_Widget_Record'Class;
      UI : Gtk_UI_Access)
      return Boolean
   is
      pragma Unreferenced (W);
   begin
      Draw_Main_View (UI);
      return False;
   end Draw_Main_View_Handler;

   -----------------------------
   -- Draw_Navigation_Handler --
   -----------------------------

   function Draw_Navigation_Handler
     (W  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr : Cairo.Cairo_Context)
      return Boolean
   is
      pragma Unreferenced (W);
      Nav_Size : Gtk.Widget.Gtk_Allocation;
      View_Size : Gtk.Widget.Gtk_Allocation;
      Context : Cairo.Cairo_Context;
      Surface : Cairo.Cairo_Surface;
      Width, Height : Glib.Gdouble;
      UI            : constant Gtk_UI_Access := Current_UI;
   begin
      UI.Navigation.Get_Allocation (Nav_Size);
      UI.Main_View.Get_Allocation (View_Size);
      UI.View_Width := Positive (View_Size.Width);
      UI.View_Height := Positive (View_Size.Height);

      Width := Glib.Gdouble (Nav_Size.Width);
      Height := Glib.Gdouble (Nav_Size.Height);

      Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32,
           Nav_Size.Width, Nav_Size.Height);
      Context := Cairo.Create (Surface);

      Cairo.Set_Source_Rgb (Context, 0.8, 0.8, 0.8);
      Cairo.Rectangle (Context, 0.0, 0.0, Width, Height);
      Cairo.Fill (Context);

      declare
         use Glib;
         Scale  : constant Gdouble :=
                    Height / Gdouble (UI.View_Height)
                    / Gdouble (UI.Vertical_Scale);
      begin
         for Widget of UI.Widgets loop
            declare
               Fragment : constant Komnenos.Fragments.Fragment_Type :=
                            Widget.Fragment;
               X        : constant Gdouble :=
                            Gdouble (Fragment.X) * Scale;
               Y        : constant Gdouble :=
                            Gdouble (Fragment.Y) * Scale;
               F_Size   : Gtk.Widget.Gtk_Allocation;
--                 W        : constant Gdouble :=
--                              Gdouble (Fragment.Width) * Scale;
--                 H        : constant Gdouble :=
--                              Gdouble (Fragment.Height) * Scale;
               R        : constant Guint16 :=
                            Gdk.Color.Red (Widget.Background);
               G        : constant Guint16 :=
                            Gdk.Color.Green (Widget.Background);
               B        : constant Guint16 :=
                            Gdk.Color.Blue (Widget.Background);
            begin
               Widget.Grid.Get_Allocation (F_Size);

               Cairo.Set_Source_Rgb
                 (Context,
                  Gdouble (R) / 65535.0,
                  Gdouble (G) / 65535.0,
                  Gdouble (B) / 65535.0);
               Cairo.Rectangle (Context, X, Y,
                                Gdouble (F_Size.Width) * Scale,
                                Gdouble (F_Size.Height) * Scale);
               Cairo.Fill (Context);
            end;
         end loop;

         Cairo.Set_Source_Rgba (Context, 0.0, 0.0, 0.5, 0.8);
         Cairo.Rectangle (Context,
                          Gdouble (UI.View_Left) * Scale,
                          Gdouble (UI.View_Top) * Scale,
                          Gdouble (UI.View_Width) * Scale,
                          Gdouble (UI.View_Height) * Scale);
         Cairo.Stroke (Context);
      end;

      Cairo.Destroy (Context);

      Cairo.Set_Source_Surface (Cr, Surface, 0.0, 0.0);
      Cairo.Paint (Cr);

      Cairo.Surface_Destroy (Surface);

      return True;

   end Draw_Navigation_Handler;

   ----------------------------
   -- Draw_Rounded_Rectangle --
   ----------------------------

   procedure Draw_Rounded_Rectangle
     (Context       : Cairo.Cairo_Context;
      X, Y          : Glib.Gdouble;
      Width, Height : Glib.Gdouble;
      Radius        : Glib.Gdouble)
   is
      use Glib;
      R  : constant Gdouble := Radius;
      Pi : constant := Ada.Numerics.Pi;
      X1 : constant Gdouble := X + R;
      X2 : constant Gdouble := X + Width - R;
      Y1 : constant Gdouble := Y + R;
      Y2 : constant Gdouble := Y + Height - R;
   begin
      Cairo.New_Path (Context);
      Cairo.Arc (Context, X1, Y1, R, 2.0 * Pi / 2.0, 3.0 * Pi / 2.0);
      Cairo.Arc (Context, X2, Y1, R, 3.0 * Pi / 2.0, 4.0 * Pi / 2.0);
      Cairo.Arc (Context, X2, Y2, R, 0.0 * Pi / 2.0, 1.0 * Pi / 2.0);
      Cairo.Arc (Context, X1, Y2, R, 1.0 * Pi / 2.0, 2.0 * Pi / 2.0);
      Cairo.Close_Path (Context);
   end Draw_Rounded_Rectangle;

   ------------------------------
   -- Find_Fragment_By_Display --
   ------------------------------

   function Find_Fragment
     (Display : not null access Display_Interface'Class)
      return Komnenos.Fragments.Fragment_Type
   is
   begin
      for LW of Current_UI.Widgets loop
         if LW.Display = Display then
            return LW.Fragment;
         end if;
      end loop;
      return null;
   end Find_Fragment;

   --------------------------
   -- Find_Fragment_Widget --
   --------------------------

   function Find_Fragment_Widget
     (Fragment : Komnenos.Fragments.Fragment_Type;
      UI       : not null access Root_Gtk_UI'Class)
      return Gtk.Widget.Gtk_Widget
   is
      use Komnenos.Fragments;
   begin
      for LW of UI.Widgets loop
         if LW.Fragment = Fragment then
            return LW.Widget;
         end if;
      end loop;
      return null;
   end Find_Fragment_Widget;

   -----------------------------------
   -- Find_Layout_Record_By_Display --
   -----------------------------------

--     function Find_Layout_Record_By_Display
--       (Display : not null access Gtk.Widget.Gtk_Widget_Record'Class;
--        UI      : not null access Root_Gtk_UI'Class)
--        return Layout_Widget_Access
--     is
--        use type Gtk.Widget.Gtk_Widget;
--     begin
--        for LW of UI.Widgets loop
--           if LW.Display = Gtk.Widget.Gtk_Widget (Display) then
--              return LW;
--           end if;
--        end loop;
--        return null;
--     end Find_Layout_Record_By_Display;

   ---------------------------------
   -- Find_Layout_Record_By_Title --
   ---------------------------------

   function Find_Layout_Record_By_Title
     (Title   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      UI      : not null access Root_Gtk_UI'Class)
      return Layout_Widget_Access
   is
      use type Gtk.Widget.Gtk_Widget;
   begin
      for LW of UI.Widgets loop
         if LW.Title = Gtk.Widget.Gtk_Widget (Title) then
            return LW;
         end if;
      end loop;
      return null;
   end Find_Layout_Record_By_Title;

   -----------------------------------------
   -- Fragment_Title_Button_Press_Handler --
   -----------------------------------------

   function Fragment_Title_Button_Press_Handler
     (W     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      UI : constant Gtk_UI_Access := Current_UI;
      LW : constant Layout_Widget_Access :=
             Find_Layout_Record_By_Title (W, UI);
   begin
      UI.Dragging := LW;
      LW.Dragging := True;
      LW.Start_X  := Event.X_Root;
      LW.Start_Y  := Event.Y_Root;
      return True;
   end Fragment_Title_Button_Press_Handler;

   -------------------------------------------
   -- Fragment_Title_Button_Release_Handler --
   -------------------------------------------

   function Fragment_Title_Button_Release_Handler
     (W     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      use Glib;
      UI : constant Gtk_UI_Access := Current_UI;
      LW : constant Layout_Widget_Access :=
             Find_Layout_Record_By_Title (W, UI);
   begin
      if LW.Dragging then
         declare
            X_Offset : constant Integer := Integer (Event.X_Root - LW.Start_X);
            Y_Offset : constant Integer := Integer (Event.Y_Root - LW.Start_Y);
            Fragment : constant Komnenos.Fragments.Fragment_Type :=
                         LW.Fragment;
         begin
            LW.Show_Border := True;
            Fragment.Set_Position (Fragment.X + X_Offset,
                                   Fragment.Y + Y_Offset);
            UI.Layout.Move_Item (Fragment);
            LW.Dragging := False;
            UI.Dragging := null;
         end;
      end if;
      return True;
   end Fragment_Title_Button_Release_Handler;

   -----------------------------------
   -- Fragment_Title_Motion_Handler --
   -----------------------------------

   function Fragment_Title_Motion_Handler
     (W     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Motion)
      return Boolean
   is
      use Glib;
      UI : constant Gtk_UI_Access := Current_UI;
      LW : constant Layout_Widget_Access :=
             Find_Layout_Record_By_Title (W, UI);
   begin
      if LW.Dragging then
         declare
            X_Offset : constant Gint := Gint (Event.X_Root - LW.Start_X);
            Y_Offset : constant Gint := Gint (Event.Y_Root - LW.Start_Y);
            Fragment : constant Komnenos.Fragments.Fragment_Type :=
                         LW.Fragment;
            New_X    : constant Gint :=
                         Gint (Fragment.X - UI.View_Left) + X_Offset;
            New_Y    : constant Gint :=
                         Gint (Fragment.Y - UI.View_Top) + Y_Offset;
         begin
            UI.Main_View.Move
              (Child_Widget => LW.Widget,
               X            => New_X,
               Y            => New_Y);
         end;
      end if;
      return True;
   end Fragment_Title_Motion_Handler;

   -----------------
   -- From_Config --
   -----------------

   overriding procedure From_Config
     (UI : not null access Root_Gtk_UI;
      Config : Tropos.Configuration)
   is
      procedure Attach_Entity
        (Fragment : Komnenos.Fragments.Fragment_Type);

      -------------------
      -- Attach_Entity --
      -------------------

      procedure Attach_Entity
        (Fragment : Komnenos.Fragments.Fragment_Type)
      is
         use type Komnenos.Entities.Entity_Reference;
         Entity : constant Komnenos.Entities.Entity_Reference :=
                    (if UI.Entities.Exists (Fragment.Entity_Key)
                     then UI.Entities.Get (Fragment.Entity_Key)
                     else null);
      begin
         if Entity /= null then
            Entity.Select_Entity
              (Table  => UI,
               Parent => null,
               Visual => Fragment,
               Offset => 0);
            UI.Layout.Item_Placed (Fragment);
         end if;
      end Attach_Entity;

   begin
      Root_Komnenos_UI (UI.all).From_Config (Config);
      if Config.Contains ("layout") then
         UI.Layout.From_Config (Config.Child ("layout"));
      end if;
      UI.Layout.Scan (Attach_Entity'Access);
   end From_Config;

   ----------------
   -- Item_Moved --
   ----------------

   overriding procedure Item_Moved
     (Layout : in out Root_Gtk_Layout;
      Item   : Komnenos.Fragments.Fragment_Type)
   is
   begin
      Update_Fragment_Position (Layout.UI, Item);
   end Item_Moved;

   -----------------
   -- Item_Placed --
   -----------------

   overriding procedure Item_Placed
     (Layout : in out Root_Gtk_Layout;
      Item   : Komnenos.Fragments.Fragment_Type)
   is
      Widget  : Gtk.Widget.Gtk_Widget;
      Grid    : Gtk.Grid.Gtk_Grid;
      Display : Komnenos_Display;
      Title   : Gtk.Widget.Gtk_Widget;
   begin
      Create_Fragment_Widget
        (Fragment => Item,
         UI       => Layout.UI,
         Top      => Widget,
         Grid     => Grid,
         Display  => Display,
         Title    => Title);

      declare
         New_Item : constant Layout_Widget_Access :=
                      new Layout_Widget_Record'
                        (Widget     => Widget,
                         Grid        => Grid,
                         Display    => Display,
                         Title      => Title,
                         Fragment   => Item,
                         Background =>
                           Gdk.Color.Parse (Item.Background_Colour),
                         Border     => (0.0, 0.0, 0.0, 1.0),
                         Show_Border => True,
                         Grab_Focus => True,
                         Dragging   => False,
                         Start_X    => 0.0,
                         Start_Y     => 0.0);
         Got_Colour : Boolean;
         pragma Unreferenced (Got_Colour);
      begin
         Gdk.RGBA.Parse (New_Item.Border, Item.Border_Colour, Got_Colour);
         Borders.Add_Borders (New_Item.all);
         Layout.UI.Widgets.Append (New_Item);
         New_Item.Widget.Show_All;
      end;

      Layout.UI.Navigation.Queue_Draw;

   end Item_Placed;

   --------------------
   -- Place_Fragment --
   --------------------

   overriding procedure Place_Fragment
     (UI       : in out Root_Gtk_UI;
      Parent   : access Komnenos.Entities.Entity_Visual'Class;
      Offset   : Natural;
      Fragment : Komnenos.Fragments.Fragment_Type)
   is
      use Komnenos.Fragments;
   begin
      for Layout_Widget of UI.Widgets loop
         Update_Fragment_Size (Layout_Widget.Widget, Layout_Widget.Fragment);
      end loop;
      if UI.Active = null then
         UI.Active := Fragment;
      end if;
      if Parent = null then
         Fragment.Set_Position (Fragment.X + UI.View_Left,
                                Fragment.Y + UI.View_Top);
         UI.Layout.Place_Item (Fragment);
      else
         UI.Layout.Place_Item
           (Fragment, Komnenos.Fragments.Fragment_Type (Parent), Offset);
         UI.Connectors.Append
           (Komnenos.Connectors.Connect
              (Class              => Komnenos.Connectors.Arrow,
               Source             => Parent,
               Source_Offset      => Offset,
               Destination        => Fragment,
               Destination_Offset => 0));
      end if;
   end Place_Fragment;

   -----------------------
   -- Set_Corner_Widget --
   -----------------------

   overriding procedure Set_Corner_Widget
     (Fragment  : in out Layout_Widget_Record;
      Corner    : Borders.Border_Corner;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use Borders;
      Left : constant array (Border_Corner) of Glib.Gint :=
               (Top_Left | Bottom_Left => 0,
                Top_Right | Bottom_Right => 2);
      Top : constant array (Border_Corner) of Glib.Gint :=
               (Top_Left | Top_Right => 0,
                Bottom_Left | Bottom_Right => 3);

   begin
      Widget.Set_Size_Request (16, 16);
      Fragment.Grid.Attach
        (Widget, Left (Corner), Top (Corner), 1, 1);
   end Set_Corner_Widget;

   ---------------------
   -- Set_Side_Widget --
   ---------------------

   overriding procedure Set_Side_Widget
     (Fragment  : in out Layout_Widget_Record;
      Edge      : Borders.Border_Edge;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use Borders;
      Attach_Left : constant array (Border_Edge) of Glib.Gint :=
               (Left => 0, Top | Bottom => 1, Right => 2);
      Attach_Top  : constant array (Border_Edge) of Glib.Gint :=
               (Left | Right => 1, Top => 0, Bottom => 3);
      Width : constant array (Border_Edge) of Glib.Gint :=
                (others => 1);
      Height : constant array (Border_Edge) of Glib.Gint :=
                      (Left | Right => 2, Top | Bottom => 1);
   begin
      Widget.Set_Size_Request (16, 16);
      Fragment.Grid.Attach
        (Widget, Attach_Left (Edge), Attach_Top (Edge),
         Width (Edge), Height (Edge));
   end Set_Side_Widget;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI : in out Root_Gtk_UI) is
   begin

      Entity_Lists.Create (UI.Entity_Tree, UI.Entity_Filter);

      UI.Top_Level.Show_All;

      Gtk.Main.Main;
   end Start;

   ---------------
   -- To_Config --
   ---------------

   overriding procedure To_Config
     (UI     : Root_Gtk_UI;
      Config : in out Tropos.Configuration)
   is
      Layout_Config : Tropos.Configuration :=
                        Tropos.New_Config ("layout");
   begin
      Root_Komnenos_UI (UI).To_Config (Config);
      UI.Layout.To_Config (Layout_Config);
      Config.Add (Layout_Config);
   end To_Config;

   -------------
   -- To_RGBA --
   -------------

   function To_RGBA
     (Colour_Spec : String)
      return Gdk.RGBA.Gdk_RGBA
   is
      Success : Boolean;
      Result  : Gdk.RGBA.Gdk_RGBA;
   begin
      Gdk.RGBA.Parse (Result, Colour_Spec, Success);
      if Success then
         return Result;
      else
         return Gdk.RGBA.Null_RGBA;
      end if;
   end To_RGBA;

   ------------------------------
   -- Update_Fragment_Position --
   ------------------------------

   procedure Update_Fragment_Position
     (UI   : Gtk_UI_Access;
      Item : Komnenos.Fragments.Fragment_Type)
   is
   begin
      UI.Main_View.Move
        (Find_Fragment_Widget (Item, UI),
         Glib.Gint (Item.X - UI.View_Left),
         Glib.Gint (Item.Y - UI.View_Top));
      UI.Navigation.Queue_Draw;
   end Update_Fragment_Position;

   -------------------------------
   -- Update_Fragment_Positions --
   -------------------------------

   procedure Update_Fragment_Positions
     (UI : Gtk_UI_Access)
   is
   begin
      for LW of UI.Widgets loop
         Update_Fragment_Position (UI, LW.Fragment);
      end loop;
   end Update_Fragment_Positions;

   --------------------------
   -- Update_Fragment_Size --
   --------------------------

   procedure Update_Fragment_Size
     (Widget   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Fragment : Komnenos.Fragments.Fragment_Type)
   is
      Size : Gtk.Widget.Gtk_Allocation;
   begin
      Widget.Get_Allocation (Size);
      Fragment.Set_Size (Natural (Size.Width), Natural (Size.Height));
   end Update_Fragment_Size;

end Komnenos.UI.Gtk_UI;
