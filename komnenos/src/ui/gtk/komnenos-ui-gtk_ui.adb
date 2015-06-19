with Ada.Characters.Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics;
--  with Ada.Text_IO;

with Tropos;

with Aquarius.Colours;
with Aquarius.Fonts;
with Aquarius.Styles;
with Aquarius.Themes;

with Komnenos.Connectors;
with Komnenos.Layouts;

with Komnenos.UI.Gtk_UI.Entity_Lists;

with Glib.Error;
with Glib.Properties;
with Glib.Values;

with Gdk.Cairo;
with Gdk.Color;
with Gdk.Cursor;
with Gdk.Event;
with Gdk.Main;
with Gdk.Pixbuf;
with Gdk.Rectangle;
with Gdk.RGBA;
with Gdk.Window;

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
with Gtk.Scrolled_Window;
with Gtk.Style_Provider;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_Tag;
with Gtk.Text_Tag_Table;
with Gtk.Text_View;
with Gtk.Tree_View;
with Gtk.Widget;
with Gtk.Window;

with Gtkada.Style;

with Pango.Enums;
with Pango.Font;

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

   overriding procedure To_Config
     (Layout : Root_Gtk_Layout;
      Config : in out Tropos.Configuration);

   overriding procedure From_Config
     (Layout : in out Root_Gtk_Layout;
      Config : Tropos.Configuration);

   type Layout_Widget_Record is
      record
         Widget       : Gtk.Widget.Gtk_Widget;
         Display      : Gtk.Widget.Gtk_Widget;
         Title        : Gtk.Widget.Gtk_Widget;
         Fragment     : Komnenos.Fragments.Fragment_Type;
         Background   : Gdk.Color.Gdk_Color;
         Border       : Gdk.Color.Gdk_Color;
         Show_Border  : Boolean;
         Grab_Focus   : Boolean;
         Dragging     : Boolean := False;
         Start_X      : Glib.Gdouble;
         Start_Y      : Glib.Gdouble;
      end record;

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
         Entity_Tree     : Gtk.Tree_View.Gtk_Tree_View;
         Entity_Filter   : Gtk.GEntry.Gtk_Entry;
         Navigation      : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Layout          : access Root_Gtk_Layout'Class;
         Widgets         : Layout_Widget_Lists.List;
         Connectors      : Connector_Lists.List;
         Active          : Komnenos.Fragments.Fragment_Type := null;
         Hover_Start     : Natural := 0;
         Hover_Finish    : Natural := 0;
         Hover_Style     : Aquarius.Styles.Aquarius_Style;
         Dragging        : Layout_Widget_Access := null;
         Vertical_Scale  : Float := 3.0;  --  three main views fit vertically
                                          --  into the navigator
      end record;

   type Gtk_UI_Access is access all Root_Gtk_UI'Class;

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
     (UI : in out Root_Gtk_UI;
      Config : Tropos.Configuration);

   procedure Create_Fragment_Widget
     (Fragment : Komnenos.Fragments.Fragment_Type;
      UI       : not null access Root_Gtk_UI'Class;
      Widget   : out Gtk.Widget.Gtk_Widget;
      Display  : out Gtk.Widget.Gtk_Widget;
      Title    : out Gtk.Widget.Gtk_Widget);

   function Find_Fragment_Widget
     (Fragment : Komnenos.Fragments.Fragment_Type;
      UI       : not null access Root_Gtk_UI'Class)
      return Gtk.Widget.Gtk_Widget;

   function Find_Fragment_By_Display
     (Display : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      UI      : not null access Root_Gtk_UI'Class)
      return Komnenos.Fragments.Fragment_Type;

   function Find_Layout_Record_By_Display
     (Display : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      UI      : not null access Root_Gtk_UI'Class)
      return Layout_Widget_Access;

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

   function Get_Tag_Entry
     (Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      Style  : Aquarius.Styles.Aquarius_Style)
      return Gtk.Text_Tag.Gtk_Text_Tag;
   --  return a text tag corresponding to the given Style.  Create a new
   --  text tag if necessary.

   procedure Render_Text
     (View : Gtk.Text_View.Gtk_Text_View;
      Fragment : Komnenos.Fragments.Fragment_Type);

   function To_RGBA
     (Colour_Spec : String)
      return Gdk.RGBA.Gdk_RGBA;

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

   package Text_View_Event_Handler is
     new Gtk.Handlers.User_Return_Callback
       (Widget_Type => Gtk.Text_View.Gtk_Text_View_Record,
        Return_Type => Boolean,
        User_Type   => Gtk_UI_Access);

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

   function Text_View_Button_Release_Handler
     (Text_View : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event     : Gdk.Event.Gdk_Event;
      UI        : Gtk_UI_Access)
      return Boolean;

   function Text_View_Motion_Handler
     (Text_View : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event     : Gdk.Event.Gdk_Event;
      UI        : Gtk_UI_Access)
      return Boolean;

   function Text_View_Draw_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean;

   procedure Update_Fragment_Size
     (Widget   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Fragment : Komnenos.Fragments.Fragment_Type);

   procedure Update_Fragment_Positions
     (UI : Gtk_UI_Access);

   procedure Update_Fragment_Position
     (UI   : Gtk_UI_Access;
      Item : Komnenos.Fragments.Fragment_Type);

   procedure Set_Text_State
     (UI        : Gtk_UI_Access;
      Text_View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      State     : Aquarius.Themes.Element_State);

   procedure Follow_If_Link
     (UI        : Gtk_UI_Access;
      Text_View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter);

   procedure Apply_Style_To_Text
     (View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Start_Offset  : Positive;
      Finish_Offset : Positive;
      Style         : Aquarius.Styles.Aquarius_Style;
      Remove        : Boolean);

   procedure Paint_Line_Background
     (View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Context : Cairo.Cairo_Context;
      Y       : Glib.Gint;
      Height  : Glib.Gint;
      Colour  : Gdk.RGBA.Gdk_RGBA);

   procedure Draw_Rounded_Rectangle
     (Context       : Cairo.Cairo_Context;
      X, Y          : Glib.Gdouble;
      Width, Height : Glib.Gdouble;
      Radius        : Glib.Gdouble);

   --------------------------------
   -- Apply_Style_To_Text_Buffer --
   --------------------------------

   procedure Apply_Style_To_Text
     (View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Start_Offset  : Positive;
      Finish_Offset : Positive;
      Style         : Aquarius.Styles.Aquarius_Style;
      Remove        : Boolean)
   is
      use Aquarius.Styles;
      Buffer     : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
                     View.Get_Buffer;
      Tag        : constant Gtk.Text_Tag.Gtk_Text_Tag :=
                     Get_Tag_Entry (Buffer, Style);
      Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Buffer.Get_Iter_At_Offset
        (Start_Iter, Glib.Gint (Start_Offset - 1));
      Buffer.Get_Iter_At_Offset
        (End_Iter, Glib.Gint (Finish_Offset - 1));
      if Remove then
         Buffer.Remove_Tag
           (Tag, Start_Iter, End_Iter);
      else
         Buffer.Apply_Tag
           (Tag, Start_Iter, End_Iter);
      end if;

   end Apply_Style_To_Text;

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
                   Gtk_UI_Access (Current_UI);
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
      Widget   : out Gtk.Widget.Gtk_Widget;
      Display  : out Gtk.Widget.Gtk_Widget;
      Title    : out Gtk.Widget.Gtk_Widget)
   is
      Text   : Gtk.Text_View.Gtk_Text_View;
      Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Grid   : Gtk.Grid.Gtk_Grid;
      Label  : Gtk.Label.Gtk_Label;
      Events : Gtk.Event_Box.Gtk_Event_Box;

   begin
      Gtk.Event_Box.Gtk_New (Events);
      Gtk.Label.Gtk_New (Label, Fragment.Title);
      Events.Add (Label);

      Gtk.Text_View.Gtk_New (Text);

--        Label.Override_Background_Color
--          (Gtk.Enums.Gtk_State_Flag_Normal,
--           To_RGBA (Fragment.Background_Colour));

      if Fragment.Background_Colour /= "" then
         Text.Override_Background_Color
           (Gtk.Enums.Gtk_State_Flag_Normal,
            To_RGBA (Fragment.Background_Colour));
      end if;

      Text.Set_Size_Request (Glib.Gint (Fragment.Width - 6),
                             Glib.Gint (Fragment.Height - 4));

      Render_Text (Text, Fragment);

      Text_View_Event_Handler.Connect
        (Text, Gtk.Widget.Signal_Button_Release_Event,
         Text_View_Event_Handler.To_Marshaller
           (Text_View_Button_Release_Handler'Access),
         Gtk_UI_Access (UI));

      declare
         use Gdk.Event;
      begin
         Text.Add_Events
           (Button_Press_Mask or Button_Release_Mask or Pointer_Motion_Mask);
         Label.Add_Events
           (Button_Press_Mask or Button_Release_Mask or Pointer_Motion_Mask);
      end;

      Text_View_Event_Handler.Connect
        (Text, Gtk.Widget.Signal_Motion_Notify_Event,
         Text_View_Event_Handler.To_Marshaller
           (Text_View_Motion_Handler'Access),
         Gtk_UI_Access (UI));

      Text.On_Draw
        (Text_View_Draw_Handler'Access);

      Events.On_Button_Press_Event
        (Fragment_Title_Button_Press_Handler'Access);

      Events.On_Button_Release_Event
        (Fragment_Title_Button_Release_Handler'Access);

      Events.On_Motion_Notify_Event
        (Fragment_Title_Motion_Handler'Access);

      Gtk.Scrolled_Window.Gtk_New (Scroll);
      Scroll.Set_Min_Content_Height (Glib.Gint (Fragment.Height));
      Scroll.Set_Min_Content_Width (Glib.Gint (Fragment.Width));

      Scroll.Add (Text);

      Gtk.Grid.Gtk_New (Grid);

      Grid.Attach (Events,
                   Left   => 1,
                   Top    => 0,
                   Width  => 1,
                   Height => 1);
      Grid.Attach (Scroll,
                   Left   => 1,
                   Top    => 1,
                   Width  => 1,
                   Height => 1);

      Widget := Gtk.Widget.Gtk_Widget (Grid);
      Display := Gtk.Widget.Gtk_Widget (Text);
      Title := Gtk.Widget.Gtk_Widget (Events);

      Widget.Show_All;

      UI.Main_View.Put (Widget,
                        Glib.Gint (Fragment.X - UI.View_Left),
                        Glib.Gint (Fragment.Y - UI.View_Top));

      declare
         Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      begin
         Text.Get_Buffer.Get_Start_Iter (Start_Iter);
         Text.Get_Buffer.Place_Cursor (Start_Iter);
      end;

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

      for LW of UI.Widgets loop
         if LW.Show_Border then
            declare
               Fragment : constant Komnenos.Fragments.Fragment_Type :=
                            LW.Fragment;
               Widget   : constant Gtk.Widget.Gtk_Widget :=
                            LW.Widget;
               Margin   : constant := Komnenos.Layouts.Margin;
               X        : constant Gdouble :=
                            Gdouble (Fragment.X) - Gdouble (Margin / 2)
                          - Gdouble (UI.View_Left);
               Y        : constant Gdouble :=
                            Gdouble (Fragment.Y) - Gdouble (Margin / 2)
                          - Gdouble (UI.View_Top);
               Width    : constant Gdouble :=
                            Gdouble (Widget.Get_Allocated_Width)
                            + Gdouble (Margin);
               Height   : constant Gdouble :=
                            Gdouble (Widget.Get_Allocated_Height)
                            + Gdouble (Margin);
               Colour   : constant Gdk.Color.Gdk_Color :=
                            LW.Border;
               Red      : constant Gdouble :=
                            Gdouble (Gdk.Color.Red (Colour)) / 65535.0;
               Green    : constant Gdouble :=
                            Gdouble (Gdk.Color.Green (Colour)) / 65535.0;
               Blue     : constant Gdouble :=
                            Gdouble (Gdk.Color.Blue (Colour)) / 65535.0;
            begin
               Cairo.Set_Source_Rgb (Context, Red, Green, Blue);
               Draw_Rounded_Rectangle (Context, X, Y, Width, Height, 25.0);
               Cairo.Fill (Context);
            end;
         end if;
      end loop;

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
      UI            : constant Gtk_UI_Access :=
                        Gtk_UI_Access (Current_UI);
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
               Widget.Widget.Get_Allocation (F_Size);

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

   function Find_Fragment_By_Display
     (Display : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      UI      : not null access Root_Gtk_UI'Class)
      return Komnenos.Fragments.Fragment_Type
   is
      use type Gtk.Widget.Gtk_Widget;
   begin
      for LW of UI.Widgets loop
         if LW.Display = Gtk.Widget.Gtk_Widget (Display) then
            return LW.Fragment;
         end if;
      end loop;
      return null;
   end Find_Fragment_By_Display;

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

   function Find_Layout_Record_By_Display
     (Display : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      UI      : not null access Root_Gtk_UI'Class)
      return Layout_Widget_Access
   is
      use type Gtk.Widget.Gtk_Widget;
   begin
      for LW of UI.Widgets loop
         if LW.Display = Gtk.Widget.Gtk_Widget (Display) then
            return LW;
         end if;
      end loop;
      return null;
   end Find_Layout_Record_By_Display;

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

   --------------------
   -- Follow_If_Link --
   --------------------

   procedure Follow_If_Link
     (UI        : Gtk_UI_Access;
      Text_View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      use type Komnenos.Entities.Entity_Reference;
      Fragment : constant Komnenos.Fragments.Fragment_Type :=
                   Find_Fragment_By_Display
                     (Display => Text_View,
                      UI      => UI);
      Entity : constant Komnenos.Entities.Entity_Reference :=
                 Fragment.Get_Link
                   (Natural (Gtk.Text_Iter.Get_Offset (Iter)) + 1);
   begin
      if Entity /= null then
         declare
            Location : Gdk.Rectangle.Gdk_Rectangle;
         begin
            Text_View.Get_Iter_Location (Iter, Location);
            Entity.Select_Entity
              (UI, Find_Fragment_By_Display (Text_View, UI),
               Natural (Location.Y) + Natural (Location.Height) / 2);
         end;
      end if;
   end Follow_If_Link;

   -----------------------------------------
   -- Fragment_Title_Button_Press_Handler --
   -----------------------------------------

   function Fragment_Title_Button_Press_Handler
     (W     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      UI : constant Gtk_UI_Access := Gtk_UI_Access (Current_UI);
      LW : constant Layout_Widget_Access :=
             Find_Layout_Record_By_Title (W, UI);
   begin
      UI.Dragging := LW;
      LW.Dragging := True;
      LW.Start_X  := Event.X_Root;
      LW.Start_Y  := Event.Y_Root;
      UI.Main_View.Queue_Draw;
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
      UI : constant Gtk_UI_Access := Gtk_UI_Access (Current_UI);
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
      UI : constant Gtk_UI_Access := Gtk_UI_Access (Current_UI);
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
            if LW.Show_Border then
               LW.Show_Border := False;
               UI.Main_View.Queue_Draw;
            end if;
         end;
      end if;
      return True;
   end Fragment_Title_Motion_Handler;

   -----------------
   -- From_Config --
   -----------------

   overriding procedure From_Config
     (Layout : in out Root_Gtk_Layout;
      Config : Tropos.Configuration)
   is
   begin
      null;
   end From_Config;

   -----------------
   -- From_Config --
   -----------------

   overriding procedure From_Config
     (UI : in out Root_Gtk_UI;
      Config : Tropos.Configuration)
   is
   begin
      null;
   end From_Config;

   -------------------
   -- Get_Tag_Entry --
   -------------------

   function Get_Tag_Entry
     (Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      Style  : Aquarius.Styles.Aquarius_Style)
      return Gtk.Text_Tag.Gtk_Text_Tag
   is
      use Gtk.Text_Tag, Gtk.Text_Tag_Table;
      Tag_Table : constant Gtk.Text_Tag_Table.Gtk_Text_Tag_Table :=
        Buffer.Get_Tag_Table;
      Result    : Gtk_Text_Tag;
      Name : constant String := Aquarius.Styles.Name (Style);
   begin
      Result := Tag_Table.Lookup (Name);
      if Result = null then
         Result := Buffer.Create_Tag (Name);
         declare
            use Pango.Font, Aquarius.Fonts;
            Font : constant Aquarius_Font := Style.Font;
            Desc : constant Pango_Font_Description :=
                     To_Font_Description
                       (Aquarius.Fonts.Name (Font),
                        Size => Glib.Gint (Size (Font)));
         begin
            if Is_Bold (Font) then
               Set_Weight (Desc, Pango.Enums.Pango_Weight_Bold);
            end if;
            if Is_Italic (Font) then
               Set_Style (Desc, Pango.Enums.Pango_Style_Italic);
            end if;
            Set_Property (Result, Font_Desc_Property, Desc);

            if Is_Underlined (Font) then
               declare
                  Value     : Glib.Values.GValue;
               begin
                  Glib.Values.Init (Value, Glib.GType_Int);
                  Glib.Values.Set_Int
                    (Value,
                     Pango.Enums.Underline'Pos
                       (Pango.Enums.Pango_Underline_Single));
                  Glib.Properties.Set_Property
                    (Result, "underline", Value);
               end;
            end if;

            if Has_Foreground (Font) then
               declare
                  use Glib;
                  use Aquarius.Colours;
                  Colour : constant Aquarius_Colour :=
                             Get_Foreground (Font);
                  Foreground : Gdk.Color.Gdk_Color;
               begin
                  Gdk.Color.Set_Rgb
                    (Color => Foreground,
                     Red   => Guint16 (Red (Colour)) * 256,
                     Green => Guint16 (Green (Colour)) * 256,
                     Blue  => Guint16 (Blue (Colour)) * 256);
                  Gdk.Color.Set_Property
                    (Result, Foreground_Gdk_Property, Foreground);
               end;
            end if;

            if Has_Background (Font) then
               declare
                  use Glib;
                  use Aquarius.Colours;
                  Colour : constant Aquarius_Colour :=
                             Get_Foreground (Font);
                  Background : Gdk.Color.Gdk_Color;
               begin
                  Gdk.Color.Set_Rgb
                    (Color => Background,
                     Red   => Guint16 (Red (Colour)) * 256,
                     Green => Guint16 (Green (Colour)) * 256,
                     Blue  => Guint16 (Blue (Colour)) * 256);
                  Gdk.Color.Set_Property
                    (Result, Background_Gdk_Property, Background);
               end;
            end if;

         end;
      end if;
      return Result;
   end Get_Tag_Entry;

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
      Display : Gtk.Widget.Gtk_Widget;
      Title   : Gtk.Widget.Gtk_Widget;
   begin
      Create_Fragment_Widget
        (Fragment => Item,
         UI       => Layout.UI,
         Widget   => Widget,
         Display  => Display,
         Title    => Title);

      declare
         New_Item : constant Layout_Widget_Access :=
                      new Layout_Widget_Record'
                        (Widget     => Widget,
                         Display    => Display,
                         Title      => Title,
                         Fragment   => Item,
                         Background =>
                           Gdk.Color.Parse (Item.Background_Colour),
                         Border     =>
                           Gdk.Color.Parse (Item.Border_Colour),
                         Show_Border => True,
                         Grab_Focus => True,
                         Dragging   => False,
                         Start_X    => 0.0,
                         Start_Y    => 0.0);
      begin
         Layout.UI.Widgets.Append (New_Item);
      end;

      Layout.UI.Navigation.Queue_Draw;
      Layout.UI.Main_View.Queue_Draw;

   end Item_Placed;

   ---------------------------
   -- Paint_Line_Background --
   ---------------------------

   procedure Paint_Line_Background
     (View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Context : Cairo.Cairo_Context;
      Y       : Glib.Gint;
      Height  : Glib.Gint;
      Colour  : Gdk.RGBA.Gdk_RGBA)
   is
      use Glib;
      Visible_Rect : Gdk.Rectangle.Gdk_Rectangle;
      Line_Rect    : Gdk.Rectangle.Gdk_Rectangle;
      Win_Y        : Glib.Gint;
      Clip_X1, Clip_Y1 : Glib.Gdouble;
      Clip_X2, Clip_Y2 : Glib.Gdouble;
   begin

      View.Get_Visible_Rect (Visible_Rect);
      View.Buffer_To_Window_Coords
        (Win      => Gtk.Enums.Text_Window_Text,
         Buffer_X => Visible_Rect.X,
         Buffer_Y => Y,
         Window_X => Line_Rect.X,
         Window_Y => Win_Y);

      Cairo.Clip_Extents
        (Context, Clip_X1, Clip_Y1, Clip_X2, Clip_Y2);

      Line_Rect.X := Gint (Clip_X1);
      Line_Rect.Width := Gint (Clip_X2 - Clip_X1);
      Line_Rect.Y := Win_Y;
      Line_Rect.Height := Height;

      Cairo.Set_Source_Rgba (Context,
                             Colour.Red, Colour.Green, Colour.Blue,
                             Colour.Alpha);
      Cairo.Rectangle (Context,
                       Gdouble (Line_Rect.X), Gdouble (Line_Rect.Y) + 0.5,
                       Gdouble (Line_Rect.Width),
                       Gdouble (Line_Rect.Height - 1));
      Cairo.Stroke_Preserve (Context);
      Cairo.Fill (Context);
   end Paint_Line_Background;

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
      UI.Main_View.Queue_Draw;
   end Place_Fragment;

   -----------------
   -- Render_Text --
   -----------------

   procedure Render_Text
     (View : Gtk.Text_View.Gtk_Text_View;
      Fragment : Komnenos.Fragments.Fragment_Type)
   is
      Buffer : constant Gtk.Text_Buffer.Gtk_Text_Buffer := View.Get_Buffer;

      procedure New_Line;
      procedure Put
        (Text  : String;
         Style : Aquarius.Styles.Aquarius_Style;
         Link  : Komnenos.Entities.Entity_Reference);

      --------------
      -- New_Line --
      --------------

      procedure New_Line is
      begin
         Buffer.Insert_At_Cursor ((1 => Ada.Characters.Latin_1.LF));
      end New_Line;

      ---------
      -- Put --
      ---------

      procedure Put
        (Text  : String;
         Style : Aquarius.Styles.Aquarius_Style;
         Link  : Komnenos.Entities.Entity_Reference)
      is
         pragma Unreferenced (Link);
         Tag : constant Gtk.Text_Tag.Gtk_Text_Tag :=
                 Get_Tag_Entry (Buffer, Style);
         Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      begin
         Buffer.Get_End_Iter (Iter);
         Buffer.Insert_With_Tags
           (Iter, Text, Tag);
      end Put;

   begin
      Fragment.Iterate (Put'Access, New_Line'Access);
   end Render_Text;

   --------------------
   -- Set_Text_State --
   --------------------

   procedure Set_Text_State
     (UI        : Gtk_UI_Access;
      Text_View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      State     : Aquarius.Themes.Element_State)
   is
      use Aquarius.Styles;

      Fragment : constant Komnenos.Fragments.Fragment_Type :=
                   Find_Fragment_By_Display
                     (Display => Text_View,
                      UI      => UI);
      Style         : Aquarius_Style;
      Current_Style : Aquarius_Style;
      Offset        : constant Positive :=
                        Natural (Gtk.Text_Iter.Get_Offset (Iter)) + 1;
      Start_Offset  : Positive;
      Finish_Offset : Positive;
   begin
      Fragment.Get_Style (State, Offset,
                          Style, Start_Offset, Finish_Offset);

      if Start_Offset = UI.Hover_Start
        and then Finish_Offset = UI.Hover_Finish
      then
         return;
      end if;

      case Mouse_Cursor (Style) is
         when Default =>
            Gdk.Window.Set_Cursor (Text_View.Get_Root_Window, null);
         when Hand =>
            Gdk.Window.Set_Cursor (Text_View.Get_Root_Window,
                                   Gdk.Cursor.Gdk_Cursor_New
                                     (Gdk.Cursor.Hand2));
            Gdk.Main.Flush;
      end case;

      if UI.Hover_Start /= 0 then
         Apply_Style_To_Text
           (Text_View, UI.Hover_Start, UI.Hover_Finish,
            UI.Hover_Style, Remove => True);
         UI.Hover_Start := 0;
         UI.Hover_Finish := 0;
      end if;

      Current_Style :=
        Fragment.Get_Style (Aquarius.Themes.Normal, Offset);

      if Current_Style = Style then
         return;
      end if;

      UI.Hover_Start := Start_Offset;
      UI.Hover_Finish := Finish_Offset;
      UI.Hover_Style := Style;

      Apply_Style_To_Text
        (Text_View, Start_Offset, Finish_Offset, Style,
         Remove => False);

   end Set_Text_State;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI : in out Root_Gtk_UI) is
   begin

      Entity_Lists.Create (UI.Entity_Tree, UI.Entity_Filter);

      UI.Top_Level.Show_All;

      Gtk.Main.Main;
   end Start;

   --------------------------------------
   -- Text_View_Button_Release_Handler --
   --------------------------------------

   function Text_View_Button_Release_Handler
     (Text_View : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event     : Gdk.Event.Gdk_Event;
      UI        : Gtk_UI_Access)
      return Boolean
   is
      use type Glib.Gint;
      use Gtk.Text_Iter;
      Buffer : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
                 Text_View.Get_Buffer;
      Have_Selection : Boolean;
      Start, Finish : Gtk_Text_Iter;
      Iter : Gtk_Text_Iter;
      X, Y : Glib.Gint;
   begin
      Buffer.Get_Selection_Bounds (Start, Finish, Have_Selection);
      if Have_Selection
        and then Get_Offset (Start) /= Get_Offset (Finish)
      then
         return False;
      end if;
      Text_View.Window_To_Buffer_Coords
        (Gtk.Enums.Text_Window_Widget,
         Glib.Gint (Event.Button.X), Glib.Gint (Event.Button.Y),
         X, Y);
      Text_View.Get_Iter_At_Location (Iter, X, Y);

      Follow_If_Link (UI, Text_View, Iter);

      return False;
   end Text_View_Button_Release_Handler;

   ----------------------------
   -- Text_View_Draw_Handler --
   ----------------------------

   function Text_View_Draw_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean
   is
      use Glib;
      Text_View    : constant Gtk.Text_View.Gtk_Text_View :=
                       Gtk.Text_View.Gtk_Text_View (Widget);
      Iter         : Gtk.Text_Iter.Gtk_Text_Iter;
      Buffer       : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
                       Text_View.Get_Buffer;
      Location     : Gdk.Rectangle.Gdk_Rectangle;

   begin

      Buffer.Get_Iter_At_Mark
        (Iter, Buffer.Get_Insert);
      Text_View.Get_Iter_Location
        (Iter, Location);

      Paint_Line_Background
        (View    => Text_View,
         Context => Cr,
         Y       => Location.Y + 1,
         Height  => Location.Height - 1,
         Colour  => (0.8, 0.5, 0.5, 0.2));

      declare
         LW : constant Layout_Widget_Access :=
                Find_Layout_Record_By_Display
                  (Widget, Gtk_UI_Access (Current_UI));
      begin
         if LW.Grab_Focus then
            Text_View.Grab_Focus;
            LW.Grab_Focus := False;
         end if;
      end;

      return False;

   end Text_View_Draw_Handler;

   ------------------------------
   -- Text_View_Motion_Handler --
   ------------------------------

   function Text_View_Motion_Handler
     (Text_View : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event     : Gdk.Event.Gdk_Event;
      UI        : Gtk_UI_Access)
      return Boolean
   is
      use type Glib.Gint;
      use Gtk.Text_Iter;
      Buffer : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
                 Text_View.Get_Buffer;
      Have_Selection : Boolean;
      Start, Finish : Gtk_Text_Iter;
      Iter : Gtk_Text_Iter;
      X, Y : Glib.Gint;
   begin
      Buffer.Get_Selection_Bounds (Start, Finish, Have_Selection);
      if Have_Selection
        and then Get_Offset (Start) /= Get_Offset (Finish)
      then
         return False;
      end if;

      Text_View.Window_To_Buffer_Coords
        (Gtk.Enums.Text_Window_Widget,
         Glib.Gint (Event.Button.X), Glib.Gint (Event.Button.Y),
         X, Y);
      Text_View.Get_Iter_At_Location (Iter, X, Y);

      Set_Text_State (UI, Text_View, Iter, Aquarius.Themes.Hover);

      return False;
   end Text_View_Motion_Handler;

   ---------------
   -- To_Config --
   ---------------

   overriding procedure To_Config
     (Layout : Root_Gtk_Layout;
      Config : in out Tropos.Configuration)
   is
      pragma Unreferenced (Layout);
      pragma Unreferenced (Config);
   begin
      null;
   end To_Config;

   ---------------
   -- To_Config --
   ---------------

   overriding procedure To_Config
     (UI     : Root_Gtk_UI;
      Config : in out Tropos.Configuration)
   is
      pragma Unreferenced (UI);
      pragma Unreferenced (Config);
   begin
      null;
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
      UI.Main_View.Queue_Draw;
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
      UI.Main_View.Queue_Draw;
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
