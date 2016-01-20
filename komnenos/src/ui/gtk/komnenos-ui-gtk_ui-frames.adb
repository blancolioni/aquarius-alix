with Glib.Object;

with Gdk.Event;

with Gtk.Enums;
with Gtk.Event_Box;
with Gtk.Label;

with Komnenos.UI.Gtk_UI.Text;

package body Komnenos.UI.Gtk_UI.Frames is

   type Frame_Object_Record is
     new Glib.Object.GObject_Record with
      record
         Frame : Gtk_Frame;
      end record;

   type Frame_Object_Access is access all Frame_Object_Record'Class;

   function On_Title_Button_Press
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button)
      return Boolean;

   function On_Title_Button_Release
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button)
      return Boolean;

   function On_Title_Motion
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Motion)
      return Boolean;

   -------------------
   -- Border_Colour --
   -------------------

   overriding function Border_Colour
     (Fragment : Root_Gtk_Frame_Record)
      return Gdk.RGBA.Gdk_RGBA
   is
      Colour  : Gdk.RGBA.Gdk_RGBA;
      Success : Boolean;
   begin
      Gdk.RGBA.Parse (Colour, Fragment.Fragment.Border_Colour, Success);
      if not Success then
         Colour := (0.0, 0.0, 0.0, 1.0);
      end if;
      return Colour;
   end Border_Colour;

   --------------
   -- Fragment --
   --------------

   function Fragment
     (Frame : Root_Gtk_Frame_Record)
      return Komnenos.Fragments.Fragment_Type
   is
   begin
      return Frame.Fragment;
   end Fragment;

   ---------------
   -- New_Frame --
   ---------------

   function New_Frame
     (Layout   : not null access Komnenos.Layouts.Root_Layout_Type'Class;
      Fragment : Komnenos.Fragments.Fragment_Type)
      return Gtk_Frame
   is
      Grid : constant Gtk_Frame := new Root_Gtk_Frame_Record;
      Text : constant Komnenos.UI.Gtk_UI.Text.Komnenos_Text_View :=
               Komnenos.UI.Gtk_UI.Text.Create_Text_View
                 (Fragment);
      Label  : Gtk.Label.Gtk_Label;
      Events : Gtk.Event_Box.Gtk_Event_Box;
      Object : constant Frame_Object_Access := new Frame_Object_Record;
   begin
      Object.Initialize;
      Object.Frame := Grid;

      Grid.Layout := Komnenos.Layouts.Layout_Type (Layout);
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
        (On_Title_Button_Press'Access, Object);

      Events.On_Button_Release_Event
        (On_Title_Button_Release'Access, Object);

      Events.On_Motion_Notify_Event
        (On_Title_Motion'Access, Object);

--        Gtk.Scrolled_Window.Gtk_New (Scroll);

      Gtk.Grid.Initialize (Grid);

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

      Grid.Fragment := Fragment;

      Grid.Show_All;

      return Grid;

   end New_Frame;

   ---------------------------
   -- On_Title_Button_Press --
   ---------------------------

   function On_Title_Button_Press
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      Frame : constant Gtk_Frame :=
                Frame_Object_Access (Object).Frame;
   begin
      Frame.Dragging := True;
      Frame.Start_X  := Event.X_Root;
      Frame.Start_Y  := Event.Y_Root;
      return True;
   end On_Title_Button_Press;

   -----------------------------
   -- On_Title_Button_Release --
   -----------------------------

   function On_Title_Button_Release
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      pragma Unreferenced (Event);
      Frame : constant Gtk_Frame :=
                Frame_Object_Access (Object).Frame;
   begin
      Frame.Dragging := False;
--        if LW.Dragging then
--           declare
--         X_Offset : constant Integer := Integer (Event.X_Root - LW.Start_X);
--         Y_Offset : constant Integer := Integer (Event.Y_Root - LW.Start_Y);
--              Fragment : constant Komnenos.Fragments.Fragment_Type :=
--                           LW.Fragment;
--           begin
--              LW.Show_Border := True;
--              Fragment.Set_Position (Fragment.X + X_Offset,
--                                     Fragment.Y + Y_Offset);
--              UI.Layout.Move_Item (Fragment);
--              LW.Dragging := False;
--              UI.Dragging := null;
--           end;
--        end if;
      return True;
   end On_Title_Button_Release;

   ---------------------
   -- On_Title_Motion --
   ---------------------

   function On_Title_Motion
     (Object : access Glib.Object.GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Motion)
      return Boolean
   is
      Frame : constant Gtk_Frame :=
                Frame_Object_Access (Object).Frame;
   begin
      if Frame.Dragging then
         declare
            use Glib;
            X_Offset : constant Integer :=
                         Integer (Event.X_Root - Frame.Start_X);
            Y_Offset : constant Integer :=
                         Integer (Event.Y_Root - Frame.Start_Y);
            Fragment : constant Komnenos.Fragments.Fragment_Type :=
                         Frame.Fragment;
         begin
            Frame.Fragment.Set_Position
              (X => Fragment.X + X_Offset,
               Y => Fragment.Y + Y_Offset);
            Frame.Layout.Move_Item (Frame.Fragment);
            Frame.Start_X := Event.X_Root;
            Frame.Start_Y := Event.Y_Root;
         end;
      end if;
      return True;
   end On_Title_Motion;

   -----------------------
   -- Set_Corner_Widget --
   -----------------------

   overriding procedure Set_Corner_Widget
     (Fragment  : in out Root_Gtk_Frame_Record;
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
      Fragment.Attach
        (Widget, Left (Corner), Top (Corner), 1, 1);
   end Set_Corner_Widget;

   ---------------------
   -- Set_Side_Widget --
   ---------------------

   overriding procedure Set_Side_Widget
     (Fragment  : in out Root_Gtk_Frame_Record;
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
      Fragment.Attach
        (Widget, Attach_Left (Edge), Attach_Top (Edge),
         Width (Edge), Height (Edge));
   end Set_Side_Widget;

end Komnenos.UI.Gtk_UI.Frames;
