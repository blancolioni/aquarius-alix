with Glib;

with Gdk.Event;
with Gdk.Types.Keysyms;

with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Text_Iter;
with Gtk.Text_View;

with Aquarius.GUI.Text;
with Aquarius.Rendering.GUI;
with Aquarius.Styles;

package body Aquarius.Fragments.Text is

   function To_Aquarius_Key (Event : Gdk.Event.Gdk_Event)
                             return Aquarius.Keys.Aquarius_Key;

   function Handle_Key_Press
     (View     : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event;
      Fragment : in     Aquarius_Fragment)
     return Boolean;

   package Key_Press_Callback is
      new Gtk.Handlers.User_Return_Callback
       (Gtk.Text_View.Gtk_Text_View_Record, Boolean,
        Aquarius_Fragment);

   -------------------
   -- Create_Widget --
   -------------------

   overriding
   procedure Create_Widget
     (Fragment : not null access Text_Fragment)
   is
      Text_View : Gtk.Text_View.Gtk_Text_View;
   begin
      Gtk.Text_Buffer.Gtk_New (Fragment.Text_Buffer);
      Fragment.Renderer :=
        Aquarius.Rendering.GUI.New_GUI_Renderer
          (Fragment.Text_Buffer);
      Fragment.Renderer.Set_Style
        (Aquarius.Styles.Default_Style);
      Gtk.Text_View.Gtk_New (Text_View);
      Text_View.Set_Size_Request (Width  => Glib.Gint (Fragment.Width),
                                  Height => Glib.Gint (Fragment.Height));
      Text_View.Modify_Font (Aquarius.GUI.Text.Default_Font);
      Text_View.Modify_Bg
        (Gtk.Enums.State_Normal,
         Aquarius.GUI.Text.Get_Gdk_Colour
           (Fragment.Background));

      Key_Press_Callback.Connect
        (Text_View, "key_press_event",
         Key_Press_Callback.To_Marshaller (Handle_Key_Press'Access),
         Aquarius_Fragment (Fragment));

      Text_View.Set_Buffer (Fragment.Text_Buffer);
      Text_View.Set_Wrap_Mode (Wrap_Mode => Gtk.Enums.Wrap_Word);
      Text_View.Set_Border_Width (5);
      Text_View.Show;
      Fragment.Widget := Gtk.Widget.Gtk_Widget (Text_View);
   end Create_Widget;

   ----------------------
   -- Handle_Key_Press --
   ----------------------

   function Handle_Key_Press
     (View     : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event;
      Fragment : in     Aquarius_Fragment)
     return Boolean
   is
      use Aquarius.Layout;
      use type Glib.Gint;
      use type Aquarius.Keys.Aquarius_Key;
      Key : constant Aquarius.Keys.Aquarius_Key :=
        To_Aquarius_Key (Event);
      TB  : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
        View.Get_Buffer;
      Cursor_Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Line, Col     : Glib.Gint;
      Position      : Aquarius.Layout.Position;
   begin
      TB.Get_Iter_At_Mark (Cursor_Iter, TB.Get_Insert);
      Line := Gtk.Text_Iter.Get_Line (Cursor_Iter) + 1;
      Col  := Gtk.Text_Iter.Get_Line_Offset (Cursor_Iter) + 1;
      Position := (Line => Positive_Count (Line),
                   Column => Positive_Count (Col));
      if Key /= Aquarius.Keys.Null_Key then
         return Fragment.On_Key_Press (Position, Key);
      end if;
      return True;
   end Handle_Key_Press;

   ---------------------
   -- To_Aquarius_Key --
   ---------------------

   function To_Aquarius_Key (Event : Gdk.Event.Gdk_Event)
                            return Aquarius.Keys.Aquarius_Key
   is
      use Gdk.Types, Gdk.Types.Keysyms;
      Key      : constant Gdk_Key_Type :=
        Gdk.Event.Get_Key_Val (Event);
      Modifier : constant Gdk_Modifier_Type := Gdk.Event.Get_State (Event);
      Result   : Aquarius.Keys.Aquarius_Key;
   begin
      if Key in 32 .. 126 then
         Result := Aquarius.Keys.Character_Key (Character'Val (Key));
      elsif Key = GDK_Tab or else Key = GDK_ISO_Left_Tab then
         Result := Aquarius.Keys.Tab;
      elsif Key = GDK_BackSpace then
         Result := Aquarius.Keys.Back_Space;
      elsif Key = GDK_Linefeed or else Key = GDK_Return then
         Result := Aquarius.Keys.Line_Feed;
      else
         return Aquarius.Keys.Null_Key;
      end if;

      return Aquarius.Keys.Modify
        (Key     => Result,
         Shift   => (Modifier and Shift_Mask) = Shift_Mask,
         Control => (Modifier and Control_Mask) = Control_Mask,
         Mod1    => (Modifier and Mod1_Mask) = Mod1_Mask);

   end To_Aquarius_Key;

end Aquarius.Fragments.Text;
