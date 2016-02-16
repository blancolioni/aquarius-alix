with Ada.Characters.Latin_1;
with Ada.Text_IO;

with Glib.Object;
with Glib.Properties;
with Glib.Values;

with Gdk.Cursor;
with Gdk.Event;
with Gdk.Rectangle;
with Gdk.RGBA;
with Gdk.Types;
with Gdk.Window;

with Gtk.Enums;
with Gtk.Handlers;

with Gtk.Text_Iter;
with Gtk.Text_Tag;
with Gtk.Text_Tag_Table;

with Cairo;

with Pango.Enums;
with Pango.Font;

with Aquarius.Colours.Gtk_Colours;
with Aquarius.Fonts;
with Aquarius.Layout;
with Aquarius.Themes;

with Aquarius.Keys.Gtk_Keys;

with Komnenos.Commands;

package body Komnenos.UI.Gtk_UI.Text is

   package Text_View_Event_Handler is
     new Gtk.Handlers.User_Return_Callback
       (Widget_Type => Gtk.Text_View.Gtk_Text_View_Record,
        User_Type   => Komnenos_Text_View,
        Return_Type => Boolean);

   function Text_View_Button_Press_Handler
     (Widget    : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event     : Gdk.Event.Gdk_Event;
      Text_View : Komnenos_Text_View)
      return Boolean;

   function Text_View_Button_Release_Handler
     (Widget    : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event     : Gdk.Event.Gdk_Event;
      Text_View : Komnenos_Text_View)
      return Boolean;

   function Text_View_Motion_Handler
     (Widget    : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event     : Gdk.Event.Gdk_Event;
      Text_View : Komnenos_Text_View)
      return Boolean;

   function Text_View_Draw_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean;

   procedure Text_View_Insert_At_Cursor
     (Self : access Glib.Object.GObject_Record'Class;
      Text : Glib.UTF8_String) with Unreferenced;

   function Text_View_Key_Press
     (Self : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key)
      return Boolean;

   procedure Text_View_Cursor_Move
     (Widget : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Text   : Komnenos_Text_View)
     with Unreferenced;

   procedure Render_Text
     (View : Gtk.Text_View.Gtk_Text_View;
      Fragment : Komnenos.Fragments.Fragment_Type);

   function Get_Tag_Entry
     (Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      Style  : Aquarius.Styles.Aquarius_Style)
      return Gtk.Text_Tag.Gtk_Text_Tag;
   --  return a text tag corresponding to the given Style.  Create a new
   --  text tag if necessary.

   procedure Move_Cursor
     (Text_View : Komnenos_Text_View) with Unreferenced;

   procedure Update_Cursor
     (Text_View : Komnenos_Text_View);

   procedure Follow_If_Link
     (Text_View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
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

   procedure Set_Text_State
     (Text_View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      State     : Aquarius.Themes.Element_State);

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

   ----------------------
   -- Create_Text_View --
   ----------------------

   function Create_Text_View
     (Fragment : Komnenos.Fragments.Fragment_Type)
      return Komnenos_Text_View
   is
      Result : constant Komnenos_Text_View :=
                 new Komnenos_Text_View_Record;
   begin
      Gtk.Text_View.Initialize (Result);
      Result.Fragment := Fragment;
      --  Gtk.Text_View.Gtk_New (Result.Text);
      Result.Text := Gtk.Text_View.Gtk_Text_View (Result);

      Result.Text.Override_Background_Color
        (Gtk.Enums.Gtk_State_Flag_Normal,
         Aquarius.Colours.Gtk_Colours.To_Gdk_RGBA
           (Fragment.Background_Colour));

      declare
         use Aquarius.Fonts;
         use Pango.Font;
         Style : constant Aquarius.Styles.Aquarius_Style :=
                   Aquarius.Themes.Active_Theme.Default_Style;
         Font  : constant Aquarius_Font := Style.Font;
         Font_Name  : constant String := Name (Font);
         Font_Size  : constant Natural := Size (Font);
         Desc  : Pango_Font_Description :=
                        To_Font_Description (Font_Name,
                                             Size => Glib.Gint (Font_Size));
      begin
         if Is_Bold (Font) then
            Set_Weight (Desc, Pango.Enums.Pango_Weight_Bold);
         end if;
         if Is_Italic (Font) then
            Set_Style (Desc, Pango.Enums.Pango_Style_Italic);
         end if;

         Result.Text.Modify_Font (Desc);
         Free (Desc);
      end;

      Result.Text.Set_Size_Request
        (Glib.Gint (Fragment.Width - 6),
         Glib.Gint (Fragment.Height - 4));

      Render_Text (Result.Text, Fragment);

      declare
         use Gdk.Event;
      begin
         Result.Text.Add_Events
           (Button_Press_Mask or Button_Release_Mask
            or Pointer_Motion_Mask or Key_Press_Mask or Key_Release_Mask);
      end;

      Text_View_Event_Handler.Connect
        (Result.Text, Gtk.Widget.Signal_Button_Release_Event,
         Text_View_Event_Handler.To_Marshaller
           (Text_View_Button_Release_Handler'Access),
         Result);

      Text_View_Event_Handler.Connect
        (Result.Text, Gtk.Widget.Signal_Button_Press_Event,
         Text_View_Event_Handler.To_Marshaller
           (Text_View_Button_Press_Handler'Access),
         Result);

      Text_View_Event_Handler.Connect
        (Result.Text, Gtk.Widget.Signal_Motion_Notify_Event,
         Text_View_Event_Handler.To_Marshaller
           (Text_View_Motion_Handler'Access),
         Result);

      Result.Text.On_Draw
        (Text_View_Draw_Handler'Access);

      Result.Text.On_Key_Press_Event
        (Text_View_Key_Press'Access,
         Result);

      Result.Buffer := Result.Text.Get_Buffer;

--        Result.Set_Min_Content_Height (Glib.Gint (Fragment.Height));
--        Result.Set_Min_Content_Width (Glib.Gint (Fragment.Width));

--        Result.Add (Result.Text);

      declare
         Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      begin
         Result.Buffer.Get_Start_Iter (Start_Iter);
         Result.Buffer.Place_Cursor (Start_Iter);
      end;

      return Result;

   end Create_Text_View;

   --------------------
   -- Follow_If_Link --
   --------------------

   procedure Follow_If_Link
     (Text_View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      use type Komnenos.Entities.Entity_Reference;
      Display  : constant Komnenos_Text_View :=
                   Komnenos_Text_View (Text_View);
      Entity : constant Komnenos.Entities.Entity_Reference :=
                 Display.Fragment.Get_Link
                   (Natural (Gtk.Text_Iter.Get_Offset (Iter)) + 1);
   begin
      if Entity /= null then
         declare
            Location : Gdk.Rectangle.Gdk_Rectangle;
         begin
            Text_View.Get_Iter_Location (Iter, Location);
            Entity.Select_Entity
              (Current_UI, Display.Fragment, null,
               Natural (Location.Y) + Natural (Location.Height) / 2);
            Display.Highlights.Append
              ((Entity, Location.Y, Location.Height,
               Aquarius.Colours.From_String
                 ("rgba(220,220,220, 180)")));
         end;
      end if;
   end Follow_If_Link;

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
            Font       : constant Aquarius_Font := Style.Font;
            Font_Name  : constant String := Aquarius.Fonts.Name (Font);
            Font_Size  : constant Natural := Size (Font);
            Desc       : constant Pango_Font_Description :=
                           To_Font_Description
                             (Font_Name,
                              Size => Glib.Gint (Font_Size));
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
                  Foreground : Gdk.RGBA.Gdk_RGBA;
                  Success    : Boolean;
               begin
                  Gdk.RGBA.Parse
                    (Foreground, Aquarius.Colours.To_String (Colour), Success);
                  if not Success then
                     Foreground := (0.0, 0.0, 0.0, 1.0);
                  end if;

                  Gdk.RGBA.Set_Property
                    (Result, Foreground_Rgba_Property, Foreground);
               end;
            end if;

            if Has_Background (Font) then
               declare
                  use Glib;
                  use Aquarius.Colours;
                  Colour : constant Aquarius_Colour :=
                             Get_Foreground (Font);
                  Background : Gdk.RGBA.Gdk_RGBA;
                  Success    : Boolean;
               begin
                  Gdk.RGBA.Parse
                    (Background, Aquarius.Colours.To_String (Colour), Success);
                  if not Success then
                     Background := (1.0, 1.0, 1.0, 1.0);
                  end if;

                  Gdk.RGBA.Set_Property
                    (Result, Background_Rgba_Property, Background);
               end;
            end if;

         end;
      end if;
      return Result;
   end Get_Tag_Entry;

   -----------------
   -- Move_Cursor --
   -----------------

   procedure Move_Cursor
     (Text_View : Komnenos_Text_View)
   is
      use Glib, Gtk.Text_Buffer, Gtk.Text_Iter;
      Iter    : Gtk_Text_Iter;
      Line    : Gint;
      Column  : Gint;
   begin
      Text_View.Buffer.Get_Iter_At_Mark
        (Iter, Text_View.Buffer.Get_Insert);

      Line := Get_Line (Iter);
      Column := Get_Line_Offset (Iter);
      Ada.Text_IO.Put_Line ("cursor move:" & Line'Img & Column'Img);

      Text_View.Fragment.On_Cursor_Move
        ((Aquarius.Layout.Positive_Count (Line + 1),
         Aquarius.Layout.Positive_Count (Column + 1)));

   end Move_Cursor;

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
      Cairo.Set_Line_Width (Context, Width => 1.0);
      Cairo.Rectangle (Context,
                       Gdouble (Line_Rect.X), Gdouble (Line_Rect.Y) + 0.5,
                       Gdouble (Line_Rect.Width),
                       Gdouble (Line_Rect.Height - 1));
      Cairo.Stroke_Preserve (Context);
      Cairo.Fill (Context);
   end Paint_Line_Background;

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
      Fragment.Clear;
      Fragment.Get_Content.Render (Fragment);
      Fragment.Iterate (Put'Access, New_Line'Access);
   end Render_Text;

   --------------------
   -- Set_Text_State --
   --------------------

   procedure Set_Text_State
     (Text_View : not null access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      State     : Aquarius.Themes.Element_State)
   is
      use Aquarius.Styles;

      Display : constant Komnenos_Text_View :=
                  Komnenos_Text_View (Text_View);
      Fragment : constant Komnenos.Fragments.Fragment_Type :=
                   Display.Fragment;
      Style         : Aquarius_Style;
      Current_Style : Aquarius_Style;
      Offset        : constant Positive :=
                        Natural (Gtk.Text_Iter.Get_Offset (Iter)) + 1;
      Start_Offset  : Positive;
      Finish_Offset : Positive;
   begin
      Fragment.Get_Style (State, Offset,
                          Style, Start_Offset, Finish_Offset);

      if Start_Offset = Display.Hover_Start
        and then Finish_Offset = Display.Hover_Finish
      then
         return;
      end if;

      if Style = null then
         return;
      end if;

      declare
         Window : constant Gdk.Gdk_Window :=
                    Text_View.Get_Window (Gtk.Enums.Text_Window_Text);
      begin
         case Mouse_Cursor (Style) is
            when Default =>
               Gdk.Window.Set_Cursor (Window,
                                      Gdk.Cursor.Gdk_Cursor_New
                                        (Gdk.Cursor.Xterm));
            when Hand =>
               Ada.Text_IO.Put_Line ("hand mouse cursor");
               Gdk.Window.Set_Cursor (Window,
                                      Gdk.Cursor.Gdk_Cursor_New
                                        (Gdk.Cursor.Hand2));
         end case;
         --  Gdk.Main.Flush;
      end;

      if Display.Hover_Start /= 0 then
         Apply_Style_To_Text
           (Text_View, Display.Hover_Start, Display.Hover_Finish,
            Display.Hover_Style, Remove => True);
         Display.Hover_Start := 0;
         Display.Hover_Finish := 0;
      end if;

      Current_Style :=
        Fragment.Get_Style (Aquarius.Themes.Normal, Offset);

      if Current_Style = Style then
         return;
      end if;

      Display.Hover_Start := Start_Offset;
      Display.Hover_Finish := Finish_Offset;
      Display.Hover_Style := Style;

      Apply_Style_To_Text
        (Text_View, Start_Offset, Finish_Offset, Style,
         Remove => False);

   end Set_Text_State;

   ------------------------------------
   -- Text_View_Button_Press_Handler --
   ------------------------------------

   function Text_View_Button_Press_Handler
     (Widget    : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event     : Gdk.Event.Gdk_Event;
      Text_View : Komnenos_Text_View)
      return Boolean
   is
      pragma Unreferenced (Widget);
      X, Y : Glib.Gint;
      Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      Line, Column : Glib.Gint;
   begin
      Text_View.Grab_Focus;
      Text_View.Text.Window_To_Buffer_Coords
        (Gtk.Enums.Text_Window_Widget,
         Glib.Gint (Event.Button.X), Glib.Gint (Event.Button.Y),
         X, Y);
      Text_View.Text.Get_Iter_At_Location (Iter, X, Y);
--        Text_View.Buffer.Place_Cursor (Iter);
      Line := Gtk.Text_Iter.Get_Line (Iter);
      Column := Gtk.Text_Iter.Get_Line_Offset (Iter);

      declare
         use Aquarius.Layout;
         use Glib;
      begin
         Text_View.Fragment.Get_Content.Execute_Command
           ((Komnenos.Commands.Set_Cursor_Command,
            (Positive_Count (Line + 1),
             Positive_Count (Column + 1))));
      end;

      return False;
   end Text_View_Button_Press_Handler;

   --------------------------------------
   -- Text_View_Button_Release_Handler --
   --------------------------------------

   function Text_View_Button_Release_Handler
     (Widget    : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event     : Gdk.Event.Gdk_Event;
      Text_View : Komnenos_Text_View)
      return Boolean
   is
      pragma Unreferenced (Widget);
      use type Glib.Gint;
      use type Gdk.Types.Gdk_Modifier_Type;
      use Gtk.Text_Iter;
      Buffer : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
                 Text_View.Buffer;
      Have_Selection : Boolean;
      Start, Finish : Gtk_Text_Iter;
      Iter : Gtk_Text_Iter;
      X, Y : Glib.Gint;
      Active : constant Boolean :=
                 (Event.Button.State and Gdk.Types.Control_Mask) /= 0;
   begin
      Buffer.Get_Selection_Bounds (Start, Finish, Have_Selection);
      if Have_Selection
        and then Get_Offset (Start) /= Get_Offset (Finish)
      then
         return False;
      end if;
      Text_View.Text.Window_To_Buffer_Coords
        (Gtk.Enums.Text_Window_Widget,
         Glib.Gint (Event.Button.X), Glib.Gint (Event.Button.Y),
         X, Y);

      Text_View.Text.Get_Iter_At_Location (Iter, X, Y);

      if Active then
         Follow_If_Link (Text_View.Text, Iter);
      end if;

      return False;
   end Text_View_Button_Release_Handler;

   ---------------------------
   -- Text_View_Cursor_Move --
   ---------------------------

   procedure Text_View_Cursor_Move
     (Widget : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Text   : Komnenos_Text_View)
   is
      pragma Unreferenced (Event);
      use Glib, Gtk.Text_Buffer, Gtk.Text_Iter;
      Iter    : Gtk_Text_Iter;
      Line    : Gint;
      Column  : Gint;
   begin
      Widget.Get_Iter_At_Mark (Iter, Widget.Get_Insert);
      Line := Get_Line (Iter);
      Column := Get_Line_Offset (Iter);
      Ada.Text_IO.Put_Line ("cursor move:" & Line'Img & Column'Img);
      Text.Fragment.On_Cursor_Move
        ((Aquarius.Layout.Positive_Count (Line + 1),
         Aquarius.Layout.Positive_Count (Column + 1)));

      if Text.Fragment.Needs_Render then
         Ada.Text_IO.Put_Line ("updated");
         Widget.Set_Text ("");
         Render_Text (Text.Text, Text.Fragment);
         Text.Fragment.Rendered;
      end if;
   end Text_View_Cursor_Move;

   ----------------------------
   -- Text_View_Draw_Handler --
   ----------------------------

   function Text_View_Draw_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean
   is
      use Glib;
      Display      : constant Komnenos_Text_View :=
                       Komnenos_Text_View (Widget);
      Text_View    : constant Gtk.Text_View.Gtk_Text_View :=
                       Display.Text;
      Iter         : Gtk.Text_Iter.Gtk_Text_Iter;
      Buffer       : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
                       Text_View.Get_Buffer;
      Location     : Gdk.Rectangle.Gdk_Rectangle;

   begin

      Buffer.Get_Iter_At_Mark
        (Iter, Buffer.Get_Insert);
      Text_View.Get_Iter_Location
        (Iter, Location);

      for Highlight of Display.Highlights loop
         Paint_Line_Background
           (View    => Text_View,
            Context => Cr,
            Y       => Highlight.Line_Start_Pixels + 1,
            Height  => Highlight.Line_Height_Pixels - 1,
            Colour  =>
              Aquarius.Colours.Gtk_Colours.To_Gdk_RGBA
                (Highlight.Highlight_Colour));
      end loop;

      Paint_Line_Background
        (View    => Text_View,
         Context => Cr,
         Y       => Location.Y + 1,
         Height  => Location.Height - 1,
         Colour  => (0.8, 0.5, 0.5, 0.8));

      return False;

   end Text_View_Draw_Handler;

   --------------------------------
   -- Text_View_Insert_At_Cursor --
   --------------------------------

   procedure Text_View_Insert_At_Cursor
     (Self : access Glib.Object.GObject_Record'Class;
      Text : Glib.UTF8_String)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put_Line ("insert: [" & Text & "]");
   end Text_View_Insert_At_Cursor;

   -------------------------
   -- Text_View_Key_Press --
   -------------------------

   function Text_View_Key_Press
     (Self : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key)
      return Boolean
   is
      use type Aquarius.Keys.Aquarius_Key;
      Text : constant Komnenos_Text_View :=
               Komnenos_Text_View (Self);
      Key  : constant Aquarius.Keys.Aquarius_Key :=
               Aquarius.Keys.Gtk_Keys.To_Aquarius_Key
                 (Event.Keyval, Event.State);
   begin
      if Key /= Aquarius.Keys.Null_Key then
         Text.Fragment.On_Key_Press (Key);
         if Text.Fragment.Needs_Render then
            Ada.Text_IO.Put_Line ("updated");
            Text.Buffer.Set_Text ("");
            Render_Text (Text.Text, Text.Fragment);
         elsif Text.Fragment.Text_Inserted then
            Text.Buffer.Insert_At_Cursor
              (Text.Fragment.Inserted_Text);
         end if;

         if Text.Fragment.Needs_Render
           or else Text.Fragment.Cursor_Moved
         then
            Update_Cursor (Text);
         end if;

         Text.Fragment.Rendered;

      end if;
      return True;
   end Text_View_Key_Press;

   ------------------------------
   -- Text_View_Motion_Handler --
   ------------------------------

   function Text_View_Motion_Handler
     (Widget    : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event     : Gdk.Event.Gdk_Event;
      Text_View : Komnenos_Text_View)
      return Boolean
   is
      use type Glib.Gint;
      use type Gdk.Types.Gdk_Modifier_Type;
      use Gtk.Text_Iter;
      Buffer : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
                 Text_View.Buffer;
      Have_Selection : Boolean;
      Start, Finish : Gtk_Text_Iter;
      Iter : Gtk_Text_Iter;
      X, Y : Glib.Gint;
      Active : constant Boolean :=
                 (Event.Motion.State and Gdk.Types.Control_Mask) /= 0;
   begin

      Buffer.Get_Selection_Bounds (Start, Finish, Have_Selection);
      if Have_Selection
        and then Get_Offset (Start) /= Get_Offset (Finish)
      then
         return False;
      end if;

      Widget.Window_To_Buffer_Coords
        (Gtk.Enums.Text_Window_Widget,
         Glib.Gint (Event.Button.X), Glib.Gint (Event.Button.Y),
         X, Y);
      Widget.Get_Iter_At_Location (Iter, X, Y);

      if Active then
         Set_Text_State (Widget, Iter, Aquarius.Themes.Hover);
      else
         Set_Text_State (Widget, Iter, Aquarius.Themes.Normal);
      end if;

      return False;
   end Text_View_Motion_Handler;

   -------------------
   -- Update_Cursor --
   -------------------

   procedure Update_Cursor
     (Text_View : Komnenos_Text_View)
   is
      use Glib;
      use Aquarius.Layout;
      New_Position : constant Position := Text_View.Fragment.Get_Cursor;
      Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Text_View.Buffer.Get_Iter_At_Line_Offset
        (Iter        => Iter,
         Line_Number => Glib.Gint (New_Position.Line) - 1,
         Char_Offset => Glib.Gint (New_Position.Column) - 1);
      Text_View.Buffer.Place_Cursor (Iter);
   end Update_Cursor;

end Komnenos.UI.Gtk_UI.Text;
