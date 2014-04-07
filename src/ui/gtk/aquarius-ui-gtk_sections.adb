with Ada.Characters.Latin_1;

with Glib;

with Gdk.Event;
with Gdk.Types.Keysyms;

with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_Tag;
with Gtk.Text_View;
with Gtk.Widget;

with Aquarius.Colours.Gtk;
with Aquarius.Keys;
with Aquarius.Layout;

with Aquarius.UI.Gtk_Text;

package body Aquarius.UI.Gtk_Sections is

   Font_Width : constant := 9;
   Font_Height : constant := 15;

   function To_Aquarius_Key (Event : Gdk.Event.Gdk_Event)
                            return Aquarius.Keys.Aquarius_Key;

   function Handle_Key_Press
     (View    : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event   : in     Gdk.Event.Gdk_Event;
      Section : Gtk_Section)
     return Boolean;

   package Key_Press_Callback is
      new Gtk.Handlers.User_Return_Callback
     (Gtk.Text_View.Gtk_Text_View_Record, Boolean,
      Gtk_Section);

   procedure Handle_Cursor_Movement
     (Widget  : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Event   : in Gdk.Event.Gdk_Event;
      Section : Gtk_Section);

   package Cursor_Movement_Callback is
     new Gtk.Handlers.User_Callback
       (Gtk.Text_Buffer.Gtk_Text_Buffer_Record,
        Gtk_Section);

   procedure Render
     (Item   : not null access Gtk_Section_Record'Class);

   type Gtk_Section_Record is
     new Aquarius.Sections.Section_Display_Interface with
      record
         Section     : Aquarius.Sections.Aquarius_Section;
         Text_View   : Gtk.Text_View.Gtk_Text_View;
         Displayed   : Boolean := False;
      end record;

   overriding procedure Update
     (Display : in out Gtk_Section_Record);

   overriding procedure Set_Point
     (Display : in out Gtk_Section_Record;
      Point   : Aquarius.Layout.Position);

   ------------
   -- Create --
   ------------

   function Create
     (From_Section  : Aquarius.Sections.Aquarius_Section;
      Map           : in out Gtk_Section_Map)
      return Gtk_Section
   is
      Result : constant Gtk_Section :=
                 new Gtk_Section_Record'(Section   => From_Section,
                                         Text_View => null,
                                         Displayed => False);
   begin
      From_Section.Set_Rendered_Size
        (40 * Font_Width,
         20 * Font_Height);

      Map.Map.Insert
        (Ada.Strings.Unbounded.To_Unbounded_String (From_Section.Id),
         Result);
      return Result;
   end Create;

   ------------
   -- Exists --
   ------------

   function Exists
     (Map     : Gtk_Section_Map;
      Name    : String)
      return Boolean
   is
      use Ada.Strings.Unbounded;
   begin
      return Map.Map.Contains (To_Unbounded_String (Name));
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Map     : Gtk_Section_Map;
      Name    : String)
      return Gtk_Section
   is
      use Ada.Strings.Unbounded;
   begin
      return Map.Map.Element (To_Unbounded_String (Name));
   end Get;

   ----------------------
   -- Get_Display_Size --
   ----------------------

   procedure Get_Display_Size
     (Section : Gtk_Section;
      Width, Height : out Natural)
   is
      Alloc : Gtk.Widget.Gtk_Allocation;
   begin
      Section.Text_View.Get_Allocation (Alloc);
      Width := Natural (Alloc.Width);
      Height := Natural (Alloc.Height);
   end Get_Display_Size;

   ----------------------------
   -- Handle_Cursor_Movement --
   ----------------------------

   procedure Handle_Cursor_Movement
     (Widget : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Event  : in Gdk.Event.Gdk_Event;
      Section : Gtk_Section)
   is
      pragma Unreferenced (Event);
      use Aquarius.Layout;
      use Glib;
      use type Aquarius.Keys.Aquarius_Key;
      Cursor_Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Line, Col     : Gint;
   begin
      Widget.Get_Iter_At_Mark (Cursor_Iter, Widget.Get_Insert);
      Line := Gtk.Text_Iter.Get_Line (Cursor_Iter);
      Col  := Gtk.Text_Iter.Get_Line_Offset (Cursor_Iter);
      Section.Section.Set_Point ((Count (Line) + 1, Count (Col) + 1));
   end Handle_Cursor_Movement;

   ----------------------
   -- Handle_Key_Press --
   ----------------------

   function Handle_Key_Press
     (View   : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event;
      Section : Gtk_Section)
     return Boolean
   is
      pragma Unreferenced (View);
      use type Aquarius.Keys.Aquarius_Key;
      Key : constant Aquarius.Keys.Aquarius_Key :=
              To_Aquarius_Key (Event);
      Handled : Boolean := False;
   begin
      if Key /= Aquarius.Keys.Null_Key then
         Section.Section.On_Key (Key, Handled);
      end if;
      return Handled;
   end Handle_Key_Press;

   ------------
   -- Render --
   ------------

   procedure Render
     (Item   : Gtk_Section;
      Target : Gtk.Fixed.Gtk_Fixed;
      X, Y   : Integer)
   is
      use type Gtk.Text_View.Gtk_Text_View;

      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;

      procedure Render_Element (Element : Aquarius.Sections.Section_Element);

      --------------------
      -- Render_Element --
      --------------------

      procedure Render_Element
        (Element : Aquarius.Sections.Section_Element)
      is
         Class     : constant String :=
                       Aquarius.Sections.Class (Element);
         Tag_Entry : Gtk.Text_Tag.Gtk_Text_Tag;
         Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      begin
         if Class = "" then
            Tag_Entry := Aquarius.UI.Gtk_Text.Default_Tag_Entry (Buffer);
         else
            Tag_Entry :=
              Aquarius.UI.Gtk_Text.Get_Tag_Entry
                (Buffer, Class, Item.Section.Style);
         end if;

         Buffer.Get_Iter_At_Mark (Iter, Buffer.Get_Insert);

         Buffer.Insert_With_Tags
           (Iter => Iter,
            Text => Aquarius.Sections.Text (Element),
            Tag  => Tag_Entry);

         if Aquarius.Sections.New_Line (Element) then
            Buffer.Insert_At_Cursor
              ((1 => Ada.Characters.Latin_1.LF));
         end if;
      end Render_Element;

      Start, Finish : Gtk.Text_Iter.Gtk_Text_Iter;
      New_View      : constant Boolean := Item.Text_View = null;
   begin
      if New_View then
         Gtk.Text_View.Gtk_New (Item.Text_View);
         Item.Text_View.Modify_Base
           (Gtk.Enums.State_Normal,
            Aquarius.Colours.Gtk.To_Gdk_Color
              (Item.Section.Background));
         Key_Press_Callback.Connect
           (Item.Text_View, Gtk.Widget.Signal_Key_Press_Event,
            Key_Press_Callback.To_Marshaller (Handle_Key_Press'Access),
            Item);
         Cursor_Movement_Callback.Connect
           (Item.Text_View.Get_Buffer, "notify::cursor-position",
            Cursor_Movement_Callback.To_Marshaller
              (Handle_Cursor_Movement'Access),
            Item);
      end if;

      Item.Section.Set_Display (Item);

      Buffer := Item.Text_View.Get_Buffer;
      Buffer.Get_Start_Iter (Start);
      Buffer.Get_End_Iter (Finish);

      Item.Text_View.Get_Buffer.Delete (Start, Finish);

      Item.Section.Render (Render_Element'Access);

      declare
         Max_Columns : constant Natural := Item.Section.Max_Column_Count;
         Lines       : constant Natural := Item.Section.Line_Count;
      begin
         Item.Section.Set_Rendered_Size
           (Max_Columns * Font_Width,
            Lines * Font_Height);
         Item.Text_View.Set_Size_Request
           (Width  => Glib.Gint (Item.Section.Render_Width),
            Height => Glib.Gint (Item.Section.Render_Height));
      end;

      if Item.Displayed then
         Target.Move (Item.Text_View, Glib.Gint (X), Glib.Gint (Y));
      else
         Target.Put (Item.Text_View, Glib.Gint (X), Glib.Gint (Y));
         Item.Displayed := True;
      end if;

      if New_View then
         Item.Text_View.Show_All;
      end if;

   end Render;

   ------------
   -- Render --
   ------------

   procedure Render
     (Item   : not null access Gtk_Section_Record'Class)
   is
      use type Gtk.Text_View.Gtk_Text_View;
      use Gtk.Text_Buffer, Gtk.Text_Iter;

      TB : constant Gtk_Text_Buffer := Item.Text_View.Get_Buffer;

      procedure Render_Element (Element : Aquarius.Sections.Section_Element);

      --------------------
      -- Render_Element --
      --------------------

      procedure Render_Element
        (Element : Aquarius.Sections.Section_Element)
      is
         Class     : constant String :=
                       Aquarius.Sections.Class (Element);
         Tag_Entry : Gtk.Text_Tag.Gtk_Text_Tag;
         Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      begin
         if Class = "" then
            Tag_Entry := Aquarius.UI.Gtk_Text.Default_Tag_Entry (TB);
         else
            Tag_Entry :=
              Aquarius.UI.Gtk_Text.Get_Tag_Entry
                (TB, Class, Item.Section.Style);
         end if;

         TB.Get_Iter_At_Mark (Iter, TB.Get_Insert);

         TB.Insert_With_Tags
           (Iter => Iter,
            Text => Aquarius.Sections.Text (Element),
            Tag  => Tag_Entry);

         if Aquarius.Sections.New_Line (Element) then
            TB.Insert_At_Cursor
              ((1 => Ada.Characters.Latin_1.LF));
         end if;
      end Render_Element;

      Start, Finish : Gtk_Text_Iter;
   begin
      TB.Begin_User_Action;
      TB.Get_Start_Iter (Start);
      TB.Get_End_Iter (Finish);
      TB.Delete (Start, Finish);
      Item.Section.Render (Render_Element'Access);
      TB.End_User_Action;
   end Render;

   ---------------
   -- Set_Point --
   ---------------

   overriding procedure Set_Point
     (Display : in out Gtk_Section_Record;
      Point   : Aquarius.Layout.Position)
   is
      use Glib;
      use Gtk.Text_Buffer, Gtk.Text_Iter;
      TB   : constant Gtk_Text_Buffer := Display.Text_View.Get_Buffer;
      Iter : Gtk_Text_Iter;
   begin
      TB.Get_Iter_At_Line_Offset (Iter,
                                  Gint (Point.Line) - 1,
                                  Gint (Point.Column) - 1);
      TB.Place_Cursor (Iter);
      Display.Section.Set_Point (Point);
   end Set_Point;

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
      else
         return Aquarius.Keys.Null_Key;
      end if;

      return Aquarius.Keys.Modify
        (Key     => Result,
         Shift   => (Modifier and Shift_Mask) = Shift_Mask,
         Control => (Modifier and Control_Mask) = Control_Mask,
         Mod1    => (Modifier and Mod1_Mask) = Mod1_Mask);

   end To_Aquarius_Key;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Display : in out Gtk_Section_Record)
   is
      use Gtk.Text_Buffer, Gtk.Text_Iter;
      TB : constant Gtk_Text_Buffer := Display.Text_View.Get_Buffer;
      Start, Finish : Gtk_Text_Iter;
   begin
      TB.Begin_User_Action;
      TB.Get_Start_Iter (Start);
      TB.Get_End_Iter (Finish);
      TB.Delete (Start, Finish);
      Render (Display'Access);
      TB.End_User_Action;
   end Update;

end Aquarius.UI.Gtk_Sections;
