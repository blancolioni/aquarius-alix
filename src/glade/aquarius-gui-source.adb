with Ada.Containers.Vectors;

with Glib;                              use Glib;
with Gdk.Event;
with Gdk.Types;
with Gdk.Types.Keysyms;

with Gtk.Enums;
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Gtk.Label;
with Gtk.Notebook;
with Gtk.Scrolled_Window;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_View;

with Gtk.Widget;

with Aquarius.Keys;
with Aquarius.Layout;
with Aquarius.Rendering.GUI;
with Aquarius.Styles;

with Aquarius.GUI.Menu;
with Aquarius.GUI.Message_View;
with Aquarius.GUI.Text;

package body Aquarius.GUI.Source is

   Source_Book : Gtk.Notebook.Gtk_Notebook;

   type Source_Info_Type is
      record
         Text_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Text_View   : Gtk.Text_View.Gtk_Text_View;
      end record;

   package Source_Info_Vectors is
      new Ada.Containers.Vectors (Positive, Source_Info_Type);

   Source_Info : Source_Info_Vectors.Vector;

   procedure Create_Source_View
     (Buffer : Aquarius.Buffers.Aquarius_Buffer);

   function To_Aquarius_Key (Event : Gdk.Event.Gdk_Event)
                            return Aquarius.Keys.Aquarius_Key;

   function Handle_Key_Press
     (View   : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event;
      Buffer : in     Aquarius.Buffers.Aquarius_Buffer)
     return Boolean;

   package Key_Press_Callback is
      new Gtk.Handlers.User_Return_Callback
     (Gtk.Text_View.Gtk_Text_View_Record, Boolean,
      Aquarius.Buffers.Aquarius_Buffer);

   procedure Handle_Cursor_Movement
     (Widget : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Event  : in Gdk.Event.Gdk_Event;
      Buffer : in     Aquarius.Buffers.Aquarius_Buffer);

   package Cursor_Movement_Callback is
     new Gtk.Handlers.User_Callback
       (Gtk.Text_Buffer.Gtk_Text_Buffer_Record,
        Aquarius.Buffers.Aquarius_Buffer);

   ------------------------
   -- Create_Source_View --
   ------------------------

   procedure Create_Source_View
     (Buffer : Aquarius.Buffers.Aquarius_Buffer)
   is
      Page_Label  : Gtk.Label.Gtk_Label;
      Scrolled    : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Text_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      Text_View   : Gtk.Text_View.Gtk_Text_View;
      Renderer    : Aquarius.Rendering.Aquarius_Renderer;
   begin

      Gtk.Label.Gtk_New (Page_Label, Buffer.Name);
      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy (Gtk.Enums.Policy_Automatic,
                           Gtk.Enums.Policy_Automatic);
      Gtk.Text_Buffer.Gtk_New (Text_Buffer);
      Gtk.Text_View.Gtk_New (Text_View, Text_Buffer);
      Scrolled.Add (Text_View);
      Key_Press_Callback.Connect
        (Text_View, Gtk.Widget.Signal_Key_Press_Event,
         Key_Press_Callback.To_Marshaller (Handle_Key_Press'Access),
         Buffer);
      Text_View.Modify_Font (Aquarius.GUI.Text.Default_Font);
      Renderer :=
        Aquarius.Rendering.GUI.New_GUI_Renderer (Text_Buffer);
      Renderer.Set_Style (Aquarius.Styles.Default_Style);
      Buffer.Set_Current_Render (Renderer);
      Buffer.Render;
      Page_Label.Show;
      Scrolled.Show_All;
      Source_Book.Append_Page (Scrolled, Page_Label);
      Source_Book.Set_Current_Page (-1);
      Aquarius.GUI.Message_View.Add_Message_Source (Buffer);
      Text_View.Grab_Focus;
      Source_Info.Append ((Text_Buffer => Text_View.Get_Buffer,
                           Text_View   => Text_View));
      Cursor_Movement_Callback.Connect
        (Text_Buffer, "notify::cursor-position",
         Cursor_Movement_Callback.To_Marshaller
           (Handle_Cursor_Movement'Access),
         Buffer);

   end Create_Source_View;

   ----------------------------
   -- Handle_Cursor_Movement --
   ----------------------------

   procedure Handle_Cursor_Movement
     (Widget : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Event  : in Gdk.Event.Gdk_Event;
      Buffer : in     Aquarius.Buffers.Aquarius_Buffer)
   is
      pragma Unreferenced (Event);
      use Aquarius.Layout;
      use type Aquarius.Keys.Aquarius_Key;
      Cursor_Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Line, Col     : Gint;
   begin
      Widget.Get_Iter_At_Mark (Cursor_Iter, Widget.Get_Insert);
      Line := Gtk.Text_Iter.Get_Line (Cursor_Iter);
      Col  := Gtk.Text_Iter.Get_Line_Offset (Cursor_Iter);
      Buffer.Set_Point ((Count (Line) + 1, Count (Col) + 1));
   end Handle_Cursor_Movement;

   ----------------------
   -- Handle_Key_Press --
   ----------------------

   function Handle_Key_Press
     (View   : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event;
      Buffer : in     Aquarius.Buffers.Aquarius_Buffer)
     return Boolean
   is
      pragma Unreferenced (View);
      use type Aquarius.Keys.Aquarius_Key;
      Key : constant Aquarius.Keys.Aquarius_Key :=
              To_Aquarius_Key (Event);
      Handled : Boolean := False;
   begin
      if Key /= Aquarius.Keys.Null_Key then
         Handled := Buffer.On_Key (Key);
      end if;
      return Handled;
   end Handle_Key_Press;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder) is
   begin
      Source_Book :=
        Gtk.Notebook.Gtk_Notebook
        (Builder.Get_Object ("Source_Book"));
      while Source_Book.Get_N_Pages > 0 loop
         Source_Book.Remove_Page (0);
      end loop;
   end Initialise;

   -----------------
   -- Load_Buffer --
   -----------------

   procedure Load_Buffer (Buffer : Aquarius.Buffers.Aquarius_Buffer;
                          Line   : Natural := 0;
                          Col    : Natural := 0)
   is
      Create_View : Boolean := True;
      Index       : Positive;
   begin
      for I in 0 .. Source_Book.Get_N_Pages - 1 loop
         declare
            Label : constant String :=
              Source_Book.Get_Tab_Label_Text (Source_Book.Get_Nth_Page (I));
         begin
            if Label = Buffer.Name then
               Source_Book.Set_Current_Page (I);
               Create_View := False;
               Index := Natural (I) + 1;
               exit;
            end if;
         end;
      end loop;

      if Create_View then
         Create_Source_View (Buffer);
         Index := Positive (Source_Book.Get_N_Pages);
      end if;

      if Line /= 0 and then Col /= 0 then
         declare
            TB    : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
              Source_Info.Element (Index).Text_Buffer;
            TV    : constant Gtk.Text_View.Gtk_Text_View :=
              Source_Info.Element (Index).Text_View;
            Where : Gtk.Text_Iter.Gtk_Text_Iter;
         begin
            TB.Get_Iter_At_Line_Offset (Where, Glib.Gint (Line - 1),
                                        Glib.Gint (Col - 1));
            TB.Place_Cursor (Where);
            TV.Scroll_Mark_Onscreen (TB.Get_Insert);
            TV.Grab_Focus;
         end;
      end if;

      Aquarius.GUI.Menu.Show_Plugin_Menu (Buffer.Grammar.Get_Menu);

   end Load_Buffer;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File (Path : String) is
      Buffer     : constant Aquarius.Buffers.Aquarius_Buffer :=
        Aquarius.Buffers.Load_Buffer_From_File (Current_UI, Path);
   begin
      Create_Source_View (Buffer);
   end Load_File;

   --------------
   -- New_File --
   --------------

   procedure New_File (Grammar_Name : String) is
   begin
      Create_Source_View
        (Aquarius.Buffers.New_Empty_Buffer (Current_UI, Grammar_Name));
   end New_File;

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

end Aquarius.GUI.Source;
