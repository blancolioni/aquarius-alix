with Ada.Containers.Vectors;
with Ada.Text_IO;

with Glib.Values;

with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Gtk.Notebook;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_Tag;
with Gtk.Text_View;

with Aquarius.Buffers;
with Aquarius.Styles;

with Aquarius.GUI.Source;
with Aquarius.GUI.Text;

package body Aquarius.GUI.Message_View is

   Output_Book     : Gtk.Notebook.Gtk_Notebook;
   pragma Warnings (Off, Output_Book);

   Buffer_Messages : Gtk.Text_Buffer.Gtk_Text_Buffer;

   type Message_Source is access all Aquarius.Messages.Message_Location'Class;

   package Message_Source_Vector is
      new Ada.Containers.Vectors (Positive, Message_Source);

   Message_Source_List : Message_Source_Vector.Vector;
   Local_Message_List  : Aquarius.Messages.Message_List;

   procedure Handle_Mark_Set
     (Widget   : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Args     : Glib.Values.GValues);

   package Mark_Set_Callback is
      new Gtk.Handlers.Callback (Gtk.Text_Buffer.Gtk_Text_Buffer_Record);

   ------------------------
   -- Add_Message_Source --
   ------------------------

   procedure Add_Message_Source
     (M : access Aquarius.Messages.Message_Location'Class)
   is
      Source : constant Message_Source := Message_Source (M);
   begin
      if not Message_Source_List.Contains (Source) then
         Message_Source_List.Append (Source);
      end if;
      Update;
   end Add_Message_Source;

   ---------------------
   -- Handle_Mark_Set --
   ---------------------

   procedure Handle_Mark_Set
     (Widget   : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Args     : Glib.Values.GValues)
   is
      pragma Unreferenced (Args);
      use Aquarius.Messages;
      use type Glib.Gint;
      Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      Start_Iter, End_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      Tag  : Gtk.Text_Tag.Gtk_Text_Tag;
      Line : Natural;
      TB   : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
        Gtk.Text_Buffer.Gtk_Text_Buffer (Widget);
   begin

      Widget.Get_Start_Iter (Start_Iter);
      Widget.Get_End_Iter (End_Iter);
      Tag := Aquarius.GUI.Text.Default_Tag_Entry (TB);
      Widget.Remove_All_Tags (Start_Iter, End_Iter);
      Widget.Apply_Tag (Tag, Start_Iter, End_Iter);

      Widget.Get_Iter_At_Mark (Iter, Widget.Get_Insert);
      Gtk.Text_Iter.Copy (Iter, Start_Iter);
      Gtk.Text_Iter.Copy (Iter, End_Iter);

      if Gtk.Text_Iter.Get_Chars_In_Line (End_Iter) > 0 then

         Gtk.Text_Iter.Set_Line_Offset (Start_Iter, 0);
         Gtk.Text_Iter.Set_Line_Offset
           (End_Iter,
            Gtk.Text_Iter.Get_Chars_In_Line (End_Iter) - 1);
         Tag :=
           Aquarius.GUI.Text.Get_Tag_Entry (TB, "selected_error",
                                            Aquarius.Styles.Default_Style);

         Widget.Apply_Tag (Tag, Start_Iter, End_Iter);
      end if;

      Line := Natural (Gtk.Text_Iter.Get_Line (Iter));

      declare
         Msg    : constant Message :=
           Get_Message (Local_Message_List, Line + 1);
         Loc    : constant access Message_Location'Class :=
           Get_Location (Msg);
         Loc_Name : constant String := Loc.Location_Name;
         Loc_Line : constant Positive := Loc.Location_Line;
         Loc_Col  : constant Positive := Loc.Location_Column;
         Buffer   : constant Aquarius.Buffers.Aquarius_Buffer :=
           Current_Project.Get_Buffer (Loc_Name, False);
      begin
         Ada.Text_IO.Put_Line ("Handle_Mark_Set:" &
                               Loc_Line'Img & Loc_Col'Img);
         Aquarius.GUI.Source.Load_Buffer (Buffer, Loc_Line, Loc_Col);
      end;
   end Handle_Mark_Set;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder) is
   begin
      Output_Book :=
        Gtk.Notebook.Gtk_Notebook
          (Builder.Get_Object ("Output_Book"));
      Gtk.Text_Buffer.Gtk_New (Buffer_Messages);

      declare
         T : constant Gtk.Text_View.Gtk_Text_View :=
               Gtk.Text_View.Gtk_Text_View
                 (Builder.Get_Object ("Messages"));
      begin
         T.Set_Buffer (Buffer_Messages);
         T.Modify_Font (Aquarius.GUI.Text.Default_Font);
      end;
      Aquarius.Messages.Create_Message_List (Local_Message_List, True);

      Mark_Set_Callback.Connect
        (Widget => Buffer_Messages,
         Name   => "mark_set",
         Cb     => Handle_Mark_Set'Access);

   end Initialise;

   ---------------------------
   -- Remove_Message_Source --
   ---------------------------

   procedure Remove_Message_Source
     (M : access Aquarius.Messages.Message_Location'Class)
   is
   begin
      for I in 1 .. Message_Source_List.Last_Index loop
         if Message_Source_List.Element (I) = Message_Source (M) then
            Message_Source_List.Delete (I);
            exit;
         end if;
      end loop;
      Update;
   end Remove_Message_Source;

   ------------
   -- Update --
   ------------

   procedure Update is
      use Aquarius.Messages;
   begin
      Clear_Message_List (Local_Message_List);
      Buffer_Messages.Set_Text ("");
      for I in 1 .. Message_Source_List.Last_Index loop
         declare
            List : Message_List;
         begin
            Message_Source_List.Element (I).Get_Messages (List);
            Copy_Message_List (List, Local_Message_List);
         end;
      end loop;
      for I in 1 .. Message_Count (Local_Message_List) loop
         Buffer_Messages.Insert_At_Cursor
           (Show (Get_Message (Local_Message_List, I)) &
              Character'Val (10));
      end loop;
   end Update;

end Aquarius.GUI.Message_View;
