with Ada.Characters.Latin_1;

with Glib;

with Gtk.Enums;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_Tag;
with Gtk.Text_View;

with Aquarius.Colours.Gtk;
with Aquarius.UI.Gtk_Text;

package body Aquarius.UI.Gtk_Sections is

   Font_Width : constant := 9;
   Font_Height : constant := 15;

   type Gtk_Section_Record is
      record
         Section     : Aquarius.Sections.Aquarius_Section;
         Text_View   : Gtk.Text_View.Gtk_Text_View;
         Displayed   : Boolean := False;
      end record;

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
      end if;

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

end Aquarius.UI.Gtk_Sections;
