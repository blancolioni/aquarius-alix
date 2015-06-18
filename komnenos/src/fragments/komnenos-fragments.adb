with Ada.Characters.Latin_1;
with Ada.Directories;

package body Komnenos.Fragments is

   function Get_Style_Info
     (Fragment : Root_Fragment_Type;
      Offset   : Positive)
      return Style_Info;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Fragment : in out Root_Fragment_Type) is
      pragma Unreferenced (Fragment);
   begin
      null;
   end Adjust;

   -----------------------
   -- Background_Colour --
   -----------------------

   function Background_Colour
     (Fragment : Root_Fragment_Type)
      return String
   is
   begin
      if Fragment.Background_Colour = null then
         return "";
      else
         return Fragment.Background_Colour.all;
      end if;
   end Background_Colour;

   -------------------
   -- Border_Colour --
   -------------------

   function Border_Colour
     (Fragment : Root_Fragment_Type)
      return String
   is
   begin
      if Fragment.Border_Colour = null then
         return "";
      else
         return Fragment.Border_Colour.all;
      end if;
   end Border_Colour;

   -----------
   -- Clear --
   -----------

   procedure Clear (Fragment : in out Root_Fragment_Type) is
   begin
      Fragment.Lines.Clear;
      Fragment.Lines.Append (new Line_Info);
   end Clear;

   --------------
   -- Editable --
   --------------

   function Editable
     (Fragment : Root_Fragment_Type)
      return Boolean
   is
   begin
      return Fragment.Editable;
   end Editable;

   ---------------
   -- File_Name --
   ---------------

   function File_Name
     (Fragment : Root_Fragment_Type'Class)
      return String
   is
      Path : constant String :=
               Ada.Strings.Unbounded.To_String (Fragment.Path);
   begin
      return Ada.Directories.Simple_Name (Path);
   end File_Name;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Fragment : in out Root_Fragment_Type) is
      pragma Unreferenced (Fragment);
   begin
      null;
   end Finalize;

   -----------------------
   -- Foreground_Colour --
   -----------------------

   function Foreground_Colour
     (Fragment : Root_Fragment_Type)
      return String
   is
   begin
      if Fragment.Foreground_Colour = null then
         return "";
      else
         return Fragment.Foreground_Colour.all;
      end if;
   end Foreground_Colour;

   --------------
   -- Get_Link --
   --------------

   function Get_Link
     (Fragment : Root_Fragment_Type;
      Offset   : Positive)
      return Komnenos.Entities.Entity_Reference
   is
   begin
      return Get_Style_Info (Fragment, Offset).Reference;
   end Get_Link;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Fragment : Root_Fragment_Type;
      State    : Komnenos.Styles.Element_State;
      Offset   : Positive)
      return Komnenos.Styles.Komnenos_Style
   is
      Style : Komnenos.Styles.Komnenos_Style;
      Start, Finish : Natural;
   begin
      Root_Fragment_Type'Class (Fragment).Get_Style
        (State, Offset, Style, Start, Finish);
      return Style;
   end Get_Style;

   ---------------
   -- Get_Style --
   ---------------

   procedure Get_Style
     (Fragment : Root_Fragment_Type;
      State    : Komnenos.Styles.Element_State;
      Offset   : Positive;
      Style    : out Komnenos.Styles.Komnenos_Style;
      Start    : out Natural;
      Finish   : out Natural)
   is
      use Ada.Strings.Unbounded;
      use Komnenos.Styles;
      Line       : Positive := 1;
      Line_Start : Positive := 1;
      Last_Line  : constant Natural :=
                     Natural (Fragment.Lines.Length);
   begin
      while Line <= Last_Line loop
         declare
            This_Length : constant Natural :=
                            Length (Fragment.Lines (Line).Text) + 1;
         begin
            exit when This_Length + Line_Start > Offset;
            Line_Start := Line_Start + This_Length;
         end;
         Line := Line + 1;
      end loop;

      if Line <= Last_Line then
         declare
            Line_Offset  : Natural := 0;
            Style_Offset : constant Natural := Offset - Line_Start;
         begin
            for Info of Fragment.Lines (Line).Styles loop
               if Line_Offset + Info.Length > Style_Offset then
                  Start := Line_Start + Line_Offset;
                  Finish := Line_Start + Line_Offset + Info.Length;
                  Style := Info.Styles (State);
                  if State /= Normal and then Style = Null_Style then
                     Style := Info.Styles (Normal);
                  end if;
                  if Style = Null_Style then
                     Style := Fragment.Default_Style;
                  end if;
                  return;
               end if;
               Line_Offset := Line_Offset + Info.Length;
            end loop;
         end;
      end if;

      Style := Komnenos.Styles.Null_Style;
      Start := 0;
      Finish := 0;

   end Get_Style;

   --------------------
   -- Get_Style_Info --
   --------------------

   function Get_Style_Info
     (Fragment : Root_Fragment_Type;
      Offset   : Positive)
      return Style_Info
   is
      use Ada.Strings.Unbounded;
      Line       : Positive := 1;
      Line_Start : Positive := 1;
      Last_Line  : constant Natural :=
                     Natural (Fragment.Lines.Length);
   begin
      while Line <= Last_Line loop
         declare
            This_Length : constant Natural :=
                            Length (Fragment.Lines (Line).Text) + 1;
         begin
            exit when This_Length + Line_Start > Offset;
            Line_Start := Line_Start + This_Length;
         end;
         Line := Line + 1;
      end loop;

      if Line <= Last_Line then
         declare
            Line_Offset  : Natural := 0;
            Style_Offset : constant Natural := Offset - Line_Start;
         begin
            for Info of Fragment.Lines (Line).Styles loop
               Line_Offset := Line_Offset + Info.Length;
               if Line_Offset > Style_Offset then
                  return Info;
               end if;
            end loop;
         end;
      end if;

      return (0, (others => Fragment.Default_Style), null);
   end Get_Style_Info;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Fragment : in out Root_Fragment_Type) is
   begin
      Fragment.Layout_Rec := (0, 0, 600, 400);
      Fragment.Lines.Append (new Line_Info);
      Fragment.Default_Style := Komnenos.Styles.Default_Style;
   end Initialize;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Fragment : Root_Fragment_Type;
      Put      : not null access
        procedure (Text : String;
                   Style : Komnenos.Styles.Komnenos_Style;
                   Link  : Komnenos.Entities.Entity_Reference);
      New_Line : not null access procedure)
   is
      use Ada.Strings.Unbounded;
   begin
      for Line of Fragment.Lines loop
         declare
            Current : Positive := 1;
         begin
            for Style of Line.Styles loop
               declare
                  use Komnenos.Styles;
                  use Komnenos.Entities;
                  Normal_Style : constant Komnenos.Styles.Komnenos_Style :=
                                   Style.Styles (Komnenos.Styles.Normal);
                  This_Style : constant Komnenos.Styles.Komnenos_Style :=
                                 (if Normal_Style = Null_Style
                                  then Fragment.Default_Style
                                  else Normal_Style);
                  This_Link  : constant Komnenos.Entities.Entity_Reference :=
                                 Style.Reference;
                  This_Text  : constant String :=
                                 Slice (Line.Text, Current,
                                        Current + Style.Length - 1);
               begin
                  Put (This_Text, This_Style, This_Link);
                  Current := Current + Style.Length;
               end;
            end loop;

            if Current <= Length (Line.Text) then
               Put
                 (Slice (Line.Text, Current, Length (Line.Text)),
                  Fragment.Default_Style, null);
            end if;
         end;
         New_Line.all;
      end loop;
   end Iterate;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Fragment : in out Root_Fragment_Type) is
   begin
      Fragment.Lines.Append (new Line_Info);
   end New_Line;

   ----------
   -- Path --
   ----------

   function Path
     (Fragment : Root_Fragment_Type'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Fragment.Path);
   end Path;

   ---------
   -- Put --
   ---------

   procedure Put
     (Fragment : in out Root_Fragment_Type;
      Text     : in     String;
      Styles   : in     Komnenos.Styles.Style_Collection :=
        (others => Komnenos.Styles.Null_Style);
      Link     : in     Komnenos.Entities.Entity_Reference := null)
   is
      use type Komnenos.Styles.Komnenos_Style;
      Line : constant Line_Info_Access := Fragment.Lines.Last_Element;
   begin
      Line.Styles.Append
        ((Text'Length, Styles, Link));
      Ada.Strings.Unbounded.Append (Line.Text, Text);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Fragment : in out Root_Fragment_Type;
      Text     : in     String;
      Styles   : in     Komnenos.Styles.Style_Collection :=
        (others => Komnenos.Styles.Null_Style);
      Link     : in     Komnenos.Entities.Entity_Reference := null)
   is
   begin
      Root_Fragment_Type'Class (Fragment).Put (Text, Styles, Link);
      Root_Fragment_Type'Class (Fragment).New_Line;
   end Put_Line;

   ---------------
   -- Rectangle --
   ---------------

   function Rectangle
     (Fragment : Root_Fragment_Type'Class)
      return Layout_Rectangle
   is
   begin
      return Fragment.Layout_Rec;
   end Rectangle;

   -----------------------
   -- Set_Default_Style --
   -----------------------

   procedure Set_Default_Style
     (Fragment : in out Root_Fragment_Type'Class;
      Style    : in Komnenos.Styles.Komnenos_Style)
   is
   begin
      Fragment.Default_Style := Style;
   end Set_Default_Style;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Fragment : in out Root_Fragment_Type'Class;
      X, Y     : Integer)
   is
   begin
      Fragment.Layout_Rec.X := X;
      Fragment.Layout_Rec.Y := Y;
   end Set_Position;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Fragment : in out Root_Fragment_Type'Class;
      Width    : Natural;
      Height   : Natural)
   is
   begin
      Fragment.Layout_Rec.Width := Width;
      Fragment.Layout_Rec.Height := Height;
   end Set_Size;

   -------------------
   -- Text_Contents --
   -------------------

   function Text_Contents
     (Fragment : Root_Fragment_Type)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Line of Fragment.Lines loop
         Result := Result & Line.Text;
         Result := Result & Ada.Characters.Latin_1.LF;
      end loop;
      return To_String (Result);
   end Text_Contents;

   -----------
   -- Title --
   -----------

   function Title
     (Fragment : Root_Fragment_Type'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Fragment.Title);
   end Title;

end Komnenos.Fragments;
