with Ada.Characters.Latin_1;
with Ada.Directories;

package body Komnenos.Fragments is

   function Get_Style_Info
     (Fragment : Root_Fragment_Type;
      Offset   : Positive)
      return Style_Info;

   function New_Fragment
     return access Komnenos.Session_Objects.Session_Object_Interface'Class;

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
      return Aquarius.Colours.Aquarius_Colour
   is
   begin
      return Fragment.Background_Colour;
   end Background_Colour;

   -------------------
   -- Border_Colour --
   -------------------

   function Border_Colour
     (Fragment : Root_Fragment_Type)
      return Aquarius.Colours.Aquarius_Colour
   is
   begin
      return Fragment.Border_Colour;
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

   ----------------
   -- Entity_Key --
   ----------------

   function Entity_Key (Fragment : Root_Fragment_Type'Class)
                        return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Fragment.Key);
   end Entity_Key;

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
      return Aquarius.Colours.Aquarius_Colour
   is
   begin
      return Fragment.Foreground_Colour;
   end Foreground_Colour;

   -----------------
   -- From_Config --
   -----------------

   overriding procedure From_Config
     (Fragment : not null access Root_Fragment_Type;
      Config   : Tropos.Configuration)
   is
   begin
      Fragment.Default_Style :=
        Aquarius.Themes.Active_Theme.Style (Config.Get ("default_style"));
      Fragment.Layout_Rec := From_Config (Config.Child ("rectangle"));
      Fragment.Path :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (String'(Config.Get ("path")));
      Fragment.Title :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (String'(Config.Get ("title")));
      Fragment.Key :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (String'(Config.Get ("key")));
      Fragment.Editable := Config.Get ("editable");
      Fragment.Background_Colour :=
        Aquarius.Colours.From_String (Config.Get ("background"));
      Fragment.Foreground_Colour :=
        Aquarius.Colours.From_String (Config.Get ("foreground"));
      Fragment.Border_Colour :=
        Aquarius.Colours.From_String (Config.Get ("border"));
   end From_Config;

   -----------------
   -- From_Config --
   -----------------

   function From_Config (Config : Tropos.Configuration)
                         return Layout_Rectangle
   is
   begin
      return (X => Config.Get ("x"), Y => Config.Get ("y"),
              Width => Config.Get ("width"),
              Height => Config.Get ("height"));
   end From_Config;

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
      State    : Aquarius.Themes.Element_State;
      Offset   : Positive)
      return Aquarius.Styles.Aquarius_Style
   is
      Style : Aquarius.Styles.Aquarius_Style;
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
      State    : Aquarius.Themes.Element_State;
      Offset   : Positive;
      Style    : out Aquarius.Styles.Aquarius_Style;
      Start    : out Natural;
      Finish   : out Natural)
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Styles;
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
            use Aquarius.Themes;
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

      Style := Aquarius.Styles.Null_Style;
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
      Fragment.Layout_Rec := (0, 0, 350, 400);
      Fragment.Lines.Append (new Line_Info);
      Fragment.Default_Style := Aquarius.Themes.Active_Theme.Default_Style;
   end Initialize;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Fragment : Root_Fragment_Type;
      Put      : not null access
        procedure (Text : String;
                   Style : Aquarius.Styles.Aquarius_Style;
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
                  use Aquarius.Styles;
                  use Komnenos.Entities;
                  Normal_Style : constant Aquarius.Styles.Aquarius_Style :=
                                   Style.Styles (Aquarius.Themes.Normal);
                  This_Style : constant Aquarius.Styles.Aquarius_Style :=
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

   ---------
   -- Key --
   ---------

   function Key
     (Fragment : Root_Fragment_Type'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Fragment.Key);
   end Key;

   ------------------
   -- New_Fragment --
   ------------------

   function New_Fragment
     return access Komnenos.Session_Objects.Session_Object_Interface'Class
   is
      Result :  constant Fragment_Type := new Root_Fragment_Type;
   begin
      return Result;
   end New_Fragment;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Fragment : in out Root_Fragment_Type) is
   begin
      Fragment.Lines.Append (new Line_Info);
   end New_Line;

   -------------------------
   -- On_Insert_Character --
   -------------------------

   procedure On_Insert_Character
     (Fragment : in out Root_Fragment_Type;
      Offset   : Natural;
      Value    : Character;
      Cancel   : out Boolean)
   is
   begin
      Fragment.Content.Set_Cursor (Offset);
      Fragment.Content.Insert_Text ((1 => Value));
      Cancel := False;
   end On_Insert_Character;

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
      Style    : in     Aquarius.Styles.Aquarius_Style;
      Link     : in     Komnenos.Entities.Entity_Reference := null)
   is
      use type Aquarius.Styles.Aquarius_Style;
      use type Komnenos.Entities.Entity_Reference;
      Line : constant Line_Info_Access := Fragment.Lines.Last_Element;
      Line_Style : Style_Info :=
                     (Length    => Text'Length,
                      Styles    => (Aquarius.Themes.Normal => Style,
                                    others                 => null),
                      Reference => Link);
   begin
      if Link /= null then
         Line_Style.Styles (Aquarius.Themes.Hover) :=
           Aquarius.Themes.Active_Theme.Default_Link_Style;
      end if;

      Line.Styles.Append (Line_Style);
      Ada.Strings.Unbounded.Append (Line.Text, Text);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Fragment : in out Root_Fragment_Type;
      Text     : in     String;
      Style    : in     Aquarius.Styles.Aquarius_Style;
      Link     : in     Komnenos.Entities.Entity_Reference := null)
   is
   begin
      Root_Fragment_Type'Class (Fragment).Put (Text, Style, Link);
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

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Komnenos.Session_Objects.Register_Session_Object
        ("fragment", New_Fragment'Access);
   end Register;

   -----------------
   -- Set_Content --
   -----------------

   overriding procedure Set_Content
     (Fragment : in out Root_Fragment_Type;
      Content  : access Komnenos.Entities.Root_Entity_Reference'Class)
   is
   begin
      Fragment.Content := Komnenos.Entities.Entity_Reference (Content);
   end Set_Content;

   --------------------
   -- Set_Entity_Key --
   --------------------

   procedure Set_Entity_Key
     (Fragment : in out Root_Fragment_Type'Class;
      Key      : String)
   is
   begin
      Fragment.Key := Ada.Strings.Unbounded.To_Unbounded_String (Key);
   end Set_Entity_Key;

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

   ---------------
   -- To_Config --
   ---------------

   overriding procedure To_Config
     (Fragment : Root_Fragment_Type;
      Config   : in out Tropos.Configuration)
   is
   begin
      Config.Add ("default_style", Fragment.Default_Style.Name);
      Config.Add ("link_style", Fragment.Default_Style.Name);
      Config.Add (To_Config (Fragment.Layout_Rec));
      Config.Add ("path", Ada.Strings.Unbounded.To_String (Fragment.Path));
      Config.Add ("title", Ada.Strings.Unbounded.To_String (Fragment.Title));
      Config.Add ("editable", (if Fragment.Editable then "yes" else "no"));
      Config.Add ("background",
                  Aquarius.Colours.To_String (Fragment.Background_Colour));
      Config.Add ("foreground",
                  Aquarius.Colours.To_String (Fragment.Foreground_Colour));
      Config.Add ("border",
                  Aquarius.Colours.To_String (Fragment.Border_Colour));
      Config.Add ("key", Fragment.Entity_Key);
   end To_Config;

   ---------------
   -- To_Config --
   ---------------

   function To_Config (Rectangle : Layout_Rectangle)
                       return Tropos.Configuration
   is
      Config : Tropos.Configuration := Tropos.New_Config ("rectangle");
   begin
      Config.Add ("x", Rectangle.X);
      Config.Add ("y", Rectangle.Y);
      Config.Add ("width", Rectangle.Width);
      Config.Add ("height", Rectangle.Height);
      return Config;
   end To_Config;

end Komnenos.Fragments;
