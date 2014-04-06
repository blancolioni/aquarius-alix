with Ada.Text_IO;

package body Aquarius.Sections is

   use Ada.Strings.Unbounded;
   use Aquarius.Layout;

   type Section_Element_Record is
      record
         Text      : Ada.Strings.Unbounded.Unbounded_String;
         Class     : Ada.Strings.Unbounded.Unbounded_String;
         Link      : Ada.Strings.Unbounded.Unbounded_String;
         New_Line  : Boolean;
      end record;

   type Default_Display_Type is
     new Section_Display_Interface with null record;

   overriding procedure Update
     (Display : in out Default_Display_Type)
   is null;

   Default_Display : aliased Default_Display_Type;

   ----------------
   -- Background --
   ----------------

   function Background (Section : Root_Aquarius_Section'Class)
                        return Aquarius.Colours.Aquarius_Colour
   is
   begin
      return Section.Background;
   end Background;

   -----------
   -- Class --
   -----------

   function Class (Element : Section_Element) return String is
   begin
      return To_String (Element.Class);
   end Class;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Section : in out Root_Aquarius_Section'Class)
   is
   begin
      Section.Elements.Clear;
      Section.Line_Count := 1;
      Section.Max_Columns := 0;
      Section.Point := (1, 1);
   end Clear;

   ------------
   -- Create --
   ------------

   procedure Create (Item : in out Root_Aquarius_Section'Class;
                     Id   : in     String)
   is
   begin
      Item.Id := To_Unbounded_String (Id);
      Ada.Text_IO.Put_Line ("new section " & Id);
   end Create;

   -------------
   -- Display --
   -------------

   function Display
     (Section : Root_Aquarius_Section'Class)
      return access Section_Display_Interface'Class
   is
   begin
      if Section.Display = null then
         return Default_Display'Access;
      else
         return Section.Display;
      end if;
   end Display;

   --------
   -- Id --
   --------

   function Id (Section : Root_Aquarius_Section'Class)
                return String
   is
   begin
      return To_String (Section.Id);
   end Id;

   ----------------
   -- Line_Count --
   ----------------

   function Line_Count
     (Section : Root_Aquarius_Section'Class)
      return Positive
   is
   begin
      return Section.Line_Count;
   end Line_Count;

   ----------------------
   -- Max_Column_Count --
   ----------------------

   function Max_Column_Count
     (Section : Root_Aquarius_Section'Class)
      return Natural
   is
      Result : Natural := 0;
      This_Line : Natural := 0;
   begin
      for Element of Section.Elements loop
         This_Line := This_Line + Length (Element.Text);
         if This_Line > Result then
            Result := This_Line;
         end if;
         if Element.New_Line then
            This_Line := 0;
         end if;
      end loop;
      return Result;
   end Max_Column_Count;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Section : in out Root_Aquarius_Section'Class) is
      Element : constant Section_Element :=
                  new Section_Element_Record'
                    (Text     => Null_Unbounded_String,
                     Class    => Null_Unbounded_String,
                     Link     => Null_Unbounded_String,
                     New_Line => True);
   begin
      Section.Line_Count := Section.Line_Count + 1;
      Section.Elements.Append (Element);
      Section.Point.Line := Section.Point.Line + 1;
      Section.Point.Column := 1;
   end New_Line;

   --------------
   -- New_Line --
   --------------

   function New_Line (Element : Section_Element) return Boolean is
   begin
      return Element.New_Line;
   end New_Line;

   ------------
   -- On_Key --
   ------------

   procedure On_Key
     (Section : in out Root_Aquarius_Section;
      Key     : Aquarius.Keys.Aquarius_Key;
      Handled : out Boolean)
   is
      pragma Unreferenced (Section);
      pragma Unreferenced (Key);
   begin
      Handled := False;
   end On_Key;

   -----------
   -- Point --
   -----------

   function Point
     (Section : Root_Aquarius_Section'Class)
      return Aquarius.Layout.Position
   is
   begin
      return Section.Point;
   end Point;

   ---------
   -- Put --
   ---------

   procedure Put (Section    : in out Root_Aquarius_Section'Class;
                  Position   :        Aquarius.Layout.Position;
                  Text       : in     String;
                  Style_Name : in     String := "")
   is
   begin
      if Natural (Position.Line) > Section.Line_Count then
         for I in Section.Line_Count .. Natural (Position.Line) - 1 loop
            Section.New_Line;
         end loop;
         Section.Point.Line := Position.Line;
         Section.Point.Column := 1;
      end if;
      if Section.Point.Column < Position.Column then
         declare
            Space_Count : constant Positive :=
                            Positive
                              (Position.Column - Section.Point.Column);
            Spaces      : constant String (1 .. Space_Count) :=
                            (others => ' ');
         begin
            Section.Put (Spaces);
         end;
      end if;
      Section.Put (Text, Style_Name);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (Section    : in out Root_Aquarius_Section'Class;
      Text       : in     String;
      Style_Name : in     String := "")
   is
      Element : constant Section_Element :=
                  new Section_Element_Record'
                    (Text     => To_Unbounded_String (Text),
                     Class    => To_Unbounded_String (Style_Name),
                     Link     => Null_Unbounded_String,
                     New_Line => False);
   begin
      Section.Elements.Append (Element);
      Section.Point.Column :=
        Section.Point.Column + Aquarius.Layout.Count (Text'Length);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Section    : in out Root_Aquarius_Section'Class;
      Text       : in     String;
      Style_Name : in     String := "")
   is
      Element : constant Section_Element :=
                  new Section_Element_Record'
                    (Text     => To_Unbounded_String (Text),
                     Class    => To_Unbounded_String (Style_Name),
                     Link     => Null_Unbounded_String,
                     New_Line => True);
   begin
      Section.Elements.Append (Element);
      Section.Line_Count := Section.Line_Count + 1;
      Section.Point.Line := Section.Point.Line + 1;
      Section.Point.Column := 1;
   end Put_Line;

   ------------
   -- Render --
   ------------

   procedure Render
     (Section : Root_Aquarius_Section'Class;
      Renderer : not null access
                       procedure (Element : Section_Element))
   is
   begin
      for Element of Section.Elements loop
         Renderer (Element);
      end loop;
   end Render;

   -------------------
   -- Render_Height --
   -------------------

   function Render_Height (Section : Root_Aquarius_Section'Class)
                           return Natural
   is
   begin
      return Section.Render_Height;
   end Render_Height;

   ------------------
   -- Render_Width --
   ------------------

   function Render_Width (Section : Root_Aquarius_Section'Class)
                          return Natural
   is
   begin
      return Section.Render_Width;
   end Render_Width;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Section : in out Root_Aquarius_Section'Class;
      Background : Aquarius.Colours.Aquarius_Colour)
   is
   begin
      Section.Background := Background;
   end Set_Background;

   -----------------
   -- Set_Display --
   -----------------

   procedure Set_Display
     (Section : in out Root_Aquarius_Section'Class;
      Display : access Section_Display_Interface'Class)
   is
   begin
      if Display = null then
         Section.Display := Default_Display'Access;
      else
         Section.Display := Display;
      end if;
   end Set_Display;

   ---------------
   -- Set_Point --
   ---------------

   procedure Set_Point
     (Section : in out Root_Aquarius_Section;
      Point   : Aquarius.Layout.Position)
   is
   begin
      Section.Point := Point;
   end Set_Point;

   -----------------------
   -- Set_Rendered_Size --
   -----------------------

   procedure Set_Rendered_Size
     (Section : in out Root_Aquarius_Section'Class;
      Width   : Natural;
      Height  : Natural)
   is
   begin
      Section.Render_Width := Width;
      Section.Render_Height := Height;
   end Set_Rendered_Size;

   -----------
   -- Style --
   -----------

   function Style (Section : Root_Aquarius_Section'Class)
                   return Aquarius.Styles.Aquarius_Style
   is
   begin
      return Section.Style;
   end Style;

   ----------
   -- Text --
   ----------

   function Text (Section : Root_Aquarius_Section'Class) return String is
      Result : Unbounded_String;
   begin
      for Element of Section.Elements loop
         Result := Result & Element.Text;
         if Element.New_Line then
            Result := Result & Character'Val (10);
         end if;
      end loop;
      return To_String (Result);
   end Text;

   ----------
   -- Text --
   ----------

   function Text (Element : Section_Element) return String is
   begin
      return To_String (Element.Text);
   end Text;

end Aquarius.Sections;
