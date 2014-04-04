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
      Section.Position.Line := Section.Position.Line + 1;
      Section.Position.Column := 1;
   end New_Line;

   --------------
   -- New_Line --
   --------------

   function New_Line (Element : Section_Element) return Boolean is
   begin
      return Element.New_Line;
   end New_Line;

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
         Section.Position.Line := Position.Line;
         Section.Position.Column := 1;
      end if;
      if Section.Position.Column < Position.Column then
         declare
            Space_Count : constant Positive :=
                            Positive
                              (Position.Column - Section.Position.Column);
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
      Section.Position.Column :=
        Section.Position.Column + Aquarius.Layout.Count (Text'Length);
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
      Section.Position.Line := Section.Position.Line + 1;
      Section.Position.Column := 1;
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
