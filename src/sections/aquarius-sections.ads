private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

with Aquarius.Colours;
with Aquarius.Keys;
with Aquarius.Layout;
with Aquarius.Styles;

package Aquarius.Sections is

   type Section_Display_Interface is interface;

   procedure Update (Display : in out Section_Display_Interface) is null;
   procedure Set_Point (Display : in out Section_Display_Interface;
                        Point   : Aquarius.Layout.Position)
   is null;

   type Root_Aquarius_Section is tagged private;

   procedure On_Key
     (Section : in out Root_Aquarius_Section;
      Key     : Aquarius.Keys.Aquarius_Key;
      Handled : out Boolean);

   function Id (Section : Root_Aquarius_Section'Class)
                return String;

   function Point
     (Section : Root_Aquarius_Section'Class)
      return Aquarius.Layout.Position;

   procedure Set_Point
     (Section : in out Root_Aquarius_Section;
      Point   : Aquarius.Layout.Position);

   function Display
     (Section : Root_Aquarius_Section'Class)
      return access Section_Display_Interface'Class;

   procedure Set_Display
     (Section : in out Root_Aquarius_Section'Class;
      Display : access Section_Display_Interface'Class);

   procedure Clear
     (Section : in out Root_Aquarius_Section'Class);

   procedure Set_Background (Section : in out Root_Aquarius_Section'Class;
                             Background : Aquarius.Colours.Aquarius_Colour);

   function Background (Section : Root_Aquarius_Section'Class)
                        return Aquarius.Colours.Aquarius_Colour;

   function Style (Section : Root_Aquarius_Section'Class)
                   return Aquarius.Styles.Aquarius_Style;

   function Max_Column_Count
     (Section : Root_Aquarius_Section'Class)
      return Natural;

   function Line_Count (Section : Root_Aquarius_Section'Class) return Positive;

   procedure Put (Section    : in out Root_Aquarius_Section'Class;
                  Position   :        Aquarius.Layout.Position;
                  Text       : in     String;
                  Style_Name : in     String := "");

   procedure Put (Section    : in out Root_Aquarius_Section'Class;
                  Text       : in     String;
                  Style_Name : in     String := "");

   procedure Put_Line (Section    : in out Root_Aquarius_Section'Class;
                       Text       : in     String;
                       Style_Name : in     String := "");

   procedure New_Line (Section : in out Root_Aquarius_Section'Class);

   function Text (Section : Root_Aquarius_Section'Class) return String;

   procedure Set_Rendered_Size
     (Section : in out Root_Aquarius_Section'Class;
      Width   : Natural;
      Height  : Natural);

   function Render_Width (Section : Root_Aquarius_Section'Class)
     return Natural;
   function Render_Height (Section : Root_Aquarius_Section'Class)
     return Natural;

   type Aquarius_Section is access all Root_Aquarius_Section'Class;

   type Section_Element is private;

   function Text (Element : Section_Element) return String;
   function Class (Element : Section_Element) return String;
   function New_Line (Element : Section_Element) return Boolean;

   procedure Render (Section : Root_Aquarius_Section'Class;
                     Renderer : not null access
                       procedure (Element : Section_Element));

private

   type Section_Element_Record;

   type Section_Element is access Section_Element_Record;

   package Section_Element_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Section_Element);

   type Root_Aquarius_Section is tagged
      record
         Id            : Ada.Strings.Unbounded.Unbounded_String;
         Background    : Aquarius.Colours.Aquarius_Colour;
         Style         : Aquarius.Styles.Aquarius_Style :=
                           Aquarius.Styles.Default_Style;
         Elements      : Section_Element_Lists.List;
         Line_Count    : Natural := 1;
         Max_Columns   : Natural := 0;
         Render_Width  : Natural := 200;
         Render_Height : Natural := 200;
         Point         : Aquarius.Layout.Position := (1, 1);
         Display       : access Section_Display_Interface'Class;
      end record;

   procedure Create (Item : in out Root_Aquarius_Section'Class;
                     Id   : in     String);

   package Section_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => Aquarius_Section,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => Ada.Strings.Unbounded."=");

   package Section_Lists is
     new Ada.Containers.doubly_Linked_Lists
       (Element_Type => Aquarius_Section);

end Aquarius.Sections;
