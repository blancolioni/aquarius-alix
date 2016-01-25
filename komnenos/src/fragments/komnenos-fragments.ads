private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Finalization;
private with Ada.Strings.Unbounded;

with Tropos;

with Aquarius.Colours;
with Aquarius.Styles;
with Aquarius.Themes;

with Komnenos.Entities;
with Komnenos.Session_Objects;

package Komnenos.Fragments is

   type Layout_Rectangle is
      record
         X, Y          : Integer;
         Width, Height : Positive;
      end record;

   function To_Config (Rectangle : Layout_Rectangle)
                       return Tropos.Configuration;

   function From_Config (Config : Tropos.Configuration)
                         return Layout_Rectangle;

   type Root_Fragment_Type is
     new Komnenos.Entities.Entity_Visual
     and Komnenos.Session_Objects.Session_Object_Interface
   with private;

   overriding function Config_Name
     (Fragment : Root_Fragment_Type)
      return String
   is ("fragment");

   function File_Name
     (Fragment : Root_Fragment_Type'Class)
      return String;

   function Path
     (Fragment : Root_Fragment_Type'Class)
      return String;

   function Title
     (Fragment : Root_Fragment_Type'Class)
      return String;

   function Key
     (Fragment : Root_Fragment_Type'Class)
      return String;

   function Rectangle
     (Fragment : Root_Fragment_Type'Class)
      return Layout_Rectangle;

   function X (Fragment : Root_Fragment_Type'Class) return Integer
   is (Fragment.Rectangle.X);

   function Y (Fragment : Root_Fragment_Type'Class) return Integer
   is (Fragment.Rectangle.Y);

   function Width (Fragment : Root_Fragment_Type'Class) return Positive
   is (Fragment.Rectangle.Width);

   function Height (Fragment : Root_Fragment_Type'Class) return Positive
   is (Fragment.Rectangle.Height);

   procedure Set_Position
     (Fragment : in out Root_Fragment_Type'Class;
      X, Y     : Integer);

   procedure Set_Size
     (Fragment : in out Root_Fragment_Type'Class;
      Width    : Natural;
      Height   : Natural);

   procedure Set_Default_Style
     (Fragment : in out Root_Fragment_Type'Class;
      Style    : in Aquarius.Styles.Aquarius_Style);

   procedure Set_Entity_Key
     (Fragment : in out Root_Fragment_Type'Class;
      Key      : String);

   function Entity_Key (Fragment : Root_Fragment_Type'Class)
                        return String;

   procedure Put
     (Fragment : in out Root_Fragment_Type;
      Text     : in     String;
      Style    : in     Aquarius.Styles.Aquarius_Style;
      Link     : in     Komnenos.Entities.Entity_Reference := null);

   procedure Put_Line
     (Fragment : in out Root_Fragment_Type;
      Text     : in     String;
      Style    : in     Aquarius.Styles.Aquarius_Style;
      Link     : in     Komnenos.Entities.Entity_Reference := null);

   procedure New_Line (Fragment : in out Root_Fragment_Type);

   procedure Clear (Fragment : in out Root_Fragment_Type);

   function Editable
     (Fragment : Root_Fragment_Type)
      return Boolean;

   function Background_Colour
     (Fragment : Root_Fragment_Type)
      return Aquarius.Colours.Aquarius_Colour;

   function Border_Colour
     (Fragment : Root_Fragment_Type)
      return Aquarius.Colours.Aquarius_Colour;

   function Foreground_Colour
     (Fragment : Root_Fragment_Type)
      return Aquarius.Colours.Aquarius_Colour;

   function Text_Contents
     (Fragment : Root_Fragment_Type)
      return String;

   function Get_Link
     (Fragment : Root_Fragment_Type;
      Offset   : Positive)
      return Komnenos.Entities.Entity_Reference;
   --  If the text at the given offset has an associated reference, return it
   --  otherwise, return null.

   procedure Get_Style
     (Fragment : Root_Fragment_Type;
      State    : Aquarius.Themes.Element_State;
      Offset   : Positive;
      Style    : out Aquarius.Styles.Aquarius_Style;
      Start    : out Natural;
      Finish   : out Natural);

   function Get_Style
     (Fragment : Root_Fragment_Type;
      State    : Aquarius.Themes.Element_State;
      Offset   : Positive)
      return Aquarius.Styles.Aquarius_Style;

   procedure Iterate
     (Fragment : Root_Fragment_Type;
      Put      : not null access
        procedure (Text  : String;
                   Style : Aquarius.Styles.Aquarius_Style;
                   Link  : Komnenos.Entities.Entity_Reference);
      New_Line : not null access procedure);

   type Fragment_Type is access all Root_Fragment_Type'Class;

   procedure Register;

private

   type Style_Collection is
     array (Aquarius.Themes.Element_State) of Aquarius.Styles.Aquarius_Style;

   type Style_Info is
      record
         Length    : Natural;
         Styles    : Style_Collection;
         Reference : Komnenos.Entities.Entity_Reference;
      end record;

   package Style_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Style_Info);

   type Line_Info is
      record
         Text   : Ada.Strings.Unbounded.Unbounded_String;
         Styles : Style_Lists.List;
      end record;

   type Line_Info_Access is access Line_Info;

   package Line_Vectors is
     new Ada.Containers.Vectors
       (Positive, Line_Info_Access);

   type Root_Fragment_Type is
     new Ada.Finalization.Controlled
     and Komnenos.Entities.Entity_Visual
     and Komnenos.Session_Objects.Session_Object_Interface with
      record
         Default_Style     : Aquarius.Styles.Aquarius_Style;
         Layout_Rec        : Layout_Rectangle;
         Path              : Ada.Strings.Unbounded.Unbounded_String;
         Title             : Ada.Strings.Unbounded.Unbounded_String;
         Key               : Ada.Strings.Unbounded.Unbounded_String;
         Editable          : Boolean;
         Background_Colour : Aquarius.Colours.Aquarius_Colour;
         Foreground_Colour : Aquarius.Colours.Aquarius_Colour;
         Border_Colour     : Aquarius.Colours.Aquarius_Colour;
         Lines             : Line_Vectors.Vector;
      end record;

   overriding procedure Initialize (Fragment : in out Root_Fragment_Type);
   overriding procedure Finalize (Fragment : in out Root_Fragment_Type);
   overriding procedure Adjust (Fragment : in out Root_Fragment_Type);

   overriding procedure To_Config
     (Fragment : Root_Fragment_Type;
      Config   : in out Tropos.Configuration);

   overriding procedure From_Config
     (Fragment : not null access Root_Fragment_Type;
      Config   : Tropos.Configuration);

end Komnenos.Fragments;
