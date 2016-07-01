private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Finalization;
private with Ada.Strings.Unbounded;

with Tropos;

with Aquarius.Colours;
with Aquarius.Keys.Sequences;
with Aquarius.Layout;
with Aquarius.Styles;
with Aquarius.Themes;

with Komnenos.Entities;
with Komnenos.Session_Objects;

with Komnenos.Commands.Manager;
private with Komnenos.Commands.Bindings;

package Komnenos.Fragments is

   type Text_Editor_Display is interface;

   procedure Insert_At_Cursor
     (Editor : in out Text_Editor_Display;
      Text   : in     String)
   is abstract;

   procedure Delete_From_Cursor
     (Editor : in out Text_Editor_Display;
      Offset : in     Aquarius.Layout.Position_Offset)
   is abstract;

   procedure Set_Cursor
     (Editor       : in out Text_Editor_Display;
      New_Position : Aquarius.Layout.Position)
   is abstract;

   procedure Set_Content
     (Editor       : in out Text_Editor_Display;
      New_Content  : String)
   is abstract;

   type Root_Fragment_Type is
     new Komnenos.Entities.Entity_Visual
     and Komnenos.Session_Objects.Session_Object_Interface
   with private;

   procedure Render_Fragment
     (Editor : in out Text_Editor_Display;
      Fragment : not null access Root_Fragment_Type'Class)
   is abstract;

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

   overriding function X (Fragment : Root_Fragment_Type) return Integer
   is (Fragment.Rectangle.X);

   overriding function Y (Fragment : Root_Fragment_Type) return Integer
   is (Fragment.Rectangle.Y);

   overriding function Width
     (Fragment : Root_Fragment_Type) return Positive
   is (Fragment.Rectangle.Width);

   overriding function Height
     (Fragment : Root_Fragment_Type) return Positive
   is (Fragment.Rectangle.Height);

   procedure Set_Text_Display
     (Fragment : in out Root_Fragment_Type;
      Display  : access Text_Editor_Display'Class);

   procedure Execute
     (Fragment : in out Root_Fragment_Type'Class;
      Command  : in out Komnenos.Commands.Root_Komnenos_Command'Class);

   procedure Rendered
     (Fragment : in out Root_Fragment_Type);

   procedure Set_Position
     (Fragment : in out Root_Fragment_Type'Class;
      X, Y     : Integer);

   procedure Set_Size
     (Fragment : in out Root_Fragment_Type'Class;
      Width    : Natural;
      Height   : Natural);

   procedure Set_Entity_Key
     (Fragment : in out Root_Fragment_Type'Class;
      Key      : String);

   function Entity_Key (Fragment : Root_Fragment_Type'Class)
                        return String;

   overriding procedure Put
     (Fragment : in out Root_Fragment_Type;
      Text     : in     String;
      Style    : in     Aquarius.Styles.Aquarius_Style;
      Link     : access Komnenos.Entities.Root_Entity_Reference'Class := null);

   overriding procedure New_Line (Fragment : in out Root_Fragment_Type);

   overriding procedure Clear (Fragment : in out Root_Fragment_Type);

   procedure On_Key_Press
     (Fragment : in out Root_Fragment_Type;
      Key      : Aquarius.Keys.Aquarius_Key);

   procedure On_Cursor_Move
     (Fragment     : in out Root_Fragment_Type;
      New_Position : Aquarius.Layout.Position);

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
         Content           : Komnenos.Entities.Entity_Reference;
         Display           : access Text_Editor_Display'Class;
         Point             : Aquarius.Layout.Position := 0;
         Commands          : Komnenos.Commands.Manager.Command_Manager;
         Default_Style     : Aquarius.Styles.Aquarius_Style;
         Layout_Rec        : Layout_Rectangle;
         Path              : Ada.Strings.Unbounded.Unbounded_String;
         Title             : Ada.Strings.Unbounded.Unbounded_String;
         Key               : Ada.Strings.Unbounded.Unbounded_String;
         Editable          : Boolean;
         Background_Colour : Aquarius.Colours.Aquarius_Colour;
         Foreground_Colour : Aquarius.Colours.Aquarius_Colour;
         Border_Colour     : Aquarius.Colours.Aquarius_Colour;
         Key_Sequence      : Aquarius.Keys.Sequences.Key_Sequence;
         Bindings          : Komnenos.Commands.Bindings.Binding_Table;
         Lines             : Line_Vectors.Vector;
         Needs_Render      : Boolean := False;
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

   overriding procedure Set_Content
     (Fragment : in out Root_Fragment_Type;
      Content  : access Komnenos.Entities.Root_Entity_Reference'Class);

   overriding function Get_Content
     (Fragment : Root_Fragment_Type)
      return access Komnenos.Entities.Root_Entity_Reference'Class
   is (Fragment.Content);

   overriding procedure Set_Cursor
     (Fragment : in out Root_Fragment_Type;
      Cursor   : Komnenos.Entities.Cursor_Type;
      Position : Aquarius.Layout.Position);

   overriding procedure Insert_At_Cursor
     (Fragment : in out Root_Fragment_Type;
      Text     : String);

   overriding procedure Delete_From_Cursor
     (Fragment  : in out Root_Fragment_Type;
      Offset    : Aquarius.Layout.Position_Offset);

   overriding procedure Invalidate
     (Fragment : not null access Root_Fragment_Type);

   function Needs_Render
     (Fragment : Root_Fragment_Type)
      return Boolean;

end Komnenos.Fragments;
