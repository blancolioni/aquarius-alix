private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;
private with Ada.Strings.Fixed.Less_Case_Insensitive;
private with Ada.Strings.Unbounded;

private with Aqua.Memory;

package Aqua.Images is

   type Root_Image_Type is new Memory_Interface with private;

   overriding function Get_Byte
     (Image : Root_Image_Type;
      Addr  : Address)
      return Byte;

   overriding procedure Set_Byte
     (Image : in out Root_Image_Type;
      Addr  : Address;
      Value : Byte);

   procedure Load
     (Image : in out Root_Image_Type'Class;
      Path  : in     String);

   procedure Link
     (Image : in out Root_Image_Type'Class);

   procedure Bind
     (Image : Root_Image_Type'Class;
      Binder : not null access
        procedure (Group_Name  : String;
                   Before      : Boolean;
                   Parent_Name : String;
                   Child_Name  : String;
                   Start       : Address));

   procedure Save
     (Image : Root_Image_Type'Class;
      Path  : String);

   function Heap_Low
     (Image : Root_Image_Type'Class)
      return Address;

   function Heap_High
     (Image : Root_Image_Type'Class)
      return Address;

   function Have_String
     (Image : Root_Image_Type'Class;
      Value : Word)
      return Boolean
     with Pre => Is_String_Reference (Value);

   function String_Count
     (Image : Root_Image_Type'Class)
      return Natural;

   function To_String
     (Image : Root_Image_Type'Class;
      Value : Word)
      return String
     with Pre => Is_String_Reference (Value)
     and then Image.Have_String (Value);

   function Show (Image : Root_Image_Type'Class;
                  Value : Word)
                  return String;

   type Image_Type is access all Root_Image_Type'Class;

   function New_Image return Image_Type;

private

   type Reference_Info is
      record
         Addr : Address;
         Relative : Boolean;
      end record;

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists (Reference_Info);

   type Link_Info is
      record
         Value      : Word;
         References : List_Of_References.List;
         Has_Value  : Boolean := False;
         Is_String  : Boolean := False;
      end record;

   package Link_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Natural, String);

   package Link_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type        => String,
        Element_Type    => Link_Info,
        "<"             => Ada.Strings.Fixed.Less_Case_Insensitive);

   type Binding_Info is
      record
         Group       : Ada.Strings.Unbounded.Unbounded_String;
         Start       : Address;
         Before      : Boolean;
         Parent_Text : Ada.Strings.Unbounded.Unbounded_String;
         Child_Text  : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Binding_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Binding_Info);

   type Root_Image_Type is new Memory_Interface with
      record
         Bindings      : Binding_Info_Vectors.Vector;
         String_Vector : Link_Vectors.Vector;
         Label_Vector  : Link_Vectors.Vector;
         Link_Map      : Link_Maps.Map;
         Memory        : Aqua.Memory.Memory_Type;
         Low           : Address := 16#1000#;
         High          : Address := 16#1000#;
      end record;

end Aqua.Images;