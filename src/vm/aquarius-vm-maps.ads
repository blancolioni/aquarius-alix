private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded.Hash;

package Aquarius.VM.Maps is

   type Map_Property_Type is
     new Object_Interface with private;

   overriding function Name
     (Item : Map_Property_Type)
      return String;

   overriding function Contains
     (Map : Map_Property_Type;
      Key : String)
      return Boolean;

   overriding function Get
     (Map : Map_Property_Type;
      Key : String)
      return VM_Value;

   overriding procedure Set
     (Map   : in out Map_Property_Type;
      Key   : in     String;
      Value : in     VM_Value);

   procedure Insert
     (Property : in out Map_Property_Type'Class;
      Key      : in     String;
      Value    : in     VM_Value);

   procedure Replace
     (Property : in out Map_Property_Type'Class;
      Key      : in     String;
      Value    : in     VM_Value);

   function Element
     (Property : Map_Property_Type'Class;
      Key      : String)
      return VM_Value;

   procedure Iterate
     (Property : Map_Property_Type'Class;
      Process  : not null access
        procedure (Key : String;
                   Value : VM_Value));

   procedure Iterate
     (Property : Map_Property_Type'Class;
      Process  : not null access
        procedure (Value : VM_Value));

private

   package VM_Value_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type    => VM_Value);

   package VM_Value_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => VM_Value_Lists.Cursor,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => Ada.Strings.Unbounded."=",
        "="             => VM_Value_Lists."=");

   type Map_Property_Type is
     new Object_Interface with
      record
         Map  : VM_Value_Maps.Map;
         List : VM_Value_Lists.List;
      end record;

end Aquarius.VM.Maps;
