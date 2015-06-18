private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;

private with Ada.Strings.Fixed.Equal_Case_Insensitive;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Unbounded;

with Aquarius.Programs;

package Komnenos.Entities is

   type Entity_Visual is interface;

   type Entity_Visual_Access is access all Entity_Visual'Class;

   type Root_Entity_Reference is abstract tagged private;

   function Identifier
     (Item : Root_Entity_Reference'Class)
      return String;

   function Class
     (Item : Root_Entity_Reference'Class)
      return String;

   function Display_Text
     (Item : Root_Entity_Reference)
      return String;

   function Description
     (Item : Root_Entity_Reference)
      return String;

   function Top_Level
     (Item : Root_Entity_Reference)
      return Boolean
   is (True);

   procedure Create
     (Item         : in out Root_Entity_Reference'Class;
      Identifier   : String;
      Class_Name   : String;
      Display_Text : String := "";
      Description  : String := "");

   type Entity_Reference is access all Root_Entity_Reference'Class;

   type Entity_Table_Interface is interface;

   procedure Select_Entity
     (Entity : Root_Entity_Reference;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Offset : Natural)
   is abstract;

   type Program_Store_Interface is interface;

   function Get_Program
     (Store  : in out Program_Store_Interface;
      Name   : String)
      return Aquarius.Programs.Program_Tree
      is abstract;

   function Program_Store
     (Table : Entity_Table_Interface)
      return access Program_Store_Interface'Class
      is abstract;

   procedure Set_Program_Store
     (Table : in out Entity_Table_Interface;
      Store : access Program_Store_Interface'Class)
   is null;

   type Array_Of_Entities is array (Positive range <>) of Entity_Reference;

--     type Cross_Reference_Type is
--       (Body_Entity, Type_Completion, Type_Discriminant,
--        Object_Definition, End_Of_Spec, Abstract_Type,
--        Implicit_Reference, Implicit_Reference_Child_Parent,
--        End_Label, Modification,
--        Primitive_Operation, Overriding_Primitive_Operation,
--        Reference, Dispatching_Subprogram_Reference,
--        Static_Subprogram_Reference, End_Of_Body,
--        With_Package, Type_Extension, Generic_Formal_Parameter,
--        Subprogram_In_Parameter, Subprogram_In_Out_Parameter,
--        Subprogram_Out_Parameter, Subprogram_Access_Parameter,
--        Unknown);
--
--     type Cross_Reference_Enable_Array is
--       array (Cross_Reference_Type) of Boolean;

   procedure Add_Entity
     (Table        : in out Entity_Table_Interface;
      Key          : String;
      Item         : Entity_Reference)
   is abstract;

   procedure Add_Cross_Reference
     (Table        : in out Entity_Table_Interface;
      Item         : Entity_Reference;
      File_Name    : String;
      Line, Column : Natural;
      Ref_Type     : String)
   is abstract;

   function Cross_References
     (Table        : Entity_Table_Interface;
      File_Name    : String;
      Line, Column : Positive;
      Enabled      : String := "all")
      return Array_Of_Entities
      is abstract;

   type File_Location is private;

   function Get_Reference
     (Table    : Entity_Table_Interface'Class;
      Location : File_Location)
      return Entity_Reference;

   type File_Location_Array is array (Positive range <>) of File_Location;

   function References
     (Table  : Entity_Table_Interface;
      Entity : Entity_Reference)
      return File_Location_Array
      is abstract;

   function Exists
     (Table : Entity_Table_Interface;
      Key   : String)
      return Boolean
      is abstract;

   function Get
     (Table : Entity_Table_Interface;
      Key   : String)
      return Entity_Reference
      is abstract;

   procedure Sort
     (Table : in out Entity_Table_Interface)
   is abstract;

   procedure Iterate
     (Table   : Entity_Table_Interface;
      Filter  : in String;
      Process : not null access
        procedure (Item : Entity_Reference);
      Top_Level_Only : Boolean := True)
   is abstract;

   function Location_File_Name
     (Table : Entity_Table_Interface;
      Location : File_Location)
      return String
      is abstract;

   function To_String
     (Table : Entity_Table_Interface'Class;
      Location : File_Location)
      return String;

   function File_Line
     (Location : File_Location)
      return Natural;

   function File_Column
     (Location : File_Location)
      return Natural;

   function Location_Reference_Type
     (Location : File_Location)
      return String;

   type Entity_Table is new Entity_Table_Interface with private;

   overriding procedure Add_Entity
     (Table        : in out Entity_Table;
      Key          : String;
      Item         : Entity_Reference);

   overriding function Exists
     (Table : Entity_Table;
      Key   : String)
      return Boolean;

   overriding function Get
     (Table : Entity_Table;
      Key   : String)
      return Entity_Reference;

   overriding procedure Add_Cross_Reference
     (Table        : in out Entity_Table;
      Item         : Entity_Reference;
      File_Name    : String;
      Line, Column : Natural;
      Ref_Type     : String);

   overriding function Cross_References
     (Table        : Entity_Table;
      File_Name    : String;
      Line, Column : Positive;
      Enabled      : String := "")
      return Array_Of_Entities;

   overriding function References
     (Table  : Entity_Table;
      Entity : Entity_Reference)
      return File_Location_Array;

   overriding procedure Sort
     (Table   : in out Entity_Table);

   overriding procedure Iterate
     (Table   : Entity_Table;
      Filter  : in String;
      Process : not null access
        procedure (Item : Entity_Reference);
      Top_Level_Only : Boolean := True);

   overriding function Location_File_Name
     (Table : Entity_Table;
      Location : File_Location)
      return String;

private

   type File_Id is new Positive;

   type File_Location is
      record
         File     : File_Id;
         Ref_Type : Ada.Strings.Unbounded.Unbounded_String;
         Line     : Natural;
         Column   : Natural;
      end record;

   package File_Location_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => File_Id,
        Element_Type => File_Location);

   package File_Name_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => File_Id,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   package File_Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (File_Id, String);

   package Entity_Vectors is
     new Ada.Containers.Vectors
       (Positive, Entity_Reference);

   type Root_Entity_Reference is abstract tagged
      record
         Identifier   : Ada.Strings.Unbounded.Unbounded_String;
         Class        : Ada.Strings.Unbounded.Unbounded_String;
         Display_Text : Ada.Strings.Unbounded.Unbounded_String;
         Description  : Ada.Strings.Unbounded.Unbounded_String;
         References   : File_Location_Vectors.Vector;
      end record;

   package Entity_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Entity_Reference,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type Cross_Reference_Record is
      record
         Entity   : Entity_Reference;
         Ref_Type : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Cross_Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Cross_Reference_Record);

   package Cross_Reference_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Cross_Reference_Lists.List,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive,
        "="             => Cross_Reference_Lists."=");

   type Entity_Table is new Entity_Table_Interface with
      record
         File_Map    : File_Name_Maps.Map;
         File_Vector : File_Name_Vectors.Vector;
         Table       : Entity_Vectors.Vector;
         Map         : Entity_Maps.Map;
         X_Ref       : Cross_Reference_Maps.Map;
         Store       : access Program_Store_Interface'Class;
      end record;

   overriding function Program_Store
     (Table : Entity_Table)
      return access Program_Store_Interface'Class
   is (Table.Store);

   overriding procedure Set_Program_Store
     (Table : in out Entity_Table;
      Store : access Program_Store_Interface'Class);

end Komnenos.Entities;
