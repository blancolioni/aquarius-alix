private with Ada.Containers.Ordered_Maps;
private with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Less_Case_Insensitive;

with Aquarius.Programs;

package Aquarius.References is

   type Reference_List is private;

   function New_Reference_List
     return Reference_List;

   procedure Add_Specification
     (List          : Reference_List;
      Name          : String;
      Standard_Name : String;
      Key           : String;
      Program       : Aquarius.Programs.Program_Tree);

   procedure Add_Implementation
     (List          : Reference_List;
      Name          : String;
      Standard_Name : String;
      Key           : String;
      Program       : Aquarius.Programs.Program_Tree);

   procedure Add_Reference
     (List      : Reference_List;
      Name      : String;
      Key       : String;
      Program   : Aquarius.Programs.Program_Tree);

   procedure Clear_Source_File
     (List : Reference_List;
      File : String);

   type Reference_Cursor is private;

   type Array_Of_Locations is
     array (Positive range <>) of Reference_Cursor;

   function Filter
     (List : Reference_List;
      Text : String;
      Max  : Positive)
      return Array_Of_Locations;

   function Find
     (List : Reference_List;
      Name : String)
      return Array_Of_Locations;

   function Find_References
     (List  : Reference_List;
      Name  : String;
      Key   : String)
      return Array_Of_Locations;

   function Reference_Name (Position : Reference_Cursor) return String;
   function Reference_Program (Position : Reference_Cursor)
                               return Aquarius.Programs.Program_Tree;

private

   use Ada.Strings.Unbounded;

   type Reference_Entry is
      record
         Name          : Unbounded_String;
         Standard_Name : Unbounded_String;
         Key           : Unbounded_String;
         Program       : Aquarius.Programs.Program_Tree;
      end record;

   package Sorted_List_Of_References is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_String,
        Element_Type => Reference_Entry,
        "<"          => Less_Case_Insensitive);

   type Reference_List_Record is
      record
         Defined_Names : Sorted_List_Of_References.Map;
      end record;

   type Reference_List is access Reference_List_Record;

   type Reference_Cursor is new Sorted_List_Of_References.Cursor;

end Aquarius.References;
