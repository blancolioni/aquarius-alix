with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Aquarius.Semantics is

   pragma Preelaborate (Aquarius.Semantics);

   Max_Semantic_Phases : constant := 6;

   type Semantic_Phase_Count is range 0 .. Max_Semantic_Phases;
   subtype Semantic_Phase_Index is
     Semantic_Phase_Count range 1 .. Semantic_Phase_Count'Last;

   type Semantic_Phase_List is private;

   procedure Add_Phase (To   : in out Semantic_Phase_List;
                        Name : in     String);

   function Phase_Exists (In_List : Semantic_Phase_List;
                          Name    : String)
                         return Boolean;

   function Get_Index (From_List : Semantic_Phase_List;
                       Name      : String)
                      return Semantic_Phase_Index;

   function Get_Count (From_List : Semantic_Phase_List)
                      return Semantic_Phase_Count;

   function Get_Phase_Name (From_List : Semantic_Phase_List;
                            Index     : Semantic_Phase_Index)
                           return String;

private

   type Semantic_Phase_Array is
     array (Semantic_Phase_Index) of Unbounded_String;

   type Semantic_Phase_List is
      record
         Count     : Semantic_Phase_Count  := 0;
         List      : Semantic_Phase_Array;
      end record;

end Aquarius.Semantics;
