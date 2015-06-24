private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;
private with Ada.Strings.Unbounded;

private with Ada.Text_IO;

private with Aqua.String_Vectors;

with Aquarius.Actions.Scanner;
with Aquarius.Programs;

package Aquarius.Actions.Pdp_11 is

   type Pdp_Scanner is
     limited new Aquarius.Actions.Scanner.Action_Processor_Interface
   with private;

   function Output_Path
     (Scanner : Pdp_Scanner'Class)
      return String;

private

   type Storage_Type is (Local, External, Subroutine);

   type Table_Entry (Storage : Storage_Type) is
      record
         case Storage is
            when Local =>
               Frame_Offset : Integer;
            when External | Subroutine =>
               External_Name : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   package Symbol_Tables is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Table_Entry,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type Label_Type is new Natural;

   type Partial_Object_Array is array (1 .. 10) of Symbol_Tables.Cursor;

   type Pdp_Scanner is
   limited new Aquarius.Actions.Scanner.Action_Processor_Interface with
      record
         Top             : Aquarius.Programs.Program_Tree;
         File            : Ada.Text_IO.File_Type;
         Group           : Action_Group;
         Global_Table    : Symbol_Tables.Map;
         Frame_Table     : Symbol_Tables.Map;
         Frame_Offset    : Integer := 0;
         Action_Parent   : Boolean;
         Action_Child    : Boolean;
         Context         : Scanner.Object_Reference_Context;
         Next_Label      : Label_Type := 0;
         Last_Line       : Integer := -1;
         Last_Col        : Integer := -1;
         Object_Start    : Symbol_Tables.Cursor;
         Object_Partial  : Aqua.String_Vectors.Vector;
      end record;

   overriding procedure Start_Process
     (Processor : in out Pdp_Scanner;
      Top       : Aquarius.Programs.Program_Tree;
      Group     : Action_Group);

   overriding procedure End_Process
     (Processor : in out Pdp_Scanner);

   overriding procedure Action_Header
     (Processor : in out Pdp_Scanner;
      Position  : Rule_Position;
      Parent    : String;
      Child     : String);

   overriding procedure Start_Action_Body
     (Processor : in out Pdp_Scanner);

   overriding procedure End_Action_Body
     (Processor : in out Pdp_Scanner);

   overriding procedure Assign
     (Processor : in out Pdp_Scanner);

   overriding procedure Start_Aggregate
     (Processor : in out Pdp_Scanner);

   overriding procedure Start_Aggregate_Element
     (Processor : in out Pdp_Scanner;
      Name      : in     String);

   overriding procedure End_Aggregate_Element
     (Processor : in out Pdp_Scanner);

   overriding procedure End_Aggregate
     (Processor : in out Pdp_Scanner);

   overriding procedure Start_Object_Reference
     (Processor  : in out Pdp_Scanner;
      Context    : Scanner.Object_Reference_Context;
      Identifier : String;
      Arguments  : Aquarius.Programs.Array_Of_Program_Trees;
      Last       : Boolean);

   overriding procedure Component_Selector
     (Processor  : in out Pdp_Scanner;
      Identifier : String;
      Arguments  : Aquarius.Programs.Array_Of_Program_Trees;
      Last       : Boolean);

   overriding procedure Subtree_Selector
     (Processor  : in out Pdp_Scanner;
      Identifier : String;
      Last       : Boolean);

   overriding procedure Ancestor_Selector
     (Processor  : in out Pdp_Scanner;
      Identifier : String;
      Last       : Boolean);

   overriding procedure Source_File
     (Processor : in out Pdp_Scanner;
      Directory_Name : in String;
      File_Name      : in String);

   overriding procedure Current_Source_Location
     (Processor : in out Pdp_Scanner;
      Line           : in Natural;
      Column         : in Natural);

   overriding procedure Operator
     (Processor : in out Pdp_Scanner;
      Name      : String);

   overriding procedure Literal_Number
     (Processor : in out Pdp_Scanner;
      Value     : Integer);

   overriding procedure Literal_String
     (Processor : in out Pdp_Scanner;
      Value     : String);

   overriding procedure Literal_Null
     (Processor : in out Pdp_Scanner);

   overriding procedure If_Statement
     (Processor : in out Pdp_Scanner;
      Expressions : Aquarius.Programs.Array_Of_Program_Trees;
      Statements  : Aquarius.Programs.Array_Of_Program_Trees);

   overriding procedure Iterator_Statement
     (Processor : in out Pdp_Scanner;
      Identifier  : String;
      Statements  : Aquarius.Programs.Program_Tree);

end Aquarius.Actions.Pdp_11;
