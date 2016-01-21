private with Ada.Strings.Fixed.Hash;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Hashed_Maps;

with Aquarius.Actions.Scanner;
with Aquarius.Programs;

private with Tagatha.Units;

package Aquarius.Actions.Tagatha_Scanner is

   type Tagatha_Scanner is
     limited new Aquarius.Actions.Scanner.Action_Processor_Interface
   with private;

   function Output_Path
     (Scanner : Tagatha_Scanner'Class)
      return String;

   procedure Write
     (Scanner : in out Tagatha_Scanner'Class);

private

   package String_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   type Tagatha_Scanner is
   limited new Aquarius.Actions.Scanner.Action_Processor_Interface with
      record
         Unit                : Tagatha.Units.Tagatha_Unit;
         Frame_Offset        : Integer := 0;
         Action_Parent       : Boolean;
         Action_Child        : Boolean;
         Shared_Binding      : Boolean := False;
         Object_Start        : Ada.Strings.Unbounded.Unbounded_String;
         Property_Name       : Ada.Strings.Unbounded.Unbounded_String;
         Nested_Loops        : Natural := 0;
         Nested_Aggregates   : Natural := 0;
         Nested_Properties   : Natural := 0;
         Local_Strings       : String_Maps.Map;
         Next_Routine        : Natural := 0;
         Next_String_Label   : Natural := 0;
      end record;

   overriding procedure Start_Process
     (Processor  : in out Tagatha_Scanner;
      File_Name  : String;
      Group_Name : String);

   overriding procedure End_Process
     (Processor : in out Tagatha_Scanner);

   overriding procedure Start_Function
     (Processor : in out Tagatha_Scanner;
      Name      : in String;
      Arguments : in Aquarius.Programs.Array_Of_Program_Trees;
      Locals    : in Aquarius.Programs.Array_Of_Program_Trees);

   overriding procedure End_Function
     (Processor : in out Tagatha_Scanner);

   overriding procedure Action_Header
     (Processor : in out Tagatha_Scanner;
      Position  : Rule_Position;
      Parent    : String;
      Child     : String);

   overriding procedure Start_Action_Body
     (Processor : in out Tagatha_Scanner);

   overriding procedure End_Action_Body
     (Processor : in out Tagatha_Scanner);

   overriding procedure Assign
     (Processor : in out Tagatha_Scanner);

   overriding procedure Start_Object_Reference
     (Processor  : in out Tagatha_Scanner);

   overriding procedure Finish_Object_Reference
     (Processor : in out Tagatha_Scanner);

   overriding procedure Start_Aggregate
     (Processor : in out Tagatha_Scanner);

   overriding procedure Start_Aggregate_Element
     (Processor : in out Tagatha_Scanner;
      Name      : in     String);

   overriding procedure End_Aggregate_Element
     (Processor : in out Tagatha_Scanner;
      Name      : in     String);

   overriding procedure End_Aggregate
     (Processor : in out Tagatha_Scanner);

   overriding procedure Push_Frame_Entry
     (Processor  : in out Tagatha_Scanner;
      Offset     : Integer);

   overriding procedure Push_External_Entry
     (Processor  : in out Tagatha_Scanner;
      Name       : String;
      Immediate  : Boolean);

   overriding procedure Push_String_Literal
     (Processor  : in out Tagatha_Scanner;
      Literal    : String);

   overriding procedure Pop_Frame_Entry
     (Processor  : in out Tagatha_Scanner;
      Offset     : Integer);

   overriding procedure Pop_External_Entry
     (Processor  : in out Tagatha_Scanner;
      Name       : String);

   overriding procedure Pop_Return_Value
     (Processor  : in out Tagatha_Scanner);

   overriding procedure Call_Function
     (Processor      : in out Tagatha_Scanner;
      Name           : String;
      Argument_Count : Natural);

   overriding procedure Get_Property
     (Processor      : in out Tagatha_Scanner;
      Name           : String;
      Argument_Count : Natural);

   overriding procedure Set_Property
     (Processor : in out Tagatha_Scanner;
      Name      : String);

   overriding procedure Clear_Result
     (Processor : in out Tagatha_Scanner);

   overriding procedure Source_File
     (Processor : in out Tagatha_Scanner;
      Directory_Name : in String;
      File_Name      : in String);

   overriding procedure Put_Source_Location
     (Processor : in out Tagatha_Scanner;
      Line           : in Natural;
      Column         : in Natural);

   overriding procedure Operator
     (Processor : in out Tagatha_Scanner;
      Name      : String);

   overriding procedure Literal_Number
     (Processor : in out Tagatha_Scanner;
      Value     : Integer);

   overriding procedure Literal_String
     (Processor : in out Tagatha_Scanner;
      Value     : String);

   overriding procedure Literal_Null
     (Processor : in out Tagatha_Scanner);

   overriding procedure If_Then_Else_Expression
     (Processor : in out Tagatha_Scanner;
      Sequence  : Aquarius.Programs.Array_Of_Program_Trees);

   overriding procedure If_Statement
     (Processor : in out Tagatha_Scanner;
      Expressions : Aquarius.Programs.Array_Of_Program_Trees;
      Statements  : Aquarius.Programs.Array_Of_Program_Trees);

   overriding procedure Iterator_Statement
     (Processor : in out Tagatha_Scanner;
      Identifier  : String;
      Statements  : Aquarius.Programs.Program_Tree);

end Aquarius.Actions.Tagatha_Scanner;
