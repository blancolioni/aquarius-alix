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

   type Label_Type is new Natural;

   type Pdp_Scanner is
   limited new Aquarius.Actions.Scanner.Action_Processor_Interface with
      record
         File            : Ada.Text_IO.File_Type;
         Frame_Offset    : Integer := 0;
         Action_Parent   : Boolean;
         Action_Child    : Boolean;
         Next_Label      : Label_Type := 0;
         Object_Start    : Ada.Strings.Unbounded.Unbounded_String;
         Object_Partial  : Aqua.String_Vectors.Vector;
      end record;

   overriding procedure Start_Process
     (Processor  : in out Pdp_Scanner;
      Group_Name : String);

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

   overriding procedure Push_Frame_Entry
     (Processor  : in out Pdp_Scanner;
      Offset     : Integer);

   overriding procedure Push_External_Entry
     (Processor  : in out Pdp_Scanner;
      Name       : String;
      Immediate  : Boolean);

   overriding procedure Push_String_Literal
     (Processor  : in out Pdp_Scanner;
      Literal    : String);

   overriding procedure Pop_Frame_Entry
     (Processor  : in out Pdp_Scanner;
      Offset     : Integer);

   overriding procedure Pop_External_Entry
     (Processor  : in out Pdp_Scanner;
      Name       : String);

   overriding procedure Get_Property
     (Processor : in out Pdp_Scanner;
      Argument_Count : Natural);

   overriding procedure Set_Property
     (Processor : in out Pdp_Scanner);

   overriding procedure Clear_Result
     (Processor : in out Pdp_Scanner);

   overriding procedure Source_File
     (Processor : in out Pdp_Scanner;
      Directory_Name : in String;
      File_Name      : in String);

   overriding procedure Put_Source_Location
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
