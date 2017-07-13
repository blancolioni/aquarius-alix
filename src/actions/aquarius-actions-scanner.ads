private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;
private with Ada.Strings.Unbounded;

with Aquarius.Programs;

package Aquarius.Actions.Scanner is

   type Action_Processor_Interface is abstract tagged limited private;

   procedure Scan_Actions
     (Processor : in out Action_Processor_Interface'Class;
      Top       : Aquarius.Programs.Program_Tree;
      Group     : Action_Group);

   procedure Scan_Expression
     (Processor  : in out Action_Processor_Interface'Class;
      Expression : in Aquarius.Programs.Program_Tree);

   procedure Scan_Action
     (Processor : in out Action_Processor_Interface'Class;
      Action    : in Aquarius.Programs.Program_Tree);

   function Group (Processor : Action_Processor_Interface'Class)
                   return Action_Group;

   function Frame_Contains
     (Processor : Action_Processor_Interface'Class;
      Name      : String)
      return Boolean;

   function Global_Environment_Contains
     (Processor : Action_Processor_Interface'Class;
      Name      : String)
      return Boolean;

   procedure Add_Frame_Entry
     (Processor : in out Action_Processor_Interface'Class;
      Name      : String;
      Offset    : Integer);

   procedure Add_Frame_Entry
     (Processor     : in out Action_Processor_Interface'Class;
      Name          : String;
      Internal_Name : String);

   procedure Add_Global_Entry
     (Processor : in out Action_Processor_Interface'Class;
      Name      : String;
      Immediate : Boolean);

   procedure Add_Global_Function
     (Processor : in out Action_Processor_Interface'Class;
      Name      : String);

   procedure Delete_Frame_Entry
     (Processor : in out Action_Processor_Interface'Class;
      Name      : String);

   procedure Delete_Frame
     (Processor : in out Action_Processor_Interface'Class);

   procedure Push_Frame_Entry
     (Processor : in out Action_Processor_Interface;
      Offset    : Integer)
   is abstract;

   procedure Push_External_Entry
     (Processor : in out Action_Processor_Interface;
      Name      : String;
      Immediate : Boolean)
   is abstract;

   procedure Pop_Frame_Entry
     (Processor : in out Action_Processor_Interface;
      Offset    : Integer)
   is abstract;

   procedure Pop_External_Entry
     (Processor : in out Action_Processor_Interface;
      Name      : String)
   is abstract;

   procedure Pop_Return_Value
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure Push_String_Literal
     (Processor : in out Action_Processor_Interface;
      Literal   : String)
   is abstract;

   procedure Clear_Result
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure Call_Function
     (Processor      : in out Action_Processor_Interface;
      Name           : String;
      Argument_Count : Natural)
   is abstract;

   procedure Declare_External_Function
     (Processor : in out Action_Processor_Interface;
      Name      : String)
   is abstract;

   procedure Call_Property
     (Processor      : in out Action_Processor_Interface;
      Name           : String;
      Argument_Count : Natural)
   is abstract;

   procedure Get_Property
     (Processor      : in out Action_Processor_Interface;
      Name           : String;
      Argument_Count : Natural)
   is abstract;

   procedure Set_Property
     (Processor      : in out Action_Processor_Interface;
      Name           : String)
   is abstract;

   procedure Start_Process
     (Processor  : in out Action_Processor_Interface;
      File_Name  : String;
      Group_Name : String)
   is abstract;

   procedure End_Process
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure Source_File
     (Processor      : in out Action_Processor_Interface;
      Directory_Name : in String;
      File_Name      : in String)
   is null;

   function Source_Path
     (Processor : Action_Processor_Interface'Class)
      return String;

   function Source_Simple_Name
     (Processor : Action_Processor_Interface'Class)
      return String;

   function Source_Base_Name
     (Processor : Action_Processor_Interface'Class)
      return String;

   procedure Current_Source_Location
     (Processor      : in out Action_Processor_Interface'Class;
      Line           : in Natural;
      Column         : in Natural);

   procedure Put_Source_Location
     (Processor      : in out Action_Processor_Interface;
      Line           : in Natural;
      Column         : in Natural)
   is null;

   procedure Start_Function
     (Processor : in out Action_Processor_Interface;
      Name      : in String;
      Arguments : in Aquarius.Programs.Array_Of_Program_Trees;
      Locals    : in Aquarius.Programs.Array_Of_Program_Trees)
   is null;

   procedure End_Function
     (Processor : in out Action_Processor_Interface)
   is null;

   procedure Declare_Local_Variables
     (Process      : in out Action_Processor_Interface;
      Names        : Aquarius.Programs.Array_Of_Program_Trees;
      Inital_Value : Aquarius.Programs.Program_Tree)
   is null;

   procedure Action_Header
     (Processor : in out Action_Processor_Interface;
      Position  : Rule_Position;
      Parent    : String;
      Child     : String)
   is abstract;

   procedure Start_Action_Body
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure End_Action_Body
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure Assign (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure Start_Object_Reference
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure Finish_Object_Reference
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure Start_Aggregate
     (Processor  : in out Action_Processor_Interface;
      Class_Name : String)
   is abstract;

   procedure Start_Aggregate_Element
     (Processor : in out Action_Processor_Interface;
      Name      : in     String)
   is abstract;

   procedure End_Aggregate_Element
     (Processor : in out Action_Processor_Interface;
      Name      : in     String)
   is abstract;

   procedure End_Aggregate
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure Operator
     (Processor : in out Action_Processor_Interface;
      Name      : String)
   is abstract;

   procedure Literal_Number
     (Processor : in out Action_Processor_Interface;
      Value     : Integer)
   is abstract;

   procedure Literal_String
     (Processor : in out Action_Processor_Interface;
      Value     : String)
   is abstract;

   procedure Literal_Null
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure If_Then_Else_Expression
     (Processor   : in out Action_Processor_Interface;
      Sequence    : Aquarius.Programs.Array_Of_Program_Trees)
   is abstract;

   procedure If_Statement
     (Processor   : in out Action_Processor_Interface;
      Expressions : Aquarius.Programs.Array_Of_Program_Trees;
      Statements  : Aquarius.Programs.Array_Of_Program_Trees)
   is abstract;

   procedure Iterator_Statement
     (Processor   : in out Action_Processor_Interface;
      Identifier  : String;
      Statements  : Aquarius.Programs.Program_Tree)
   is abstract;

   procedure Allocate
     (Processor : in out Action_Processor_Interface)
   is null;

private

   type Frame_Entry_Type is (Stack_Offset, Register_Name);

   type Frame_Entry (Entry_Type : Frame_Entry_Type) is
      record
         case Entry_Type is
            when Stack_Offset =>
               Offset : Integer;
            when Register_Name =>
               Name   : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   package Frame_Tables is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Frame_Entry,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type External_Entry is
      record
         Is_Immediate  : Boolean;
         Is_Function   : Boolean;
         External_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package External_Tables is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => External_Entry,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type Action_Processor_Interface is abstract tagged limited
      record
         Top             : Aquarius.Programs.Program_Tree;
         Group           : Action_Group;
         Frame_Table     : Frame_Tables.Map;
         External_Table  : External_Tables.Map;
         Last_Line       : Integer := -1;
         Last_Col        : Integer := -1;
      end record;

end Aquarius.Actions.Scanner;
