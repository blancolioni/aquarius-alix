with Aquarius.Programs;

package Aquarius.Actions.Scanner is

   type Action_Processor_Interface is limited interface;

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

   procedure Start_Process
     (Processor : in out Action_Processor_Interface;
      Top       : Aquarius.Programs.Program_Tree;
      Group     : Action_Group)
   is abstract;

   procedure End_Process
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure Source_File
     (Processor      : in out Action_Processor_Interface;
      Directory_Name : in String;
      File_Name      : in String)
   is null;

   procedure Current_Source_Location
     (Processor      : in out Action_Processor_Interface;
      Line           : in Natural;
      Column         : in Natural)
   is null;

   procedure Argument_Name
     (Process   : in out Action_Processor_Interface;
      Index     : in     Positive;
      Name      : in     String)
   is null;

   procedure Local_Variable_Name
     (Process   : in out Action_Processor_Interface;
      Index     : in     Positive;
      Name      : in     String)
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

   procedure Start_Aggregate
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure Start_Aggregate_Element
     (Processor : in out Action_Processor_Interface;
      Name      : in     String)
   is abstract;

   procedure End_Aggregate_Element
     (Processor : in out Action_Processor_Interface)
   is abstract;

   procedure End_Aggregate
     (Processor : in out Action_Processor_Interface)
   is abstract;

   type Object_Reference_Context is (Assignment_Target,
                                     Evaluation, Allocation, Call);

   procedure Start_Object_Reference
     (Processor  : in out Action_Processor_Interface;
      Context    : Object_Reference_Context;
      Identifier : String;
      Arguments  : Aquarius.Programs.Array_Of_Program_Trees;
      Last       : Boolean)
   is abstract;

   procedure Component_Selector
     (Processor  : in out Action_Processor_Interface;
      Identifier : String;
      Arguments  : Aquarius.Programs.Array_Of_Program_Trees;
      Last       : Boolean)
   is abstract;

   procedure Subtree_Selector
     (Processor  : in out Action_Processor_Interface;
      Identifier : String;
      Last       : Boolean)
   is abstract;

   procedure Ancestor_Selector
     (Processor  : in out Action_Processor_Interface;
      Identifier : String;
      Last       : Boolean)
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

end Aquarius.Actions.Scanner;
