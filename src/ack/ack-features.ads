with Tagatha.Units;

private with Ada.Containers.Vectors;

limited with Ack.Classes;
private with Ack.Variables;

package Ack.Features is

   type Feature_Entity_Record is
     new Root_Entity_Type with private;

   overriding procedure Bind
     (Feature : in out Feature_Entity_Record);

   procedure Scan_Original_Classes
     (Feature : Feature_Entity_Record'Class;
      Process : not null access
        procedure (Class : not null access constant
                     Ack.Classes.Class_Entity_Record'Class));

   function Definition_Class
     (Feature : Feature_Entity_Record'Class)
     return access constant Ack.Classes.Class_Entity_Record'Class;

   procedure Add_Argument
     (Feature   : in out Feature_Entity_Record'Class;
      Name_Node : in     Node_Id;
      Arg_Type  : not null access Root_Entity_Type'Class);

   procedure Add_Local
     (Feature    : in out Feature_Entity_Record'Class;
      Name_Node  : in     Node_Id;
      Local_Type : not null access Root_Entity_Type'Class);

   procedure Set_Default_Value
     (Feature : Feature_Entity_Record;
      Unit    : in out Tagatha.Units.Tagatha_Unit);

   procedure Generate_Allocation_Value
     (Feature : Feature_Entity_Record'Class;
      Unit    : in out Tagatha.Units.Tagatha_Unit);

   procedure Generate_Routine
     (Feature : Feature_Entity_Record'Class;
      Unit    : in out Tagatha.Units.Tagatha_Unit);

   type Feature_Entity is access all Feature_Entity_Record'Class;

   function New_Property_Feature
     (Name          : Name_Id;
      Class         : not null access Ack.Classes.Class_Entity_Record'Class;
      Property_Type : not null access Ack.Classes.Class_Entity_Record'Class;
      Declaration   : Node_Id)
      return Feature_Entity;

   function New_Routine_Feature
     (Name          : Name_Id;
      Class         : not null access Ack.Classes.Class_Entity_Record'Class;
      Result_Type   : access Ack.Classes.Class_Entity_Record'Class;
      Declaration   : Node_Id;
      Routine       : Node_Id)
      return Feature_Entity;

   function New_External_Feature
     (Name           : Name_Id;
      Class          : not null access Ack.Classes.Class_Entity_Record'Class;
      External_Type  : String;
      External_Alias : String;
      Result_Type    : access Ack.Classes.Class_Entity_Record'Class;
      Declaration    : Node_Id)
      return Feature_Entity;

   function Is_Feature
     (Entity : not null access constant Root_Entity_Type'Class)
      return Boolean;

private

   package Variable_Vectors is
     new Ada.Containers.Vectors
       (Positive, Ack.Variables.Variable_Entity, Ack.Variables."=");

   type Feature_Entity_Record is
     new Root_Entity_Type with
      record
         Routine          : Boolean := False;
         Property         : Boolean := False;
         Named_Value      : Boolean := False;
         Deferred         : Boolean := False;
         External         : Boolean := False;
         Has_Result       : Boolean := False;
         Has_Current      : Boolean := False;
         External_Object  : Ada.Strings.Unbounded.Unbounded_String;
         External_Type    : Ada.Strings.Unbounded.Unbounded_String;
         External_Label   : Ada.Strings.Unbounded.Unbounded_String;
         Original_Classes : List_Of_Entities.List;
         Definition_Class : access Ack.Classes.Class_Entity_Record'Class;
         Result_Type      : access Ack.Classes.Class_Entity_Record'Class;
         Arguments        : Variable_Vectors.Vector;
         Locals           : Variable_Vectors.Vector;
         Routine_Node     : Node_Id;
      end record;

   overriding function Argument_Count
     (Entity : Feature_Entity_Record)
      return Natural
   is (Entity.Arguments.Last_Index);

   overriding function Argument
     (Entity : Feature_Entity_Record;
      Index  : Positive)
      return Entity_Type
   is (Entity_Type (Entity.Arguments.Element (Index)));

   function Is_Feature
     (Entity : not null access constant Root_Entity_Type'Class)
      return Boolean
   is (Entity.all in Feature_Entity_Record'Class);

end Ack.Features;
