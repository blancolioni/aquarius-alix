with Tagatha.Units;

private with Ada.Containers.Vectors;

limited with Ack.Classes;
limited with Ack.Types;

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

   procedure Set_Result_Type
     (Feature     : in out Feature_Entity_Record'Class;
      Result_Type : not null access Ack.Types.Type_Entity_Record'Class);

   procedure Set_Routine
     (Feature      : in out Feature_Entity_Record'Class;
      Routine_Node : Node_Id)
     with Pre => Kind (Routine_Node) = N_Internal;

   procedure Set_Deferred
     (Feature     : in out Feature_Entity_Record'Class);

   procedure Set_Redefined
     (Feature     : in out Feature_Entity_Record'Class;
      Original    : not null access Ack.Classes.Class_Entity_Record'Class);

   procedure Set_External
     (Feature        : in out Feature_Entity_Record'Class;
      External_Type  : String;
      External_Alias : String);

   procedure Set_Explicit_Value
     (Feature : in out Feature_Entity_Record'Class;
      Value   : Node_Id);

   procedure Add_Argument
     (Feature   : in out Feature_Entity_Record'Class;
      Name_Node : in     Node_Id;
      Arg_Type  : not null access Ack.Types.Type_Entity_Record'Class);

   procedure Add_Local
     (Feature    : in out Feature_Entity_Record'Class;
      Name_Node  : in     Node_Id;
      Local_Type : not null access Ack.Types.Type_Entity_Record'Class);

   overriding procedure Add_Implicit
     (Feature    : in out Feature_Entity_Record;
      Implicit_Entity : not null access Root_Entity_Type'Class);

   overriding procedure Remove_Implicit
     (Feature    : in out Feature_Entity_Record);

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

   function New_Feature
     (Name        : Name_Id;
      Declaration : Node_Id;
      Class       : not null access Ack.Classes.Class_Entity_Record'Class)
      return Feature_Entity;

   function Is_Feature
     (Entity : not null access constant Root_Entity_Type'Class)
      return Boolean;

   procedure Set_Feature_Entity
     (Node    : Node_Id;
      Feature : not null access Feature_Entity_Record'Class);

   function Has_Feature_Entity
     (Node    : Node_Id)
      return Boolean;

   function Get_Feature_Entity
     (Node    : Node_Id)
      return Feature_Entity
     with Pre => Has_Feature_Entity (Node);

private

   package Variable_Vectors is
     new Ada.Containers.Vectors
       (Positive, Ack.Variables.Variable_Entity, Ack.Variables."=");

   type Feature_Entity_Record is
     new Root_Entity_Type with
      record
         Routine             : Boolean := False;
         Property            : Boolean := False;
         Explicit_Value      : Boolean := False;
         Deferred            : Boolean := False;
         External            : Boolean := False;
         Has_Result          : Boolean := False;
         Has_Current         : Boolean := False;
         Original_Classes    : List_Of_Entities.List;
         Definition_Class    : access Ack.Classes.Class_Entity_Record'Class;
         External_Object     : Ada.Strings.Unbounded.Unbounded_String;
         External_Type       : Ada.Strings.Unbounded.Unbounded_String;
         External_Label      : Ada.Strings.Unbounded.Unbounded_String;
         Arguments           : Variable_Vectors.Vector;
         Locals              : Variable_Vectors.Vector;
         Routine_Node        : Node_Id;
         Explicit_Value_Node : Node_Id;
         Local_Count         : Natural := 0;
      end record;

   overriding function Instantiate
     (Entity             : not null access Feature_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type;

   overriding function Argument_Count
     (Entity : Feature_Entity_Record)
      return Natural
   is (Entity.Arguments.Last_Index);

   overriding function Argument
     (Entity : Feature_Entity_Record;
      Index  : Positive)
      return Entity_Type
   is (Entity_Type (Entity.Arguments.Element (Index)));

   overriding function Description
     (Feature : Feature_Entity_Record)
      return String;

   overriding function Full_Name
     (Feature : Feature_Entity_Record)
      return String;

   overriding procedure Push_Entity
     (Feature : Feature_Entity_Record;
      Unit    : in out Tagatha.Units.Tagatha_Unit);

   overriding procedure Pop_Entity
     (Feature : Feature_Entity_Record;
      Unit    : in out Tagatha.Units.Tagatha_Unit);

   function Is_Feature
     (Entity : not null access constant Root_Entity_Type'Class)
      return Boolean
   is (Entity.all in Feature_Entity_Record'Class);

   function Has_Feature_Entity
     (Node    : Node_Id)
      return Boolean
   is (Has_Entity (Node) and then Is_Feature (Get_Entity (Node)));

   function Get_Feature_Entity
     (Node    : Node_Id)
      return Feature_Entity
   is (Feature_Entity (Get_Entity (Node)));

end Ack.Features;
