private with Ada.Containers.Doubly_Linked_Lists;

limited with Ack.Classes;
with Ack.Features;

package Ack.Types is

   type Type_Entity_Record is
     new Root_Entity_Type with private;

   function Has_Feature
     (Typ   : Type_Entity_Record'Class;
      Name  : Name_Id)
      return Boolean;

   function Feature
     (Typ   : Type_Entity_Record'Class;
      Name  : Name_Id)
      return Ack.Features.Feature_Entity
     with Pre => Typ.Has_Feature (Name);

   function Class
     (Typ : Type_Entity_Record'Class)
      return access Ack.Classes.Class_Entity_Record'Class;

   type Type_Entity is access all Type_Entity_Record'Class;

   function Has_Type_Entity
     (Node : Node_Id)
      return Boolean;

   function Get_Type_Entity
     (Node : Node_Id)
      return Type_Entity
     with Pre => Kind (Node) in N_Class_Name | N_Class_Type
     and then Has_Type_Entity (Node);

   type Array_Of_Types is array (Positive range <>) of Type_Entity;

   Empty_Type_Array : Array_Of_Types (1 .. 0);

   function New_Class_Type
     (Node            : Node_Id;
      Class           : not null access
        Ack.Classes.Class_Entity_Record'Class;
      Detachable      : Boolean)
      return Type_Entity;

   function Instantiate_Generic_Class
     (Node            : Node_Id;
      Generic_Class   : not null access Ack.Classes.Class_Entity_Record'Class;
      Generic_Actuals : Array_Of_Types)
      return Type_Entity;

   function New_Generic_Formal_Type
     (Name          : Name_Id;
      Node          : Node_Id;
      Generic_Class : not null access Ack.Classes.Class_Entity_Record'Class;
      Constraints   : Array_Of_Types := Empty_Type_Array)
      return Type_Entity;

   function Get_Class_Type
     (Class : not null access Ack.Classes.Class_Entity_Record'Class)
      return Type_Entity;

   function Get_Top_Level_Type
     (Name : String)
      return Type_Entity;

private

   type Constant_Type_Entity is access constant Type_Entity_Record'Class;

   type Generic_Argument_Binding is
      record
         Formal : Constant_Type_Entity;
         Actual : Type_Entity;
      end record;

   package List_Of_Generic_Bindings is
     new Ada.Containers.Doubly_Linked_Lists (Generic_Argument_Binding);

   package List_Of_Constraints is
     new Ada.Containers.Doubly_Linked_Lists (Type_Entity);

   type Type_Entity_Record is
     new Root_Entity_Type with
      record
         Class            : access Ack.Classes.Class_Entity_Record'Class;
         Generic_Bindings : List_Of_Generic_Bindings.List;
         Constraints      : List_Of_Constraints.List;
         Generic_Formal   : Boolean := False;
         Detachable       : Boolean := False;
         Anchored         : Boolean := False;
      end record;

   overriding function Instantiate
     (Entity             : not null access Type_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type;

   overriding function Conforms_To
     (Conformer : not null access constant Type_Entity_Record;
      Other     : not null access constant Root_Entity_Type'Class)
      return Boolean;

   overriding function Description
     (Typ       : Type_Entity_Record)
      return String;

   overriding function Full_Name
     (Typ       : Type_Entity_Record)
      return String;

   overriding function Link_Name
     (Typ       : Type_Entity_Record)
      return String;

   overriding function Contains
     (Typ       : Type_Entity_Record;
      Name      : String;
      Recursive : Boolean := True)
      return Boolean;

   overriding function Get
     (Typ  : not null access constant Type_Entity_Record;
      Name : String)
      return Entity_Type;

   overriding procedure Allocate
     (Typ  : Type_Entity_Record;
      Unit : in out Tagatha.Units.Tagatha_Unit);

   function Class
     (Typ : Type_Entity_Record'Class)
      return access Ack.Classes.Class_Entity_Record'Class
   is (if Typ.Generic_Formal then null else Typ.Class);

   function Has_Type_Entity
     (Node : Node_Id)
      return Boolean
   is (Has_Entity (Node)
       and then Get_Entity (Node).all in Type_Entity_Record'Class);

end Ack.Types;
