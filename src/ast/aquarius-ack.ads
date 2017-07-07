private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;

private with WL.String_Maps;

with Aquarius.Programs;

package Aquarius.Ack is

   type Node_Kind is
     (N_Uninitialized_Node,
      N_Error_Node,
      N_Class_Declaration,
      N_Class_Header,
      N_Class_Name,
      N_Inheritance,
      N_Inherited,
      N_New_Exports,
      N_Undefine,
      N_Redefine,
      N_Rename,
      N_Select,
      N_Features,
      N_Feature_Clause,
      N_Feature_Declaration,
      N_New_Feature,
      N_Declaration_Body,
      N_Formal_Arguments,
      N_Entity_Declaration_Group,
      N_Identifier_List,
      N_Class_Type,
      N_Anchored_Type,
      N_Feature_Name,
      N_Feature_Value,
      N_Explicit_Value,
      N_Routine,
      N_Local_Declarations,
      N_Effective_Routine,
      N_Compound,
      N_Assignment,
      N_Creation_Instruction,
      N_Conditional,
      N_Expression,
      N_Operator,
      N_Precursor,
      N_Constant,
      N_Variable,
      N_Identifier);

   subtype N_Type is Node_Kind range
     N_Class_Type .. N_Anchored_Type;

   subtype N_Instruction is Node_Kind range
     N_Assignment .. N_Conditional;

   subtype N_Expression_Node is Node_Kind range
     N_Operator .. N_Constant;

   type Error_Kind is
     (E_No_Error,
      E_Undeclared_Name,
      E_No_Component,
      E_No_Child,
      E_Id_List_With_Arguments,
      E_Id_List_With_No_Type,
      E_Id_List_With_Routine,
      E_Type_Error
     );

   type Entity_Kind is
     (Table_Entity,
      Class_Entity,
      Feature_Entity,
      Argument_Entity,
      Local_Entity
     );

   subtype Local_Entity_Kind is Entity_Kind range
     Argument_Entity .. Local_Entity;

   type Node_Id is private;

   function Kind (Node : Node_Id) return Node_Kind;

   type List_Id is private;

   No_List : constant List_Id;

   function New_List return List_Id;
   procedure Append (List : List_Id;
                     Node : Node_Id);

   procedure Scan
     (List : List_Id;
      Process : not null access
        procedure (Node : Node_Id));

   type Name_Id is private;

   No_Name : constant Name_Id;

   function Find_Name_Id
     (Name : String)
      return Name_Id;

   function Get_Name_Id
     (Name : String)
      return Name_Id;

   function To_String
     (Name : Name_Id)
      return String;

   type Entity_Id is private;

   No_Entity : constant Entity_Id;

   function Get_Context (Entity : Entity_Id) return Entity_Id;
   function Get_Name (Entity : Entity_Id) return Name_Id;
   function Get_Declaration (Entity : Entity_Id) return Node_Id;
   function Get_Type (Entity : Entity_Id) return Entity_Id;

   function New_Entity
     (Name        : Name_Id;
      Kind        : Entity_Kind;
      Context     : Entity_Id;
      Declaration : Node_Id;
      Entity_Type : Entity_Id)
      return Entity_Id;

   function Find_Entity
     (Context : Entity_Id;
      Name    : Name_Id)
      return Entity_Id;

   function Has_Error
     (Node : Node_Id)
      return Boolean;

   function Get_Error
     (Node : Node_Id)
      return Error_Kind;

   function Get_Error_Entity
     (Node : Node_Id)
      return Entity_Id;

   procedure Scan_Errors
     (Top     : Node_Id;
      Process : not null access
        procedure (Node : Node_Id;
                   Error : Error_Kind));

   procedure Error
     (Node   : Node_Id;
      Kind   : Error_Kind;
      Entity : Entity_Id := No_Entity);

   function Get_Program
     (N : Node_Id)
      return Aquarius.Programs.Program_Tree;

   function Class_Header (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Declaration;

   function Class_Name (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_Class_Header | N_Class_Type;

   function Identifiers (N : Node_Id) return List_Id
     with Pre => Kind (N) in N_Class_Name | N_Entity_Declaration_Group;

   function Get_Name (N : Node_Id) return Name_Id
     with Pre => Kind (N) = N_Identifier
     or else Kind (N) = N_Feature_Name
     or else Kind (N) = N_Variable;

   function Get_Entity (N : Node_Id) return Entity_Id;

   function Features (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Declaration;

   function Feature_Clauses (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Features;

   function Feature_Declarations (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Feature_Clause;

   function New_Feature_List (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Feature_Declaration;

   function Feature_Name (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_New_Feature;

   function Declaration_Body (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Feature_Declaration;

   function Formal_Arguments (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Declaration_Body;

   function Value_Type (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Declaration_Body;

   function Value (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Declaration_Body;

   function Entity_Declaration_Groups (N : Node_Id) return List_Id
     with Pre => Kind (N) in N_Formal_Arguments | N_Local_Declarations;

   function Effective_Routine (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Routine;

   function Compound (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Effective_Routine;

   function Instructions (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Compound;

   function Variable (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Assignment;

   function Expression (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Assignment;

   function Entity_Type (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Entity_Declaration_Group;

private

   type Node_Id is range 0 .. 99_999_999;

   No_Node : constant Node_Id := 0;
   Error_Node : constant Node_Id := 1;

   subtype Real_Node_Id is Node_Id range 2 .. Node_Id'Last;

   type List_Id is range 0 .. 99_999_999;

   No_List : constant List_Id := 0;

   subtype Real_List_Id is List_Id range 1 .. List_Id'Last;

   type Name_Id is range 0 .. 99_999_999;

   No_Name : constant Name_Id := 0;
   Error_Name : constant Name_Id := 1;

   subtype Real_Name_Id is Name_Id range 2 .. Name_Id'Last;

   type Entity_Id is range 0 .. 99_999_999;

   No_Entity         : constant Entity_Id := 0;
   Error_Entity      : constant Entity_Id := 1;
   Undeclared_Entity : constant Entity_Id := 2;

   subtype Real_Entity_Id is Entity_Id range 3 .. Entity_Id'Last;

   type Program_Id is range 0 .. 99_999_999;

   No_Program : constant Program_Id := 0;
   Error_Program : constant Program_Id := 1;

   subtype Real_Program_Id is Program_Id range 2 .. Program_Id'Last;

   type Field_Index is range 1 .. 6;

   type Node_Field_Array is array (Field_Index) of Node_Id;

   type Node_Record is
      record
         Kind         : Node_Kind  := N_Uninitialized_Node;
         From         : Aquarius.Programs.Program_Tree := null;
         Deferred     : Boolean    := False;
         Expanded     : Boolean    := False;
         Frozen       : Boolean    := False;
         Defining     : Boolean    := False;
         Single       : Boolean    := False;
         Once         : Boolean    := False;
         Field        : Node_Field_Array := (others => No_Node);
         List         : List_Id    := No_List;
         Name         : Name_Id    := No_Name;
         Entity       : Entity_Id  := No_Entity;
         Error        : Error_Kind := E_No_Error;
         Error_Entity : Entity_Id;
      end record;

   package List_Of_Nodes is
     new Ada.Containers.Doubly_Linked_Lists (Node_Id);

   type List_Record is
      record
         List : List_Of_Nodes.List;
      end record;

   package List_Of_Entities is
     new Ada.Containers.Doubly_Linked_Lists (Entity_Id);

   type Entity_Record is
      record
         Name        : Name_Id;
         Kind        : Entity_Kind;
         Context     : Entity_Id;
         Declaration : Node_Id;
         Entity_Type : Entity_Id;
         Children    : List_Of_Entities.List;
      end record;

   package Node_Vectors is
     new Ada.Containers.Vectors (Real_Node_Id, Node_Record);

   package List_Vectors is
     new Ada.Containers.Vectors (Real_List_Id, List_Record);

   package Entity_Vectors is
     new Ada.Containers.Vectors (Real_Entity_Id, Entity_Record);

   package Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Real_Name_Id, String);

   package Name_Maps is
     new WL.String_Maps (Name_Id);

   Node_Table   : Node_Vectors.Vector;
   List_Table   : List_Vectors.Vector;
   Entity_Table : Entity_Vectors.Vector;
   Name_Table   : Name_Vectors.Vector;
   Name_Map     : Name_Maps.Map;

   function To_String
     (Name : Name_Id)
      return String
   is (Name_Table (Name));

   function Get_Program
     (N : Node_Id)
      return Aquarius.Programs.Program_Tree
   is (Node_Table (N).From);

   function Field_1 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (1));

   function Field_2 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (2));

   function Field_3 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (3));

   function Field_4 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (4));

   function Field_5 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (5));

   function Field_6 (Node : Node_Id) return Node_Id
   is (Node_Table (Node).Field (6));

   function Kind (Node : Node_Id) return Node_Kind
   is (Node_Table.Element (Node).Kind);

   function Class_Header (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (2));

   function Class_Name (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (2));

   function Identifiers (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Feature_Clauses (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Feature_Declarations (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function New_Feature_List (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Feature_Name (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (1));

   function Declaration_Body (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (1));

   function Formal_Arguments (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (1));

   function Value_Type (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (2));

   function Value (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (3));

   function Entity_Declaration_Groups (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Entity_Type (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Get_Name (N : Node_Id) return Name_Id
   is (Node_Table.Element (N).Name);

   function Get_Entity (N : Node_Id) return Entity_Id
   is (Node_Table.Element (N).Entity);

   function Features (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (5));

   function Effective_Routine (N : Node_Id) return Node_Id
   is (Field_3 (N));

   function Compound (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Instructions (N : Node_Id) return List_Id
   is (Node_Table.Element (N).List);

   function Variable (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Expression (N : Node_Id) return Node_Id
   is (Field_2 (N));

   function Has_Error
     (Node : Node_Id)
      return Boolean
   is (Node_Table.Element (Node).Error /= E_No_Error);

   function Get_Error
     (Node : Node_Id)
      return Error_Kind
   is (Node_Table.Element (Node).Error);

   function Get_Error_Entity
     (Node : Node_Id)
      return Entity_Id
   is (Node_Table.Element (Node).Error_Entity);

   function New_Node
     (Kind     : Node_Kind;
      From     : Aquarius.Programs.Program_Tree;
      Deferred : Boolean    := False;
      Expanded : Boolean    := False;
      Frozen   : Boolean    := False;
      Defining : Boolean    := False;
      Once     : Boolean    := False;
      Field_1  : Node_Id    := No_Node;
      Field_2  : Node_Id    := No_Node;
      Field_3  : Node_Id    := No_Node;
      Field_4  : Node_Id    := No_Node;
      Field_5  : Node_Id    := No_Node;
      Field_6  : Node_Id    := No_Node;
      List     : List_Id    := No_List;
      Name     : Name_Id    := No_Name;
      Entity   : Entity_Id  := No_Entity)
    return Node_Id;

   procedure Depth_First_Scan
     (Top : Node_Id;
      Process : not null access
        procedure (Node : Node_Id));

   function Get_Context (Entity : Entity_Id) return Entity_Id
   is (Entity_Table.Element (Entity).Context);

   function Get_Name (Entity : Entity_Id) return Name_Id
   is (if Entity = No_Entity
       then Get_Name_Id ("(none)")
       elsif Entity = Undeclared_Entity
       then Get_Name_Id ("(undeclared)")
       else Entity_Table.Element (Entity).Name);

   function Get_Declaration (Entity : Entity_Id) return Node_Id
   is (Entity_Table.Element (Entity).Declaration);

   function Get_Type (Entity : Entity_Id) return Entity_Id
   is (Entity_Table.Element (Entity).Entity_Type);

   procedure Set_Entity
     (Node : Real_Node_Id;
      Entity : Entity_Id)
     with Pre => Entity /= No_Entity and then Get_Entity (Node) = No_Entity;

end Aquarius.Ack;
