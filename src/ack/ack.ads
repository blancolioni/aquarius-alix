private with Ada.Characters.Handling;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

private with WL.String_Maps;

with Tagatha.Units;

with Aquarius.Programs;

package Ack is

   type Node_Kind is
     (N_Uninitialized_Node,
      N_Error_Node,
      N_Class_Declaration,
      N_Class_Header,
      N_Class_Name,
      N_Formal_Generics,
      N_Formal_Generic,
      N_Formal_Generic_Name,
      N_Actual_Generics,
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
      N_Entity_Declaration_Group_List,
      N_Entity_Declaration_Group,
      N_Identifier_List,
      N_Class_Type,
      N_Anchored_Type,
      N_Feature_Name,
      N_Feature_Alias,
      N_Feature_Value,
      N_Explicit_Value,
      N_Routine,
      N_Local_Declarations,
      N_Internal,
      N_External,
      N_Expression,
      N_Compound,
      N_Assignment,
      N_Creation_Instruction,
      N_Conditional,
      N_Loop,
      N_Precursor,
      N_Operator,
      N_Constant,
      N_String_Constant,
      N_Integer_Constant,
      N_Variable,
      N_Precursor_Element,
      N_Actual_List,
      N_Identifier);

   subtype N_Type is Node_Kind range
     N_Class_Type .. N_Anchored_Type;

   subtype N_Instruction is Node_Kind range
     N_Assignment .. N_Precursor;

   subtype N_Expression_Node is Node_Kind range
     N_Precursor .. N_Constant;

   subtype N_Constant_Value is Node_Kind range
     N_String_Constant .. N_Integer_Constant;

   subtype N_Effective_Routine is Node_Kind range N_Internal .. N_External;

   type Error_Kind is
     (E_No_Error,
      E_Undeclared_Name,
      E_Redefined_Name,
      E_Not_Defined_In,
      E_Missing_Redefinition,
      E_No_Component,
      E_No_Child,
      E_Id_List_With_Arguments,
      E_Id_List_With_No_Type,
      E_Id_List_With_Routine,
      E_Type_Error,
      E_Insufficient_Arguments,
      E_Too_Many_Arguments,
      E_Does_Not_Accept_Arguments,
      E_Ignored_Return_Value,
      E_Requires_Value
     );

   type Node_Id is private;

   function Kind (Node : Node_Id) return Node_Kind;

   type List_Id is private;

   No_List : constant List_Id;

   function New_List return List_Id;
   procedure Append (List : List_Id;
                     Node : Node_Id);

   function Length
     (List : List_Id)
      return Natural;

   procedure Scan
     (List : List_Id;
      Process : not null access
        procedure (Node : Node_Id));

   type Array_Of_Nodes is array (Positive range <>) of Node_Id;

   function To_Array
     (List : List_Id)
      return Array_Of_Nodes;

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

   function To_Standard_String
     (Name : Name_Id)
      return String;

   type Root_Entity_Type is abstract tagged private;

   type Entity_Type is access all Root_Entity_Type'Class;

   function Standard_Name (Entity : Root_Entity_Type'Class) return String;
   function Declared_Name (Entity : Root_Entity_Type'Class) return String;
   function Qualified_Name (Entity : Root_Entity_Type'Class) return String;
   function Link_Name (Entity : Root_Entity_Type'Class) return String;
   function Base_File_Name (Entity : Root_Entity_Type'Class) return String;
   function Base_Child_File_Name
     (Entity     : Root_Entity_Type;
      Child_Name : Name_Id)
      return String;

   function Description (Entity : Root_Entity_Type) return String
   is (Root_Entity_Type'Class (Entity).Qualified_Name);

   function Conforms_To
     (Class : not null access constant Root_Entity_Type;
      Other : not null access constant Root_Entity_Type'Class)
      return Boolean;

   function Argument_Count (Entity : Root_Entity_Type) return Natural
   is (0);

   function Argument
     (Entity : Root_Entity_Type;
      Index  : Positive)
      return Entity_Type
   is (null)
     with Pre'Class =>
       Root_Entity_Type'Class (Entity).Argument_Count >= Index;

   function Declaration_Node
     (Entity : Root_Entity_Type'Class)
      return Node_Id;

   function Get_Type
     (Entity : Root_Entity_Type'Class)
      return Entity_Type;

   procedure Bind (Entity : in out Root_Entity_Type) is null;

   procedure Push_Entity
     (Entity : Root_Entity_Type;
      Unit   : Tagatha.Units.Tagatha_Unit)
   is null;

   procedure Pop_Entity
     (Entity : Root_Entity_Type;
      Unit   : Tagatha.Units.Tagatha_Unit)
   is null;

   function Contains
     (Table_Entity : Root_Entity_Type;
      Name         : String;
      Recursive    : Boolean := True)
      return Boolean;

   function Get
     (Table_Entity : Root_Entity_Type;
      Name         : String)
      return Entity_Type
   with Pre'Class => Table_Entity.Contains (Name);

   function Contains
     (Table_Entity : Root_Entity_Type'Class;
      Name         : Name_Id)
      return Boolean
   is (Table_Entity.Contains (To_Standard_String (Name)));

   function Get
     (Table_Entity : Root_Entity_Type'Class;
      Name         : Name_Id)
      return Entity_Type
   is (Table_Entity.Get (To_Standard_String (Name)));

   procedure Insert
     (Table_Entity : in out Root_Entity_Type'Class;
      Entity       : not null access Root_Entity_Type'Class)
     with Pre => not Table_Entity.Contains (Entity.Standard_Name, False),
     Post => Table_Entity.Contains (Entity.Standard_Name, False);

   --     type Entity_Type is private;
--
--     No_Entity : constant Entity_Type;
--
--     function Get_Context (Entity : Entity_Type) return Entity_Type;
--     function Get_Name (Entity : Entity_Type) return Name_Id;
--     function Get_Description (Entity : Entity_Type) return String;
--     function Get_Declaration (Entity : Entity_Type) return Node_Id;
--     function Get_Kind (Entity : Entity_Type) return Entity_Kind;
--     function Get_Type (Entity : Entity_Type) return Entity_Type;
--
--     function Get_Virtual_Table_Length (Entity : Entity_Type) return Natural
--       with Pre => Get_Kind (Entity) = Class_Entity;
--     function Get_Property_Count (Entity : Entity_Type) return Natural
--       with Pre => Get_Kind (Entity) = Class_Entity;
--
--     function Get_Virtual_Table_Offset (Entity : Entity_Type) return Natural
--       with Pre => Get_Kind (Entity) = Routine_Feature_Entity;
--
--     function Get_Property_Offset (Entity : Entity_Type) return Natural
--       with Pre => Get_Kind (Entity) = Property_Feature_Entity;
--
--     function Get_Argument_Offset (Entity : Entity_Type) return Natural
--       with Pre => Get_Kind (Entity) = Argument_Entity;
--
--     function Get_Local_Offset (Entity : Entity_Type) return Natural
--       with Pre => Get_Kind (Entity) = Local_Entity;
--
--     function Get_Defined_In (Entity : Entity_Type) return Entity_Type;
--   function Get_Original_Ancestor (Feature : Entity_Type) return Entity_Type
--       with Pre => Get_Kind (Feature) in Feature_Entity_Kind,
--    Post => Get_Kind (Get_Original_Ancestor'Result) in Feature_Entity_Kind;
--
--     function Get_File_Name (Entity : Entity_Type) return String;
--     function Get_Link_Name (Entity : Entity_Type) return String;
--
--     function Get_Formal_Arguments_Node
--       (Entity : Entity_Type)
--        return Node_Id
--       with Pre => Get_Kind (Entity) in Feature_Entity_Kind;
--
--     procedure Create_Current_Entity
--       (Class       : Entity_Type;
--        Feature     : Node_Id;
--        Table       : Entity_Type);
--
--     function New_Entity
--       (Name        : Name_Id;
--        Kind        : Entity_Kind;
--        Context     : Entity_Type;
--        Declaration : Node_Id;
--        Entity_Type : Entity_Type)
--        return Entity_Type
--       with Post => Get_Type (New_Entity'Result) = Entity_Type
--       and then Get_Kind (New_Entity'Result) = Kind
--       and then Get_Declaration (New_Entity'Result) = Declaration;
--
--     procedure Inherit_Entity
--       (Entity        : Entity_Type;
--        Derived_Class : Entity_Type;
--        Declaration   : Node_Id;
--        Redefine      : Boolean;
--        Rename        : Name_Id);
--
--     procedure Instantiate_Entity
--       (Generic_Class  : Entity_Type;
--        Concrete_Class : Entity_Type;
--        Formal_Entity  : Entity_Type;
--        Actual_Entity  : Entity_Type;
--        Declaration    : Node_Id);
--
--     procedure Scan_Children
--       (Entity  : Entity_Type;
--        Process : not null access
--          procedure (Child : Entity_Type));
--
--     procedure Scan_Children
--       (Entity  : Entity_Type;
--        Test    : not null access
--          function (Child : Entity_Type)
--        return Boolean;
--        Process : not null access
--          procedure (Child : Entity_Type));
--
--     function New_Primitive_Class
--       (Name        : Name_Id)
--        return Entity_Type;
--
--     function Find_Entity
--       (Context : Entity_Type;
--        Name    : Name_Id)
--        return Entity_Type;
--
--     function Find_Local_Entity
--       (Context : Entity_Type;
--        Name    : Name_Id)
--        return Entity_Type;

   function Has_Error
     (Node : Node_Id)
      return Boolean;

   function Get_Error
     (Node : Node_Id)
      return Error_Kind;

   function Get_Error_Entity
     (Node : Node_Id)
      return Entity_Type;

   procedure Scan_Errors
     (Top     : Node_Id;
      Process : not null access
        procedure (Node : Node_Id;
                   Error : Error_Kind));

   procedure Error
     (Node   : Node_Id;
      Kind   : Error_Kind;
      Entity : Entity_Type := null);

   function Get_Program
     (N : Node_Id)
      return Aquarius.Programs.Program_Tree;

   function Class_Header (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Declaration;

   function Class_Name (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_Class_Header | N_Class_Type;

   function Actual_Generics (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Type;

   function Formal_Generics (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Header;

   function Formal_Generics_List (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Formal_Generics;

   function Formal_Generic_Name (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Formal_Generic;

   function Inheritance (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Class_Declaration;

   function Inherits (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Inheritance;

   function Inherit_Class_Type (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Inherited;

   function Redefine (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Inherited;

   function Identifiers (N : Node_Id) return List_Id
     with Pre => Kind (N) in N_Class_Name | N_Entity_Declaration_Group;

   function Get_Name (N : Node_Id) return Name_Id
     with Pre => Kind (N) = N_Identifier
     or else Kind (N) = N_Feature_Name
     or else Kind (N) = N_External
     or else Kind (N) = N_Feature_Alias
     or else Kind (N) = N_Variable
     or else Kind (N) = N_Integer_Constant
     or else Kind (N) = N_String_Constant
     or else Kind (N) = N_Precursor_Element
     or else Kind (N) = N_Formal_Generic_Name;

   function Get_Entity (N : Node_Id) return Entity_Type;
   function Has_Entity (N : Node_Id) return Boolean;

   function Class_Features (N : Node_Id) return Node_Id
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

   function Declaration_Count (N : Node_Id) return Natural
     with Pre => Kind (N) = N_Entity_Declaration_Group_List;

   function Group_Type (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Entity_Declaration_Group;

   function Value_Type (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Declaration_Body;

   function Value (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Declaration_Body;

   function Entity_Declaration_Group_List (N : Node_Id) return Node_Id
     with Pre => Kind (N) in N_Formal_Arguments | N_Local_Declarations;

   procedure Scan_Entity_Declarations
     (Group   : Node_Id;
      Process : not null access
        procedure (Declaration_Node : Node_Id))
     with Pre => Kind (Group) = N_Entity_Declaration_Group_List;

   function Feature_Alias (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_External;

   function Effective_Routine (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Routine;

   function Compound (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Internal;

   function Instructions (N : Node_Id) return List_Id
     with Pre => Kind (N) = N_Compound;

   function Variable (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Assignment;

   function Expression (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Assignment;

   function Actual_List (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Precursor_Element;

   function Constant_Value (N : Node_Id) return Node_Id
     with Pre => Kind (N) = N_Constant;

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

   type Program_Id is range 0 .. 99_999_999;

   No_Program : constant Program_Id := 0;
   Error_Program : constant Program_Id := 1;

   subtype Real_Program_Id is Program_Id range 2 .. Program_Id'Last;

   type Field_Index is range 1 .. 6;

   type Node_Field_Array is array (Field_Index) of Node_Id;

   type Node_Record is
      record
         Kind          : Node_Kind  := N_Uninitialized_Node;
         From          : Aquarius.Programs.Program_Tree := null;
         Deferred      : Boolean    := False;
         Expanded      : Boolean    := False;
         Frozen        : Boolean    := False;
         Defining      : Boolean    := False;
         Single        : Boolean    := False;
         Once          : Boolean    := False;
         Field         : Node_Field_Array := (others => No_Node);
         List          : List_Id    := No_List;
         Name          : Name_Id    := No_Name;
         Entity        : Entity_Type := null;
         Error         : Error_Kind := E_No_Error;
         Error_Entity  : Entity_Type := null;
         Integer_Value : Integer;
      end record;

   package List_Of_Nodes is
     new Ada.Containers.Doubly_Linked_Lists (Node_Id);

   type List_Record is
      record
         List : List_Of_Nodes.List;
      end record;

--     package List_Of_Entities is
--       new Ada.Containers.Doubly_Linked_Lists (Entity_Type);
--
--     type Entity_Record is
--        record
--           Redefine          : Boolean;
--           Name              : Name_Id;
--           Kind              : Entity_Kind;
--           Context           : Entity_Type;
--           Defined_In        : Entity_Type;
--           Inherited_From    : Entity_Type;
--           Instantiated_From : Entity_Type;
--           Declaration       : Node_Id;
--           Entity_Type       : Entity_Type;
--           Virtual_Offset    : Natural;
--           Property_Offset   : Natural;
--           Argument_Offset   : Positive;
--           Local_Offset      : Positive;
--           Children          : List_Of_Entities.List;
--        end record;

   package Node_Vectors is
     new Ada.Containers.Vectors (Real_Node_Id, Node_Record);

   package List_Vectors is
     new Ada.Containers.Vectors (Real_List_Id, List_Record);

--     package Entity_Vectors is
--       new Ada.Containers.Vectors (Real_Entity_Type, Entity_Record);

   package Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Real_Name_Id, String);

   package Name_Maps is
     new WL.String_Maps (Name_Id);

   Node_Table   : Node_Vectors.Vector;
   List_Table   : List_Vectors.Vector;
--     Entity_Table : Entity_Vectors.Vector;
   Name_Table   : Name_Vectors.Vector;
   Name_Map     : Name_Maps.Map;

   function To_String
     (Name : Name_Id)
      return String
   is (Name_Table (Name));

   function To_Standard_String
     (Name : Name_Id)
      return String
   is (Ada.Characters.Handling.To_Lower (To_String (Name)));

   function Length
     (List : List_Id)
      return Natural
   is (if List = No_List then 0
       else Natural (List_Table (List).List.Length));

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
   is (Field_2 (N));

   function Actual_Generics (N : Node_Id) return Node_Id
   is (Field_3 (N));

   function Actual_Generics_List (N : Node_Id) return List_Id
   is (Node_Table (N).List)
   with Pre => Kind (N) = N_Actual_Generics;

   function Formal_Generics (N : Node_Id) return Node_Id
   is (Field_3 (N));

   function Formal_Generics_List (N : Node_Id) return List_Id
   is (Node_Table (N).List);

   function Formal_Generic_Name (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Inheritance (N : Node_Id) return Node_Id
   is (Field_3 (N));

   function Inherits (N : Node_Id) return List_Id
   is (Node_Table (N).List);

   function Inherit_Class_Type (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Redefine (N : Node_Id) return List_Id
   is (if Field_4 (N) = No_Node then No_List
       else Node_Table (Field_4 (N)).List);

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

   function Declaration_Count (N : Node_Id) return Natural
   is (Node_Table.Element (N).Integer_Value);

   procedure Set_Declaration_Count
     (N : Node_Id;
      Count : Natural)
     with Pre => Kind (N) = N_Entity_Declaration_Group;

   function Group_Type (N : Node_Id) return Node_Id
   is (Field_1 (N));

   function Value_Type (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (2));

   function Value (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (3));

   function Entity_Declaration_Group_List (N : Node_Id) return Node_Id
   is (Field_1 (N));

--     function Entity_Type (N : Node_Id) return Node_Id
--     is (Field_1 (N));

   function Get_Name (N : Node_Id) return Name_Id
   is (Node_Table.Element (N).Name);

   function Has_Entity (N : Node_Id) return Boolean
   is (Node_Table.Element (N).Entity /= null);

   function Get_Entity (N : Node_Id) return Entity_Type
   is (Node_Table.Element (N).Entity);

   function Class_Features (N : Node_Id) return Node_Id
   is (Node_Table.Element (N).Field (5));

   function Feature_Alias (N : Node_Id) return Node_Id
   is (Field_1 (N));

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

   function Actual_List (N : Node_Id) return Node_Id
   is (Field_2 (N));

   function Constant_Value (N : Node_Id) return Node_Id
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
      return Entity_Type
   is (Node_Table.Element (Node).Error_Entity);

   function New_Node
     (Kind     : Node_Kind;
      From     : Aquarius.Programs.Program_Tree;
      Deferred : Boolean     := False;
      Expanded : Boolean     := False;
      Frozen   : Boolean     := False;
      Defining : Boolean     := False;
      Once     : Boolean     := False;
      Field_1  : Node_Id     := No_Node;
      Field_2  : Node_Id     := No_Node;
      Field_3  : Node_Id     := No_Node;
      Field_4  : Node_Id     := No_Node;
      Field_5  : Node_Id     := No_Node;
      Field_6  : Node_Id     := No_Node;
      List     : List_Id     := No_List;
      Name     : Name_Id     := No_Name;
      Entity   : Entity_Type := null)
    return Node_Id;

   procedure Depth_First_Scan
     (Top : Node_Id;
      Process : not null access
        procedure (Node : Node_Id));

--     function Get_Context (Entity : Entity_Type) return Entity_Type
--     is (Entity_Table.Element (Entity).Context);
--
--     function Get_Defined_In (Entity : Entity_Type) return Entity_Type
--     is (Entity_Table.Element (Entity).Defined_In);
--
--     function Get_Name (Entity : Entity_Type) return Name_Id
--     is (if Entity = No_Entity
--         then Get_Name_Id ("(none)")
--         elsif Entity = Undeclared_Entity
--         then Get_Name_Id ("(undeclared)")
--         else Entity_Table.Element (Entity).Name);
--
--     function Get_Declaration (Entity : Entity_Type) return Node_Id
--     is (Entity_Table.Element (Entity).Declaration);
--
--     function Get_Type (Entity : Entity_Type) return Entity_Type
--     is (Entity_Table.Element (Entity).Entity_Type);
--
--     function Get_Kind (Entity : Entity_Type) return Entity_Kind
--     is (Entity_Table.Element (Entity).Kind);
--
--     function Get_Virtual_Table_Length (Entity : Entity_Type) return Natural
--     is (Entity_Table.Element (Entity).Virtual_Offset);
--
--     function Get_Property_Count (Entity : Entity_Type) return Natural
--     is (Entity_Table.Element (Entity).Property_Offset);
--
--     function Get_Virtual_Table_Offset (Entity : Entity_Type) return Natural
--     is (Entity_Table.Element (Entity).Virtual_Offset);
--
--     function Get_Property_Offset (Entity : Entity_Type) return Natural
--     is (Entity_Table.Element (Entity).Property_Offset);
--
--     function Get_Argument_Offset (Entity : Entity_Type) return Natural
--     is (Entity_Table.Element (Entity).Argument_Offset);
--
--     function Get_Local_Offset (Entity : Entity_Type) return Natural
--     is (Entity_Table.Element (Entity).Local_Offset);

   procedure Set_Entity
     (Node : Real_Node_Id;
      Entity : not null access Root_Entity_Type'Class)
     with Pre => Get_Entity (Node) = null;

   function Contains_Name
     (List : List_Id;
      Name : Name_Id)
      return Boolean;

   package Compiled_Class_Maps is
     new WL.String_Maps (String);

   Class_Object_Paths : Compiled_Class_Maps.Map;

   package Class_Node_Maps is
     new WL.String_Maps (Node_Id);

   Loaded_Classes : Class_Node_Maps.Map;

   package Class_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Node_Id);

   Partial_Class_List : Class_Node_Lists.List;

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   package Entity_Maps is
     new WL.String_Maps (Entity_Type);

   package List_Of_Entities is
     new Ada.Containers.Doubly_Linked_Lists (Entity_Type);

   type Root_Entity_Type is abstract tagged
      record
         Name                : Ada.Strings.Unbounded.Unbounded_String;
         Source_Name         : Ada.Strings.Unbounded.Unbounded_String;
         Declaration_Node    : Node_Id;
         Declaration_Context : Entity_Type;
         Value_Type          : Entity_Type;
         Child_Map           : Entity_Maps.Map;
         Child_List          : List_Of_Entities.List;
         Parent_Environment  : Entity_Type;
      end record;

   function Has_Context
     (Entity : Root_Entity_Type)
      return Boolean
   is (Entity.Declaration_Context /= null);

   function Declaration_Context
     (Entity : Root_Entity_Type)
      return Entity_Type
   is (Entity.Declaration_Context);

   function Standard_Name (Entity : Root_Entity_Type'Class) return String
   is (-Entity.Name);

   function Declared_Name (Entity : Root_Entity_Type'Class) return String
   is (-Entity.Source_Name);

   function Context_Name
     (Entity : Root_Entity_Type'Class;
      Separator : String;
      Standard_Names : Boolean)
      return String
   is ((if Entity.Has_Context
        then Entity.Declaration_Context.Context_Name
          (Separator, Standard_Names) & Separator else "")
       & (if Standard_Names
          then Entity.Standard_Name else Entity.Declared_Name));

   function Qualified_Name (Entity : Root_Entity_Type'Class) return String
   is (Entity.Context_Name (".", False));

   function Link_Name (Entity : Root_Entity_Type'Class) return String
   is (Entity.Context_Name ("__", True));

   function Base_File_Name (Entity : Root_Entity_Type'Class) return String
   is (Entity.Context_Name ("-", True));

   function Base_Child_File_Name
     (Entity     : Root_Entity_Type;
      Child_Name : Name_Id)
      return String
   is (Entity.Base_File_Name & "-" & To_Standard_String (Child_Name));

   function Get_Type
     (Entity : Root_Entity_Type'Class)
      return Entity_Type
   is (Entity.Value_Type);

   function Declaration_Node
     (Entity : Root_Entity_Type'Class)
      return Node_Id
   is (Entity.Declaration_Node);

   procedure Create
     (Entity             : in out Root_Entity_Type'Class;
      Name               : Name_Id;
      Node               : Node_Id;
      Parent_Environment : access Root_Entity_Type'Class := null;
      Context            : access Root_Entity_Type'Class := null);

end Ack;
