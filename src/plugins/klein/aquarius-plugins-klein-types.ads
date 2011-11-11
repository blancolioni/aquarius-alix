private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Aquarius.Programs;
with Aquarius.Types;

private package Aquarius.Plugins.Klein.Types is

   pragma Elaborate_Body (Klein.Types);

   function Create_Integer_Type (Start, Bound : Integer)
                                return Aquarius.Types.Aquarius_Type;

   function Create_Enumeration_Type return Aquarius.Types.Aquarius_Type;

   --  Error_Type is used to give types to declarations that
   --  have a problem in their type definition, to avoid problems
   --  later (and to indicate that we shouldn't generate type-based
   --  error messages for the declaration).
   --  The 'Restrictions' argument is used to pass what we know
   --  about the type based on its context; e.g. if it appears inside
   --  an array range, it must be discrete
   function New_Error_Type
     (Restrictions : Aquarius.Types.Aquarius_Type)
     return Aquarius.Types.Aquarius_Type;

   procedure Add_Literal
     (To_Enumeration : in Aquarius.Types.Aquarius_Type;
      Name           : in String;
      Declaration    : in Aquarius.Programs.Program_Tree);

   procedure Set_Literal_Value
     (In_Enumeration : in Aquarius.Types.Aquarius_Type;
      Name           : in String;
      Value          : in Natural);

   function Universal_Integer return Aquarius.Types.Aquarius_Type;

   function Universal_Boolean return Aquarius.Types.Aquarius_Type;

   type Root_Klein_Type is
     abstract new Aquarius.Types.Root_Aquarius_Type with private;

   type Klein_Type is access all Root_Klein_Type'Class;

   type Error_Type is new Root_Klein_Type with private;

   type Elementary_Type is abstract new Root_Klein_Type with private;

   type Composite_Type is abstract new Root_Klein_Type with private;

   type Access_Type is new Elementary_Type with private;

   type Scalar_Type is abstract new Elementary_Type with private;

   type Discrete_Type is abstract new Scalar_Type with private;

   type Enumeration_Type is new Discrete_Type with private;

   type Integer_Type is abstract new Discrete_Type with private;

   type Universal_Integer_Type is new Integer_Type with private;

   type Signed_Integer_Type is new Integer_Type with private;

   type Unsigned_Integer_Type is new Integer_Type with private;

   type Record_Type is new Composite_Type with private;

   function New_Record_Type return Aquarius.Types.Aquarius_Type;
   procedure Add_Component (To_Record : Aquarius.Types.Aquarius_Type;
                            Component : Aquarius.Entries.Table_Entry);
   function Get_Component (From_Record : Aquarius.Types.Aquarius_Type;
                           Name        : String)
                          return Aquarius.Entries.Table_Entry;

private

   type Root_Klein_Type is
     abstract new Aquarius.Types.Root_Aquarius_Type with null record;

   overriding
   function Create_Derived_Type (Item : Root_Klein_Type)
                                return Aquarius.Types.Aquarius_Type;
   use Aquarius.Types;

   type Elementary_Type is abstract new Root_Klein_Type with null record;

   type Composite_Type is abstract new Root_Klein_Type with null record;

   type Access_Type is new Elementary_Type with
      record
         Access_To : Klein_Type;
      end record;

   overriding
   function Description (Item : Access_Type) return String;

   type Scalar_Type is abstract new Elementary_Type with null record;

   type Discrete_Type is abstract new Scalar_Type with null record;

   type Enumeration_Type_Entry is
      record
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Value       : Natural;
         Declaration : Aquarius.Programs.Program_Tree;
      end record;

   package Enumeration_Vector is
     new Ada.Containers.Vectors (Positive, Enumeration_Type_Entry);

   type Enumeration_Type is new Discrete_Type with
      record
         Literals : Enumeration_Vector.Vector;
      end record;

   overriding
   function Description (Item : Enumeration_Type)
                          return String;

   type Integer_Type is abstract new Discrete_Type with null record;

   type Universal_Integer_Type is new Integer_Type with null record;

   overriding
   function Unify (Item      : not null access Universal_Integer_Type;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type;

   overriding
   function Description (Item : Universal_Integer_Type)
                 return String;

   type Signed_Integer_Type is new Integer_Type with
      record
         Start : Integer;
         Bound : Integer;
      end record;

   overriding
   function Description
     (Item : Signed_Integer_Type)
     return String;

   type Unsigned_Integer_Type is new Integer_Type with
      record
         Modulus : Natural;
      end record;

   overriding
   function Description
     (Item : Unsigned_Integer_Type)
     return String;

   type Record_Type is new Composite_Type with
      record
         Components : Aquarius.Entries.Symbol_Table;
      end record;

   overriding
   function Description (Item : Record_Type) return String;

   type Error_Type is new Root_Klein_Type with
      record
         Restrictions : Aquarius.Types.Aquarius_Type;
      end record;

   overriding
   function Description (Item : Error_Type) return String;

   overriding
   function Unify (Item      : not null access Error_Type;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type;

end Aquarius.Plugins.Klein.Types;
