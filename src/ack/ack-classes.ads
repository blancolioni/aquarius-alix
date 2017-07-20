with Ack.Features;

package Ack.Classes is

   type Class_Entity_Record is
     new Root_Entity_Type with private;

   type Class_Entity is access all Class_Entity_Record'Class;

   function Class_Declaration_Context
     (Class : Class_Entity_Record'Class)
      return Class_Entity;

   procedure Add_Constraint
     (Formal : in out Class_Entity_Record'Class;
      Class  : Class_Entity);

   procedure Add_Feature
     (Class : in out Class_Entity_Record'Class;
      Feature : not null access Ack.Features.Feature_Entity_Record'Class);

   procedure Inherit
     (Class           : in out Class_Entity_Record'Class;
      Inherited_Class : not null access Class_Entity_Record'Class);

   procedure Redefine
     (Class           : in out Class_Entity_Record'Class;
      Inherited_Class : not null access Class_Entity_Record'Class;
      Feature_Name    : Name_Id);

   procedure Rename
     (Class            : in out Class_Entity_Record'Class;
      Inherited_Class  : not null access Class_Entity_Record'Class;
      Feature_Name     : Name_Id;
      New_Feature_Name : Name_Id);

   overriding procedure Bind
     (Class : in out Class_Entity_Record);

   function Feature
     (Class : Class_Entity_Record'Class;
      Name  : Name_Id)
      return Ack.Features.Feature_Entity;

   procedure Scan_Features
     (Class : Class_Entity_Record'Class;
      Process : not null access
        procedure (Feature : not null access constant
                     Ack.Features.Feature_Entity_Record'Class));

   procedure Scan_Features
     (Class   : Class_Entity_Record'Class;
      Test    : not null access
        function (Feature : not null access constant
                    Ack.Features.Feature_Entity_Record'Class)
          return Boolean;
      Process : not null access
        procedure (Feature : not null access constant
                     Ack.Features.Feature_Entity_Record'Class));

   function New_Generic_Formal
     (Name        : Name_Id;
      Declaration : Node_Id)
      return Class_Entity;

   procedure Add_Generic_Formal
     (Class  : in out Class_Entity_Record'Class;
      Formal : not null access Class_Entity_Record'Class);

   function New_Class
     (Name        : Name_Id;
      Context     : Class_Entity;
      Declaration : Node_Id)
      return Class_Entity;

   function Get_Class_Entity
     (Node : Node_Id)
      return Class_Entity
     with Pre => Kind (Node) in
     N_Class_Declaration | N_Class_Header | N_Class_Name | N_Class_Type;

   type Array_Of_Classes is
     array (Positive range <>) of Class_Entity;

   function Detachable_Class
     (From_Class : not null access Class_Entity_Record'Class)
      return Class_Entity;

   function New_Instantiated_Class
     (Generic_Class   : Class_Entity;
      Generic_Actuals : Array_Of_Classes;
      Node            : Node_Id)
      return Class_Entity;

   function Get_Top_Level_Class
     (Name : String)
      return Class_Entity;

private

   package List_Of_Class_Entities is
     new Ada.Containers.Doubly_Linked_Lists (Class_Entity);

   package List_Of_Feature_Entities is
     new Ada.Containers.Doubly_Linked_Lists
       (Ack.Features.Feature_Entity, Ack.Features."=");

   type Feature_Rename is
      record
         Old_Name : Name_Id;
         New_Name : Name_Id;
      end record;

   package List_Of_Feature_Renames is
     new Ada.Containers.Doubly_Linked_Lists (Feature_Rename);

   type Inherited_Class_Record is
      record
         Inherited_Class    : Class_Entity;
         Redefined_Features : List_Of_Feature_Entities.List;
         Renamed_Features   : List_Of_Feature_Renames.List;
      end record;

   package List_Of_Inherited_Class_Records is
     new Ada.Containers.Doubly_Linked_Lists (Inherited_Class_Record);

   type Class_Entity_Record is
     new Root_Entity_Type with
      record
         Generic_Class      : Boolean := False;
         Instantiated_Class : Boolean := False;
         Formal_Argument    : Boolean := False;
         Detachable         : Boolean := False;
         Deferred           : Boolean := False;
         Expanded           : Boolean := False;
         Frozen             : Boolean := False;
         Anchored           : Boolean := False;
         Top_Level          : Boolean := False;
         Reference_Entity   : Entity_Type;
         Reference_Class    : Class_Entity;
         Constraints        : List_Of_Class_Entities.List;
         Inherited_Classes  : List_Of_Inherited_Class_Records.List;
         Class_Features     : List_Of_Feature_Entities.List;
         Formal_Arguments   : List_Of_Class_Entities.List;
         Actual_Arguments   : List_Of_Class_Entities.List;
      end record;

   overriding function Has_Context
     (Class : Class_Entity_Record)
      return Boolean
   is (not Class.Top_Level);

   overriding function Description
     (Class : Class_Entity_Record)
      return String
   is ("class " & Class_Entity_Record'Class (Class).Qualified_Name);

   overriding function Conforms_To
     (Class : not null access Class_Entity_Record;
      Other : not null access Root_Entity_Type'Class)
      return Boolean;

   function Class_Declaration_Context
     (Class : Class_Entity_Record'Class)
      return Class_Entity
   is (Class_Entity (Class.Declaration_Context));

   type Generic_Formal_Record is
     new Root_Entity_Type with
      record
         Constraints : List_Of_Class_Entities.List;
      end record;

end Ack.Classes;
