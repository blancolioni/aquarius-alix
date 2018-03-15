private with WL.String_Sets;

with Ack.Features;
limited with Ack.Types;

package Ack.Classes is

   type Class_Behaviour is
     (Normal, Aqua_Primitive);

   type Class_Entity_Record is
     new Root_Entity_Type with private;

   type Class_Entity is access all Class_Entity_Record'Class;
   type Constant_Class_Entity is access constant Class_Entity_Record'Class;

   function Behaviour
     (Class : Class_Entity_Record'Class)
      return Class_Behaviour;

   function Aqua_Primitive_Behaviour
     (Class : Class_Entity_Record'Class)
      return Boolean;

   function Class_Declaration_Context
     (Class : Class_Entity_Record'Class)
      return Class_Entity;

   procedure Add_Feature
     (Class : in out Class_Entity_Record'Class;
      Feature : not null access Ack.Features.Feature_Entity_Record'Class);

   procedure Add_Creator
     (Class : in out Class_Entity_Record'Class;
      Name  : Name_Id);

   procedure Add_Note
     (Class : in out Class_Entity_Record'Class;
      Name  : String;
      Value : String);

   function Has_Note
     (Class : Class_Entity_Record'Class;
      Name  : String)
      return Boolean;

   function Get_Note
     (Class : Class_Entity_Record'Class;
      Name  : String)
      return String
     with Pre => Class.Has_Note (Name);

   function Deferred
     (Class : Class_Entity_Record'Class)
      return Boolean;

   function Expanded
     (Class : Class_Entity_Record'Class)
      return Boolean;

   function Frozen
     (Class : Class_Entity_Record'Class)
      return Boolean;

   procedure Set_Deferred
     (Class : in out Class_Entity_Record'Class);

   procedure Set_Expanded
     (Class : in out Class_Entity_Record'Class);

   procedure Set_Frozen
     (Class : in out Class_Entity_Record'Class);

   procedure Inherit
     (Class           : in out Class_Entity_Record'Class;
      Inherited_Type : not null access Ack.Types.Type_Entity_Record'Class);

   procedure Rename
     (Class            : in out Class_Entity_Record'Class;
      Inherited_Class  : not null access Class_Entity_Record'Class;
      Feature_Name     : Name_Id;
      New_Feature_Name : Name_Id);

   function Is_Rename
     (Class        : Class_Entity_Record'Class;
      Feature_Name : Name_Id)
      return Boolean;

   function Is_Redefinition
     (Class : Class_Entity_Record'Class;
      Feature_Name : Name_Id)
      return Boolean;

   function Generic_Formal_Count
     (Class : Class_Entity_Record'Class)
      return Natural;

   function Generic_Formal
     (Class : Class_Entity_Record'Class;
      Index : Positive)
      return access constant Ack.Types.Type_Entity_Record'Class;

   overriding procedure Bind
     (Class : in out Class_Entity_Record);

   overriding procedure Allocate
     (Class : Class_Entity_Record;
      Unit  : in out Tagatha.Units.Tagatha_Unit);

   function Has_Feature
     (Class : not null access constant Class_Entity_Record'Class;
      Name  : Name_Id)
      return Boolean;

   function Feature
     (Class : not null access constant Class_Entity_Record'Class;
      Name  : Name_Id)
      return Ack.Features.Feature_Entity
     with Pre => Class.Has_Feature (Name);

   function Has_Aliased_Feature
     (Class : not null access constant Class_Entity_Record'Class;
      Alias : Name_Id)
      return Boolean;

   function Aliased_Feature
     (Class : not null access constant Class_Entity_Record'Class;
      Alias : Name_Id)
      return Ack.Features.Feature_Entity
     with Pre => Class.Has_Aliased_Feature (Alias);

   procedure Scan_Conforming_Child_Ancestors
     (Class : not null access constant Class_Entity_Record'Class;
      Child : not null access constant Class_Entity_Record'Class;
      Process : not null access
        procedure (Ancestor_Class : not null access constant
                     Class_Entity_Record'Class;
                   Call_Name      : String));

   procedure Scan_Ancestors
     (Class            : not null access constant Class_Entity_Record'Class;
      Proper_Ancestors : Boolean;
      Process          : not null access
        procedure (Ancestor : not null access constant
                     Class_Entity_Record'Class));

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

   procedure Add_Generic_Formal
     (Class  : in out Class_Entity_Record'Class;
      Formal : not null access Ack.Types.Type_Entity_Record'Class);

   function New_Class
     (Name        : Name_Id;
      Context     : Class_Entity;
      Declaration : Node_Id)
      return Class_Entity;

   function Has_Class_Entity
     (Node : Node_Id)
      return Boolean;

   function Get_Class_Entity
     (Node : Node_Id)
      return Class_Entity
     with Pre => Kind (Node) in
     N_Class_Declaration | N_Class_Header | N_Class_Name;

   function Get_Ancestor_Type
     (Descendent_Class : Class_Entity_Record'Class;
      Descendent_Type  : not null access constant
        Ack.Types.Type_Entity_Record'Class;
      Ancestor_Class   : not null access constant Class_Entity_Record'Class)
      return access Ack.Types.Type_Entity_Record'Class;

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

   type Inherited_Type_Record is
      record
         Inherited_Type     : access Ack.Types.Type_Entity_Record'Class;
         Redefined_Features : List_Of_Feature_Entities.List;
         Renamed_Features   : List_Of_Feature_Renames.List;
      end record;

   package List_Of_Inherited_Type_Records is
     new Ada.Containers.Doubly_Linked_Lists (Inherited_Type_Record);

   package Notes_Map is
     new WL.String_Maps (String);

   type Class_Entity_Record is
     new Root_Entity_Type with
      record
         Generic_Class           : Boolean := False;
         Deferred                : Boolean := False;
         Expanded                : Boolean := False;
         Frozen                  : Boolean := False;
         Behaviour               : Class_Behaviour := Normal;
         Conforming_Child_Action : Name_Id := No_Name;
         Inherited_Types         : List_Of_Inherited_Type_Records.List;
         Inherited_List          : List_Of_Class_Entities.List;
         Class_Features          : List_Of_Feature_Entities.List;
         Formal_Arguments        : List_Of_Entities.List;
         Notes                   : Notes_Map.Map;
         Creators                : WL.String_Sets.Set;
      end record;

   overriding function Description
     (Class : Class_Entity_Record)
      return String
   is ("class " & Class_Entity_Record'Class (Class).Qualified_Name);

   overriding function Contains
     (Class     : Class_Entity_Record;
      Name      : String;
      Recursive : Boolean := True)
      return Boolean;

   overriding function Get
     (Class : not null access constant Class_Entity_Record;
      Name  : String)
      return Entity_Type;

   overriding function Instantiate
     (Entity             : not null access Class_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type
   is (Entity_Type (Entity));

   overriding function Conforms_To
     (Class : not null access constant Class_Entity_Record;
      Other : not null access constant Root_Entity_Type'Class)
      return Boolean;

   function Has_Feature
     (Class : not null access constant Class_Entity_Record'Class;
      Name  : Name_Id)
      return Boolean
   is (Class.Contains (Name)
       and then Ack.Features.Is_Feature (Class.Get (Name)));

   function Class_Declaration_Context
     (Class : Class_Entity_Record'Class)
      return Class_Entity
   is (Class_Entity (Class.Declaration_Context));

   function Has_Class_Entity
     (Node : Node_Id)
      return Boolean
   is (Has_Entity (Node)
       and then Get_Entity (Node).all in Class_Entity_Record'Class);

   function Find_Feature
     (Class   : Class_Entity_Record'Class;
      Test    : not null access
        function (Feature : not null access constant
                    Ack.Features.Feature_Entity_Record'Class)
      return Boolean)
      return Ack.Features.Feature_Entity;

   function Find_Aliased_Feature
     (Class   : Class_Entity_Record'Class;
      Alias   : Name_Id)
      return Ack.Features.Feature_Entity;

   function Has_Note
     (Class : Class_Entity_Record'Class;
      Name  : String)
      return Boolean
   is (Class.Notes.Contains (Name));

   function Get_Note
     (Class : Class_Entity_Record'Class;
      Name  : String)
      return String
   is (Class.Notes.Element (Name));

   function Behaviour (Class : Class_Entity_Record'Class)
                       return Class_Behaviour
   is (Class.Behaviour);

   function Aqua_Primitive_Behaviour
     (Class : Class_Entity_Record'Class)
      return Boolean
   is (Class.Behaviour = Aqua_Primitive);

   function Deferred
     (Class : Class_Entity_Record'Class)
      return Boolean
   is (Class.Deferred);

   function Expanded
     (Class : Class_Entity_Record'Class)
      return Boolean
   is (Class.Expanded);

   function Frozen
     (Class : Class_Entity_Record'Class)
      return Boolean
   is (Class.Frozen);

end Ack.Classes;
