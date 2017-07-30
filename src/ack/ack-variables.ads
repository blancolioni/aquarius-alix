package Ack.Variables is

   type Variable_Entity_Record is
     new Root_Entity_Type with private;

   procedure Set_Offset
     (Variable : in out Variable_Entity_Record'Class;
      Offset   : Positive);

   type Variable_Entity is access all Variable_Entity_Record'Class;

   function New_Argument_Entity
     (Name          : Name_Id;
      Node          : Node_Id;
      Argument_Type : not null access Root_Entity_Type'Class)
      return Variable_Entity;

   function New_Local_Entity
     (Name       : Name_Id;
      Node       : Node_Id;
      Local_Type : not null access Root_Entity_Type'Class)
      return Variable_Entity;

   function New_Iterator_Entity
     (Name       : Name_Id;
      Node       : Node_Id;
      Local_Type : not null access Root_Entity_Type'Class)
      return Variable_Entity;

   function Is_Variable
     (Entity : not null access Root_Entity_Type'Class)
      return Boolean;

private

   type Variable_Kind is (Argument, Local);

   type Variable_Entity_Record is
     new Root_Entity_Type with
      record
         Kind     : Variable_Kind;
         Offset   : Positive;
         Iterator : Boolean := False;
      end record;

   overriding function Instantiate
     (Entity             : not null access Variable_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type;

   overriding procedure Push_Entity
     (Variable     : Variable_Entity_Record;
      Have_Context : Boolean;
      Unit         : in out Tagatha.Units.Tagatha_Unit)
     with Pre => not Have_Context;

   overriding procedure Pop_Entity
     (Variable : Variable_Entity_Record;
      Unit     : in out Tagatha.Units.Tagatha_Unit);

   function Is_Variable
     (Entity : not null access Root_Entity_Type'Class)
      return Boolean
   is (Entity.all in Variable_Entity_Record'Class);

end Ack.Variables;
