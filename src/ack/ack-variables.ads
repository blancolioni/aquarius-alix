package Ack.Variables is

   type Variable_Entity_Record is
     new Root_Entity_Type with private;

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

private

   type Variable_Entity_Record is
     new Root_Entity_Type with
      record
         null;
      end record;

end Ack.Variables;
