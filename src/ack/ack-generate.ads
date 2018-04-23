package Ack.Generate is

   procedure Generate_Class_Declaration
     (Node : Node_Id;
      Root : Boolean);

   procedure Generate_Compound
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Context : not null access constant Root_Entity_Type'Class;
      Node    : Node_Id);

   procedure Generate_Expression
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Context    : not null access constant Root_Entity_Type'Class;
      Expression : Node_Id);

end Ack.Generate;
