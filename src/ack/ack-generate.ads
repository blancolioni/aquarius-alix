package Ack.Generate is

   procedure Generate_Class_Declaration
     (Node : Node_Id);

   procedure Generate_Compound
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Node    : Node_Id);

end Ack.Generate;
