package Aquarius.Ack.Classes is

   procedure Add_Class
     (Context : Entity_Id;
      Name    : Name_Id;
      Class   : Node_Id);

   function Get_Class
     (Context : Node_Id;
      Name    : Name_Id)
      return Node_Id;

   function Is_Derived_From
     (Ancestor   : Entity_Id;
      Descendent : Entity_Id)
      return Boolean
     with Pre => Ancestor /= No_Entity and then Descendent /= No_Entity;
   --  Return True if Ancestor = Descendent, or else
   --  for some parent class P of Descendent,
   --  Id_Derived_From (Ancestor, P)

end Aquarius.Ack.Classes;
