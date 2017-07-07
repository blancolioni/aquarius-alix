package body Aquarius.Ack.Classes is

   ---------------
   -- Add_Class --
   ---------------

   procedure Add_Class
     (Context : Entity_Id;
      Name    : Name_Id;
      Class   : Node_Id)
   is
      Entity : constant Entity_Id :=
                 New_Entity (Name, Class_Entity, Context, Class, No_Entity);
   begin
      Entity_Table (Entity).Entity_Type := Entity;
      Node_Table (Class).Entity := Entity;
   end Add_Class;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class
     (Context : Node_Id;
      Name    : Name_Id)
      return Node_Id
   is (No_Node);

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From
     (Ancestor   : Entity_Id;
      Descendent : Entity_Id)
      return Boolean
   is
   begin
      return Ancestor = Descendent;
   end Is_Derived_From;

end Aquarius.Ack.Classes;
