package body Ack.Variables is

   --------------------------
   -- Instantiate_Argument --
   --------------------------

   function Instantiate_Argument
     (Generic_Argument : Variable_Entity;
      Instantiate_Type : not null access
        function (Generic_Type : not null access
                    Root_Entity_Type'Class)
      return Entity_Type)
      return Variable_Entity
   is
   begin
      return new Variable_Entity_Record'
        (Name                => Generic_Argument.Name,
         Source_Name         => Generic_Argument.Source_Name,
         Declaration_Node    => Generic_Argument.Declaration_Node,
         Declaration_Context => Generic_Argument.Declaration_Context,
         Value_Type          => Instantiate_Type (Generic_Argument.Value_Type),
         Child_Map           => <>,
         Child_List          => <>,
         Parent_Environment  => Generic_Argument.Parent_Environment);
   end Instantiate_Argument;

   -------------------------
   -- New_Argument_Entity --
   -------------------------

   function New_Argument_Entity
     (Name          : Name_Id;
      Node          : Node_Id;
      Argument_Type : not null access Root_Entity_Type'Class)
      return Variable_Entity
   is
   begin
      return Result : constant Variable_Entity :=
        new Variable_Entity_Record
      do
         Result.Create (Name, Node);
         Result.Value_Type := Entity_Type (Argument_Type);
      end return;
   end New_Argument_Entity;

   ----------------------
   -- New_Local_Entity --
   ----------------------

   function New_Local_Entity
     (Name       : Name_Id;
      Node       : Node_Id;
      Local_Type : not null access Root_Entity_Type'Class)
      return Variable_Entity
   is
   begin
      return Result : constant Variable_Entity :=
        new Variable_Entity_Record
      do
         Result.Create (Name, Node);
         Result.Value_Type := Entity_Type (Local_Type);
      end return;
   end New_Local_Entity;

end Ack.Variables;
