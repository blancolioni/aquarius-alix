package body Aquarius.Ack.Primitives is

   Local_String_Class  : Entity_Id := No_Entity;
   Local_Integer_Class : Entity_Id := No_Entity;

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is
   begin
      Local_String_Class :=
        New_Primitive_Class
          (Get_Name_Id ("String"));
      Local_Integer_Class :=
        New_Primitive_Class
          (Get_Name_Id ("Integer"));
   end Create_Primitives;

   -------------------
   -- Integer_Class --
   -------------------

   function Integer_Class return Entity_Id is
   begin
      return Local_Integer_Class;
   end Integer_Class;

   ------------------
   -- String_Class --
   ------------------

   function String_Class return Entity_Id is
   begin
      return Local_String_Class;
   end String_Class;

end Aquarius.Ack.Primitives;