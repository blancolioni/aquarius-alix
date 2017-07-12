package body Aquarius.Ack.Primitives is

   Primitives_Created : Boolean := False;
   Local_Boolean_Class : Entity_Id := No_Entity;
   Local_Integer_Class : Entity_Id := No_Entity;
   Local_String_Class  : Entity_Id := No_Entity;

   -------------------
   -- Boolean_Class --
   -------------------

   function Boolean_Class return Entity_Id is
   begin
      return Local_Boolean_Class;
   end Boolean_Class;

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is
   begin
      if not Primitives_Created then
         Local_Boolean_Class :=
           New_Primitive_Class
             (Get_Name_Id ("Boolean"));
         Local_Integer_Class :=
           New_Primitive_Class
             (Get_Name_Id ("Integer"));
         Local_String_Class :=
           New_Primitive_Class
             (Get_Name_Id ("String"));
         Primitives_Created := True;
      end if;
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
