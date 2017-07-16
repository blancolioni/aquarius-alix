package body Aquarius.Ack.Primitives is

   Primitives_Created    : Boolean := False;

   Local_Any_Class       : Entity_Id := No_Entity;
   Local_Boolean_Class   : Entity_Id := No_Entity;
   Local_Character_Class : Entity_Id := No_Entity;
   Local_Integer_Class   : Entity_Id := No_Entity;
   Local_None_Class      : Entity_Id := No_Entity;
   Local_String_Class    : Entity_Id := No_Entity;

   Local_Void_Feature    : Entity_Id := No_Entity;

   ---------------
   -- Any_Class --
   ---------------

   function Any_Class return Entity_Id is
   begin
      return Local_Any_Class;
   end Any_Class;

   -------------------
   -- Boolean_Class --
   -------------------

   function Boolean_Class return Entity_Id is
   begin
      return Local_Boolean_Class;
   end Boolean_Class;

   ---------------------
   -- Character_Class --
   ---------------------

   function Character_Class return Entity_Id is
   begin
      return Local_Character_Class;
   end Character_Class;

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is
   begin
      if not Primitives_Created then
         Local_Any_Class :=
           New_Primitive_Class
             (Get_Name_Id ("Any"));
         Local_Boolean_Class :=
           New_Primitive_Class
             (Get_Name_Id ("Boolean"));
         Local_Character_Class :=
           New_Primitive_Class
             (Get_Name_Id ("Character"));
         Local_Integer_Class :=
           New_Primitive_Class
             (Get_Name_Id ("Integer"));
         Local_String_Class :=
           New_Primitive_Class
             (Get_Name_Id ("String"));
         Local_None_Class :=
           New_Primitive_Class
             (Get_Name_Id ("None"));

         Local_Void_Feature :=
           New_Entity
             (Name        => Get_Name_Id ("Void"),
              Kind        => Property_Feature_Entity,
              Context     => No_Entity,
              Declaration => No_Node,
              Entity_Type => Local_None_Class);

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

   ----------------
   -- None_Class --
   ----------------

   function None_Class return Entity_Id is
   begin
      return Local_None_Class;
   end None_Class;

   ------------------
   -- String_Class --
   ------------------

   function String_Class return Entity_Id is
   begin
      return Local_String_Class;
   end String_Class;

   ------------------
   -- Void_Feature --
   ------------------

   function Void_Feature return Entity_Id is
   begin
      return Local_Void_Feature;
   end Void_Feature;

end Aquarius.Ack.Primitives;
