package Ack.Primitives is

   procedure Create_Primitives;

   function Any_Class return Entity_Id;
   function None_Class return Entity_Id;

   function Boolean_Class return Entity_Id;
   function Character_Class return Entity_Id;
   function Integer_Class return Entity_Id;
   function String_Class return Entity_Id;

   function Void_Feature return Entity_Id;

end Ack.Primitives;
