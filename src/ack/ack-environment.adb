package body Ack.Environment is

   type Top_Level_Entity_Record is
     new Root_Entity_Type with null record;

   overriding function Base_Child_File_Name
     (Entity     : Top_Level_Entity_Record;
      Child_Name : Name_Id)
      return String
   is (To_Standard_String (Child_Name));

   overriding function Instantiate
     (Entity             : not null access Top_Level_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type
   is (Entity_Type (Entity));

   Top_Level_Entity : aliased Top_Level_Entity_Record;
   Initialised      : Boolean := False;

   procedure Create_Top_Level;

   ----------------------
   -- Create_Top_Level --
   ----------------------

   procedure Create_Top_Level is
   begin
      Top_Level_Entity.Create
        (Name               => Get_Name_Id ("standard"),
         Node               => No_Node,
         Table              => True,
         Parent_Environment => null,
         Context            => null);
      Initialised := True;
   end Create_Top_Level;

   ---------------
   -- Top_Level --
   ---------------

   function Top_Level return Entity_Type is
   begin
      if not Initialised then
         Create_Top_Level;
      end if;
      return Top_Level_Entity'Access;
   end Top_Level;

end Ack.Environment;
