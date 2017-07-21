package body Ack.Environment is

   type Top_Level_Entity_Record is
     new Root_Entity_Type with null record;

   overriding function Base_Child_File_Name
     (Entity     : Top_Level_Entity_Record;
      Child_Name : Name_Id)
      return String
   is (To_Standard_String (Child_Name));

   Top_Level_Entity : aliased Top_Level_Entity_Record;
   Initialised      : Boolean := False;

   procedure Create_Top_Level;

   ----------------------
   -- Create_Top_Level --
   ----------------------

   procedure Create_Top_Level is
   begin
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
