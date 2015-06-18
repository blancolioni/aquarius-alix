package Komnenos.Entities.Source is

   type Root_Source_Entity_Reference is
     new Root_Entity_Reference with private;

   type Source_Entity_Reference is
     access all Root_Source_Entity_Reference'Class;

   procedure Create
     (Item         : in out Root_Source_Entity_Reference'Class;
      Name         : String;
      File_Name    : String;
      Class        : String;
      Line         : Natural;
      Column       : Natural);

   function New_Source_Entity
     (Name         : String;
      File_Name    : String;
      Class        : String;
      Line         : Natural;
      Column       : Natural)
      return Entity_Reference;

private

   type Root_Source_Entity_Reference is
     new Root_Entity_Reference with
      record
         Name      : Ada.Strings.Unbounded.Unbounded_String;
         File_Name : Ada.Strings.Unbounded.Unbounded_String;
         Line      : Natural;
         Column    : Natural;
      end record;

   overriding procedure Select_Entity
     (Entity : Root_Source_Entity_Reference;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Offset : Natural);

end Komnenos.Entities.Source;
