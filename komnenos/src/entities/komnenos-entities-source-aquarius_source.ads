with Aquarius.Programs;

package Komnenos.Entities.Source.Aquarius_Source is

   function Create_Aquarius_Source_Entity
     (Table            : not null access Entity_Table_Interface'Class;
      Name             : String;
      File_Name        : String;
      Class            : String;
      Line             : Natural;
      Column           : Natural;
      Top_Level        : Boolean;
      Compilation_Unit : Aquarius.Programs.Program_Tree;
      Entity_Spec      : Aquarius.Programs.Program_Tree;
      Entity_Body      : Aquarius.Programs.Program_Tree)
      return Entity_Reference;

   procedure Set_Entity_Body
     (Entity : Entity_Reference;
      Entity_Body : Aquarius.Programs.Program_Tree);

   function Find_Entity_Containing
     (Table     : not null access Entity_Table_Interface'Class;
      Location  : File_Location)
      return Entity_Reference;

end Komnenos.Entities.Source.Aquarius_Source;
