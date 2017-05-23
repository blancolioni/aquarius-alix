with Komnenos.Entities;

package Aquarius.Programs.Komnenos_Entities is

   function Create_Aquarius_Source_Entity
     (Table            : not null access
        Komnenos.Entities.Entity_Table_Interface'Class;
      Name             : String;
      File_Name        : String;
      Class            : String;
      Top_Level        : Boolean;
      Compilation_Unit : not null access Program_Tree_Type'Class;
      Defining_Name    : not null access Program_Tree_Type'Class;
      Entity_Spec      : not null access Program_Tree_Type'Class;
      Entity_Body      : access Program_Tree_Type'Class)
      return Komnenos.Entities.Entity_Reference;

   procedure Set_Entity_Body
     (Entity : Komnenos.Entities.Entity_Reference;
      Entity_Body : not null access Program_Tree_Type'Class);

   function Syntax_Entity
     (Table  : not null access Komnenos.Entities.Entity_Table_Interface'Class;
      Entity : Komnenos.Entities.Entity_Reference)
      return Komnenos.Entities.Entity_Reference;

--     function Find_Entity_Containing
--       (Table     : not null access Entity_Table_Interface'Class;
--        Location  : File_Location)
--        return Entity_Reference;

end Aquarius.Programs.Komnenos_Entities;
