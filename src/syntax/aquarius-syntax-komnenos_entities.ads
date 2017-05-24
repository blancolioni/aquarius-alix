with Komnenos.Entities;

package Aquarius.Syntax.Komnenos_Entities is

   procedure Create_Aquarius_Syntax_Entity
     (Table        : not null access
        Komnenos.Entities.Entity_Table_Interface'Class;
      Grammar_Name : String;
      Tree         : Syntax_Tree);

end Aquarius.Syntax.Komnenos_Entities;
