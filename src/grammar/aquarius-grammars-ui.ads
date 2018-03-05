package Aquarius.Grammars.UI is

   procedure Load_Grammar
     (Grammar : Aquarius_Grammar;
      Target  : not null access
        Komnenos.Entities.Entity_Table_Interface'Class);

end Aquarius.Grammars.UI;
