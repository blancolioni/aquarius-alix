with Aquarius.Programs;

package Aquarius.Grammars.EBNF is

   function Create_EBNF_Grammar return Aquarius_Grammar;

   procedure Cross_Reference
     (Table : not null access
        Komnenos.Entities.Entity_Table_Interface'Class;
      Top : Aquarius.Programs.Program_Tree);

end Aquarius.Grammars.EBNF;
