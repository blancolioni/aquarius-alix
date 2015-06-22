with Komnenos.UI;
with Aquarius.Programs;

package Aquarius.Grammars.EBNF is

   function Create_EBNF_Grammar return Aquarius_Grammar;

   procedure Cross_Reference
     (UI  : Komnenos.UI.Komnenos_UI;
      Top : Aquarius.Programs.Program_Tree);

end Aquarius.Grammars.EBNF;
