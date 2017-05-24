with Aquarius.Syntax.Komnenos_Entities;

package body Aquarius.Grammars.UI is

   ------------------
   -- Load_Grammar --
   ------------------

   procedure Load_Grammar
     (Grammar : Aquarius_Grammar;
      Target  : Komnenos.UI.Komnenos_UI)
   is
   begin
      for Non_Terminal of Grammar.Non_Terminals loop
         Aquarius.Syntax.Komnenos_Entities.Create_Aquarius_Syntax_Entity
           (Target, Grammar.Name, Non_Terminal);
      end loop;
   end Load_Grammar;

end Aquarius.Grammars.UI;
