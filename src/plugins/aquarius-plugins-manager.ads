with Aquarius.Grammars;

package Aquarius.Plugins.Manager is

   procedure Load (From_Grammar : Aquarius.Grammars.Aquarius_Grammar);

   function Get_Plugin (Name : String) return Aquarius_Plugin;

end Aquarius.Plugins.Manager;
