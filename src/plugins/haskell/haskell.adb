with Aquarius.Actions;

package body Haskell is

   Global_Haskell_Plugin : Haskell_Plugin_Access;

   ----------
   -- Load --
   ----------

   overriding
   procedure Load (Plugin  : not null access Haskell_Plugin_Type;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar)
   is
      Parser : Aquarius.Actions.Action_Group;
   begin
      Aquarius.Plugins.Load
        (Aquarius.Plugins.Aquarius_Plugin_Type (Plugin.all)'Access, Grammar);

      Grammar.Add_Action_Group ("parser",
                                Aquarius.Actions.Parse_Trigger,
                                Parser);
      Global_Haskell_Plugin := Haskell_Plugin_Access (Plugin);
   end Load;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Plugin : Haskell_Plugin_Type)
                 return String
   is
      pragma Unreferenced (Plugin);
   begin
      return "Haskell";
   end Name;

   ------------
   -- Plugin --
   ------------

   function Plugin return Haskell_Plugin_Access is
   begin
      return Global_Haskell_Plugin;
   end Plugin;

   -------------
   -- Version --
   -------------

   overriding
   function Version (Plugin : Haskell_Plugin_Type)
                    return String
   is
      pragma Unreferenced (Plugin);
   begin
      return "0.1";
   end Version;

end Haskell;
