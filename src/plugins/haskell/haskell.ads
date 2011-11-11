with Aquarius.Grammars;
with Aquarius.Plugins;

package Haskell is

   type Haskell_Plugin_Type is
     new Aquarius.Plugins.Aquarius_Plugin_Type
     with private;

   overriding
   function Name (Plugin : Haskell_Plugin_Type) return String;

   overriding
   function Version (Plugin : Haskell_Plugin_Type)
                    return String;

   overriding
   procedure Load (Plugin  : not null access Haskell_Plugin_Type;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar);

private

   type Haskell_Plugin_Type is
     new Aquarius.Plugins.Aquarius_Plugin_Type
     with null record;

   type Haskell_Plugin_Access is access all Haskell_Plugin_Type'Class;

   function Plugin return Haskell_Plugin_Access;

end Haskell;
