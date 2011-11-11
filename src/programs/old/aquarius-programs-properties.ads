with Aquarius.Entries;
with Aquarius.Grammars;
with Aquarius.Plugins;
with Aquarius.Syntax;
with Aquarius.Trees.Properties;
with Aquarius.Types;

package Aquarius.Programs.Properties is

   package Entries is
      new Aquarius.Trees.Properties ("entry",
                                     Aquarius.Entries.Table_Entry_Record,
                                     Aquarius.Entries.Table_Entry);

   package Grammar is
      new Aquarius.Trees.Properties ("grammar",
                                     Aquarius.Grammars.Aquarius_Grammar_Record,
                                     Aquarius.Grammars.Aquarius_Grammar);

   package Plugin is
      new Aquarius.Trees.Properties ("plugin",
                                     Aquarius.Plugins.Aquarius_Plugin_Type,
                                     Aquarius.Plugins.Aquarius_Plugin);

   package Syntax is
      new Aquarius.Trees.Properties ("syntax",
                                     Aquarius.Syntax.Syntax_Tree_Record,
                                     Aquarius.Syntax.Syntax_Tree);
   package Types is
      new Aquarius.Trees.Properties ("type",
                                     Aquarius.Types.Root_Aquarius_Type,
                                     Aquarius.Types.Aquarius_Type);

end Aquarius.Programs.Properties;

