with Aquarius.Entries;
with Aquarius.Source;
with Aquarius.Tagatha_Object;
with Aquarius.Trees.Properties;

with Tagatha.Units;

package body Ada_Plugin.Ch10.Gen is

   ----------------------------
   -- Compilation_Unit_After --
   ----------------------------

   procedure Compilation_Unit_After
     (Tree : Program_Tree)
   is
      Unit : constant Tagatha.Units.Tagatha_Unit_Access :=
        Aquarius.Tagatha_Object.Get_Unit
        (Aquarius.Trees.Properties.Get_Tagatha (Tree.all));
   begin

      Unit.Finish_Unit;
      Unit.Optimise;
      Unit.Write ("6502", ".");

   end Compilation_Unit_After;

   -----------------------------
   -- Compilation_Unit_Before --
   -----------------------------

   procedure Compilation_Unit_Before
     (Tree : Program_Tree)
   is
      Top_Entry : constant Aquarius.Entries.Table_Entry :=
        Tree.Get_Entry;
      Unit : constant Tagatha.Units.Tagatha_Unit_Access :=
        new Tagatha.Units.Tagatha_Unit;
      Source_Name : constant String :=
        Aquarius.Source.Get_File_Name
        (Aquarius.Source.Get_Source_File (Tree.Get_Location));
   begin
      Unit.Create_Unit (Top_Entry.Name, Source_Name);
      Aquarius.Trees.Properties.Set_Tagatha
        (Tree.all,
         Aquarius.Tagatha_Object.Create_Tagatha_Object (Unit));

   end Compilation_Unit_Before;

end Ada_Plugin.Ch10.Gen;
