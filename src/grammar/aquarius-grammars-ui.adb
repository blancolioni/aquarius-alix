with Komnenos.Entities;

with Aquarius.Programs.Komnenos_Entities;

package body Aquarius.Grammars.UI is

   ------------------
   -- Load_Grammar --
   ------------------

   procedure Load_Grammar
     (Grammar : Aquarius_Grammar;
      Target  : Komnenos.UI.Komnenos_UI)
   is

      use type Aquarius.Programs.Program_Tree;
      use type Komnenos.Entities.Entity_Reference;

      File_Name : constant String :=
                    Grammar.Definition.Source_File_Name;

      procedure Load (Program : Aquarius.Programs.Program_Tree)
        with Pre => Program /= null;

      procedure Cross_Reference
        (Program : Aquarius.Programs.Program_Tree)
        with Pre => Program /= null;

      procedure Cross_Rule
        (Entity  : Komnenos.Entities.Entity_Reference;
         Program : Aquarius.Programs.Program_Tree)
        with Pre => Entity /= null and then Program /= null;

      ---------------------
      -- Cross_Reference --
      ---------------------

      procedure Cross_Reference (Program : Aquarius.Programs.Program_Tree) is
      begin
         if Program.Text = "rule-definition" then
            Cross_Rule
              (Entity  =>
                 Target.Find
                   (Program.Program_Child ("identifier").Text, ""),
               Program =>
                 Program.Program_Child ("definition-body"));
         else
            for I in 1 .. Program.Child_Count loop
               Cross_Reference (Program.Program_Child (I));
            end loop;
         end if;
      end Cross_Reference;

      ----------------
      -- Cross_Rule --
      ----------------

      procedure Cross_Rule
        (Entity  : Komnenos.Entities.Entity_Reference;
         Program : Aquarius.Programs.Program_Tree)
      is
      begin
         if Program.Name = "identifier" then
            declare
               Name      : constant String := Program.Text;
               Reference : constant Komnenos.Entities.Entity_Reference :=
                             Target.Find (Name, "");
            begin
               if Reference /= null then
                  Target.Add_Cross_Reference
                    (Item      => Reference,
                     Referrer  => Entity,
                     File_Name => File_Name,
                     Line      => Program.Source_Line,
                     Column    => Program.Source_Column,
                     Ref_Type  => "reference");
               end if;
            end;
         else
            for I in 1 .. Program.Child_Count loop
               Cross_Rule (Entity, Program.Program_Child (I));
            end loop;
         end if;

      end Cross_Rule;

      ----------
      -- Load --
      ----------

      procedure Load (Program : Aquarius.Programs.Program_Tree) is
      begin
         if Program.Text = "rule-definition" then
            declare
               use Aquarius.Programs.Komnenos_Entities;
               Name_Node : constant Aquarius.Programs.Program_Tree :=
                             Program.Program_Child ("identifier");
               Rule_Name : constant String :=
                             Name_Node.Text;
               Entity    : constant Komnenos.Entities.Entity_Reference :=
                             Create_Aquarius_Source_Entity
                               (Table            => Target,
                                Name             => Rule_Name,
                                File_Name        => File_Name,
                                Class            => Program.Text,
                                Top_Level        => True,
                                Compilation_Unit => Grammar.Definition,
                                Defining_Name    => Name_Node,
                                Entity_Spec      => Program,
                                Entity_Body      => null);
            begin
               pragma Unreferenced (Entity);
            end;
         else
            for I in 1 .. Program.Child_Count loop
               Load (Program.Program_Child (I));
            end loop;
         end if;

      end Load;

   begin
      Load (Grammar.Definition);
      Cross_Reference (Grammar.Definition);
   end Load_Grammar;

end Aquarius.Grammars.UI;
