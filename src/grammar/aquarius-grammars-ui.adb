with Ada.Text_IO;

with Komnenos.Entities;

with Aquarius.Programs.Komnenos_Entities;
with Aquarius.Syntax.Komnenos_Entities;

package body Aquarius.Grammars.UI is

   procedure Bind_Grammar_Actions
     (Target  : Komnenos.UI.Komnenos_UI;
      Grammar : Aquarius_Grammar);

   procedure Bind_Action_File
     (Target      : Komnenos.UI.Komnenos_UI;
      Grammar     : Aquarius_Grammar;
      Group       : Aquarius.Actions.Action_Group;
      Action_File : Aquarius.Programs.Program_Tree);

   ----------------------
   -- Bind_Action_File --
   ----------------------

   procedure Bind_Action_File
     (Target      : Komnenos.UI.Komnenos_UI;
      Grammar     : Aquarius_Grammar;
      Group       : Aquarius.Actions.Action_Group;
      Action_File : Aquarius.Programs.Program_Tree)
   is
      Actions : constant Aquarius.Trees.Array_Of_Trees :=
                  Action_File.Children
                    ("compilation_unit"
                     & "/sequence_of_top_level_declarations"
                     & "/top_level_declaration"
                     & "/action_declaration");

      function Get_Action_Key
        (Header : Aquarius.Programs.Program_Tree)
         return String;

      function Get_Action_Name
        (Header : Aquarius.Programs.Program_Tree)
         return String;

      --------------------
      -- Get_Action_Key --
      --------------------

      function Get_Action_Key
        (Header : Aquarius.Programs.Program_Tree)
         return String
      is (Action_Entity_Key
          (Group_Name    => Aquarius.Actions.Action_Group_Name (Group),
           Position_Name =>
              Header.Program_Child ("action_time").Concatenate_Children,
           Parent_Name   =>
              Header.Program_Child ("action_context").Concatenate_Children,
           Child_Name    => ""));

      ---------------------
      -- Get_Action_Name --
      ---------------------

      function Get_Action_Name
        (Header : Aquarius.Programs.Program_Tree)
         return String
      is
         Ids : constant Aquarius.Trees.Array_Of_Trees :=
                 Header.Children ("action_context/identifier");
         Prefix : constant String :=
                    Aquarius.Actions.Action_Group_Name (Group)
                    & ": ";
      begin
         if Ids'Length = 0 then
            raise Constraint_Error with
              "action has no context: " & Header.Concatenate_Children;
         elsif Ids'Length = 1 then
            return Prefix
              & Header.Program_Child ("action_time").Concatenate_Children
              & " " & Ids (Ids'First).Text;
         elsif Ids'Length = 2 then
            return Prefix
              & Ids (Ids'First).Text
              & " "
              & Header.Program_Child ("action_time").Concatenate_Children
              & " " & Ids (Ids'Last).Text;
         else
            raise Constraint_Error with
              "too many contexts: " & Header.Concatenate_Children;
         end if;
      end Get_Action_Name;

   begin
      for Action_Tree of Actions loop
         declare
            use Aquarius.Programs;
            use Aquarius.Programs.Komnenos_Entities;
            Action : constant Program_Tree :=
                       Program_Tree (Action_Tree);
            Header : constant Program_Tree :=
                       Action.Program_Child ("action_header");
            Definition : constant Program_Tree :=
                           Action.Program_Child ("action_definition");
            Name       : constant String :=
                           Get_Action_Name (Header);
            Key        : constant String :=
                           Get_Action_Key (Header);
            Entity     : constant Komnenos.Entities.Entity_Reference :=
                           Create_Aquarius_Source_Entity
                             (Table            => Target,
                              Name             => Name,
                              Qualified_Name   => Name,
                              Class_Name       => "action",
                              Top_Level        => False,
                              Compilation_Unit => Action_File,
                              Defining_Name    => Header,
                              Entity_Spec      => Definition,
                              Entity_Body      => null);
         begin
            Ada.Text_IO.Put_Line ("action: " & Entity.Name
                                  & " [" & Entity.Key & "]");
            Grammar.Action_Entities.Insert
              (Key, Entity);
         end;
      end loop;
   end Bind_Action_File;

   --------------------------
   -- Bind_Grammar_Actions --
   --------------------------

   procedure Bind_Grammar_Actions
     (Target  : Komnenos.UI.Komnenos_UI;
      Grammar : Aquarius_Grammar)
   is

      procedure Bind
        (Group          : Aquarius.Actions.Action_Group;
         Action_Program : Aquarius.Programs.Program_Tree);

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Group          : Aquarius.Actions.Action_Group;
         Action_Program : Aquarius.Programs.Program_Tree)
      is
      begin
         Bind_Action_File
           (Target, Grammar, Group, Action_Program);
      end Bind;

   begin
      Grammar.Scan_Action_Programs (Bind'Access);
   end Bind_Grammar_Actions;

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

      Bind_Grammar_Actions (Target, Grammar);

   end Load_Grammar;

end Aquarius.Grammars.UI;
