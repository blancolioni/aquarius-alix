with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Komnenos.Entities;

with Aquarius.Config_Paths;
with Aquarius.Loader;

with Aquarius.Programs.Komnenos_Entities;
with Aquarius.Syntax.Komnenos_Entities;

package body Aquarius.Grammars.UI is

   function Grammar_Path
     (Grammar : Aquarius_Grammar;
      Name    : String)
      return String
   is (Aquarius.Config_Paths.Config_File
       ("grammar/" & Grammar.Name & "/"
        & (if Name (Name'First) = '.'
           then Grammar.Name & Name
           else Name)));

   procedure Bind_Plugin_Actions
     (Target  : Komnenos.UI.Komnenos_UI;
      Grammar : Aquarius_Grammar;
      Plugin  : Aquarius.Programs.Program_Tree);

   procedure Bind_Action_File
     (Target      : Komnenos.UI.Komnenos_UI;
      Grammar     : Aquarius_Grammar;
      Group_Name  : String;
      Action_File : Aquarius.Programs.Program_Tree);

   ----------------------
   -- Bind_Action_File --
   ----------------------

   procedure Bind_Action_File
     (Target      : Komnenos.UI.Komnenos_UI;
      Grammar     : Aquarius_Grammar;
      Group_Name  : String;
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
          (Group_Name    => Group_Name,
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
      begin
         if Ids'Length = 0 then
            raise Constraint_Error with
              "action has no context: " & Header.Concatenate_Children;
         elsif Ids'Length = 1 then
            return Header.Program_Child ("action_time").Concatenate_Children
              & " " & Ids (Ids'First).Text;
         elsif Ids'Length = 2 then
            return Ids (Ids'First).Text
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
                              File_Name        => Header.Source_File_Name,
                              Top_Level        => False,
                              Compilation_Unit => Action_File,
                              Defining_Name    => Header,
                              Entity_Spec      => Definition,
                              Entity_Body      => null);
         begin
            Ada.Text_IO.Put_Line ("action: " & Entity.Name
                                  & " [" & Key & "]");
            Grammar.Action_Entities.Insert
              (Key, Entity);
         end;
      end loop;
   end Bind_Action_File;

   -------------------------
   -- Bind_Plugin_Actions --
   -------------------------

   procedure Bind_Plugin_Actions
     (Target  : Komnenos.UI.Komnenos_UI;
      Grammar : Aquarius_Grammar;
      Plugin  : Aquarius.Programs.Program_Tree)
   is

      procedure Bind (Group : Aquarius.Programs.Program_Tree);

      ----------
      -- Bind --
      ----------

      procedure Bind (Group : Aquarius.Programs.Program_Tree) is
      begin
         for Tree of
           Group.Children ("list_of_actions/action_file_reference")
         loop
            declare
               use Ada.Strings.Unbounded;
               Name : Unbounded_String := Null_Unbounded_String;
            begin
               for Id of Tree.Children ("identifier") loop
                  if Name /= Null_Unbounded_String then
                     Name := Name & "-" & Id.Text;
                  else
                     Name := To_Unbounded_String (Id.Text);
                  end if;
               end loop;

               Bind_Action_File
                 (Target, Grammar,
                  Group.Program_Child ("identifier").Text,
                  Aquarius.Loader.Load_From_File
                    (Grammar_Path (Grammar, To_String (Name) & ".action")));

            end;
         end loop;
      end Bind;

   begin
      for Tree of
        Plugin.Children
          ("source_file/plugin_declaration/list_of_declarations/declaration/"
           & "group_declaration")
      loop
         Ada.Text_IO.Put_Line ("binding: " & Tree.Image);
         Bind (Aquarius.Programs.Program_Tree (Tree));
      end loop;

   end Bind_Plugin_Actions;

   ------------------
   -- Load_Grammar --
   ------------------

   procedure Load_Grammar
     (Grammar : Aquarius_Grammar;
      Target  : Komnenos.UI.Komnenos_UI)
   is

      procedure Create_Action_Entities
        (Group : Aquarius.Actions.Action_Group);

      ----------------------------
      -- Create_Action_Entities --
      ----------------------------

      procedure Create_Action_Entities
        (Group : Aquarius.Actions.Action_Group)
      is
      begin
         Ada.Text_IO.Put_Line
           ("action group: " & Aquarius.Actions.Action_Group_Name (Group));
      end Create_Action_Entities;

   begin
      for Non_Terminal of Grammar.Non_Terminals loop
         Aquarius.Syntax.Komnenos_Entities.Create_Aquarius_Syntax_Entity
           (Target, Grammar.Name, Non_Terminal);
      end loop;

      for Trigger in Aquarius.Actions.Action_Execution_Trigger loop
         Aquarius.Actions.Iterate
           (List    => Grammar.Action_Groups,
            Trigger => Trigger,
            Process => Create_Action_Entities'Access);
      end loop;

      declare
         use type Aquarius.Programs.Program_Tree;
         Plugin_Path : constant String :=
                         Grammar_Path (Grammar, ".plugin");
         Plugin      : constant Aquarius.Programs.Program_Tree :=
                         (if Ada.Directories.Exists (Plugin_Path)
                          then Aquarius.Loader.Load_From_File (Plugin_Path)
                          else null);
      begin
         if Plugin = null then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "warning: " & Grammar.Name & " has no plugin");
         else
            Bind_Plugin_Actions (Target, Grammar, Plugin);
         end if;
      end;

   end Load_Grammar;

end Aquarius.Grammars.UI;
