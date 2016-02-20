with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aquarius.Grammars.Manager;
with Aquarius.Names;
with Aquarius.Programs.Arrangements;
with Aquarius.Rendering.Manager;
with Aquarius.Source;
with Aquarius.Themes;
with Aquarius.Tokens;
with Aquarius.Trees.Cursors;

package body Aquarius.Plugins.Plugin is

   Global_Plugin_Plugin : Plugin_Plugin_Access;

   function Ada_Case (S : String) return String;

   function Make_Tree (Top_Node : String;
                       Source_Text : String)
                      return Aquarius.Programs.Program_Tree;

   function Make_Tree (Top_Node    : String;
                       Before_Text : String;
                       Child       : Aquarius.Programs.Program_Tree;
                       After_Text  : String)
                      return Aquarius.Programs.Program_Tree;

   function Repeat_Child
     (Top_Node    : String;
      Property    : Aquarius.Properties.Property_Type;
      Child       : Aquarius.Programs.Array_Of_Program_Trees)
     return Aquarius.Programs.Program_Tree;

   procedure Repeat_Child
     (Result      : Aquarius.Programs.Program_Tree;
      Property    : Aquarius.Properties.Property_Type;
      Child       : Aquarius.Programs.Array_Of_Program_Trees);

   function Make_Internal_Package_Name
     (Sub_Names : Aquarius.Programs.Array_Of_Program_Trees)
     return String;

   function Make_Qualified_Name
     (Sub_Names : Aquarius.Programs.Array_Of_Program_Trees)
     return String;

   procedure Action_Handler_After
     (Handler_Actionable : not null access Actions.Actionable'Class);

   procedure Declaration_After
     (Decl_Actionable : not null access Actions.Actionable'Class);

   procedure Package_Declaration_After
     (Package_Decl_Actionable : not null access Actions.Actionable'Class);

   procedure Package_After_Qualified_Identifier
     (Package_Actionable    : not null access Actions.Actionable'Class;
      Identifier_Actionable : not null access Actions.Actionable'Class);

   procedure Package_After_Identifier
     (Package_Actionable    : not null access Actions.Actionable'Class;
      Identifier_Actionable : not null access Actions.Actionable'Class);

   procedure Project_After_Name
     (Project_Actionable  : not null access Actions.Actionable'Class;
      Name_Actionable     : not null access Actions.Actionable'Class);

   procedure Project_After
     (Project_Actionable    : not null access Actions.Actionable'Class);

   procedure Action_Group_After
     (Group_Actionable      : not null access Actions.Actionable'Class);

   function Create_Spec (Plugin_Name   : String;
                         Project_Name  : String;
                         Package_Name  : String)
                        return Aquarius.Programs.Program_Tree;

   function Create_Body (Plugin_Name   : String;
                         Project_Name  : String;
                         Package_Name  : String;
                         Project       : Aquarius.Programs.Program_Tree)
                        return Aquarius.Programs.Program_Tree;

   function Create_Binder (Plugin_Name   : String;
                           Project       : Aquarius.Programs.Program_Tree)
                          return Aquarius.Programs.Program_Tree;

   procedure Write_Package (P    : Aquarius.Programs.Program_Tree;
                            Path : String);

   function To_Package_Withs (Parent : String;
                              Props  : Aquarius.Trees.Array_Of_Objects)
                             return String;

   ------------------------
   -- Action_Group_After --
   ------------------------

   procedure Action_Group_After
     (Group_Actionable      : not null access Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      P   : constant Program_Tree := Program_Tree (Group_Actionable);
      Group_Name    : constant String :=
        Program_Tree (P.First_Leaf).Text;
      Group_Trigger : constant String :=
        Ada.Characters.Handling.To_Lower (Program_Tree (P.Last_Leaf).Text);
   begin
      P.Set_Property (Plugin.Property_Action_Group,
                      Aquarius.Names.Name_Value (Group_Name));
      P.Set_Property (Plugin.Property_Action_Trigger,
                      Aquarius.Names.Name_Value (Group_Trigger));
   end Action_Group_After;

   --------------------------
   -- Action_Handler_After --
   --------------------------

   procedure Action_Handler_After
     (Handler_Actionable : not null access Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      P : constant Program_Tree := Program_Tree (Handler_Actionable);
      Target     : constant String := P.Program_Child (1).First_Leaf.Text;
      Pos        : constant String := P.Program_Child (2).First_Leaf.Text;
      Child      : constant String := P.Program_Child (3).Last_Leaf.Text;
      Have_Child : constant Boolean := Child /= "";
      As_Name    : constant String  := P.Program_Child (4).Last_Leaf.Text;
      Have_As    : constant Boolean := P.Program_Child (4).Has_Children;
      Group      : constant String  :=
        P.Property (Plugin.Property_Package_Group).Name;
      Result     : Program_Tree;
      Package_Name : constant String :=
        P.Property (Plugin.Property_Internal_Package_Name).Name;

      function Handler_Name return String;
      function Handler_Ada_Name return String;

      function Handler_Spec return String;
      function Handler_Body return String;

      function Binder_Statement return String;

      ----------------------
      -- Binder_Statement --
      ----------------------

      function Binder_Statement return String is
         use Ada.Characters.Handling;
      begin
         if Have_Child then
            return "Plugin.Register_Action (""" &
              To_Lower (Target) & """, """ &
              To_Lower (Child) & """, " & Ada_Case (Group) &
              ", Aquarius." & Ada_Case (Pos) & ", " &
              Package_Name & "." &
              "Actionable_" & Handler_Name & "'Access);";
         else
            return "Plugin.Register_Action (""" &
              To_Lower (Target) & """, " & Ada_Case (Group) &
              ", Aquarius." & Ada_Case (Pos) & ", " &
              Package_Name & "." &
              "Actionable_" & Handler_Name & "'Access);";
         end if;
      end Binder_Statement;

      ----------------------
      -- Handler_Ada_Name --
      ----------------------

      function Handler_Ada_Name return String is
      begin
         if Have_As then
            return As_Name;
         else
            return Handler_Name;
         end if;
      end Handler_Ada_Name;

      ------------------
      -- Handler_Body --
      ------------------

      function Handler_Body return String is
      begin
         if Have_Child then
            return " is begin " &
              P.Property (Plugin.Property_Project_Name).Name & "." &
              P.Property (Plugin.Property_Package_Name).Name & "." &
              Handler_Ada_Name &
              " (Aquarius.Programs.Program_Tree (Target_Actionable)," &
              " Aquarius.Programs.Program_Tree (Child_Actionable));" &
              "end Actionable_" & Handler_Name;
         else
            return " is begin " &
              P.Property (Plugin.Property_Project_Name).Name & "." &
              P.Property (Plugin.Property_Package_Name).Name & "." &
              Handler_Ada_Name &
              " (Aquarius.Programs.Program_Tree (Target_Actionable));" &
              "end Actionable_" & Handler_Name;
         end if;
      end Handler_Body;

      ------------------
      -- Handler_Name --
      ------------------

      function Handler_Name return String is
      begin
         if Have_Child then
            return Target & "_" & Ada_Case (Pos) & "_" & Child;
         else
            return Target & "_" & Ada_Case (Pos);
         end if;
      end Handler_Name;

      ------------------
      -- Handler_Spec --
      ------------------

      function Handler_Spec return String is
      begin
         if Have_Child then
            return "procedure Actionable_" & Handler_Name &
              " (Target_Actionable : " &
              "not null access Aquarius.Actions.Actionable'Class;" &
              "Child_Actionable : " &
              "not null access Aquarius.Actions.Actionable'Class)";
         else
            return "procedure Actionable_" & Handler_Name &
              " (Target_Actionable : " &
              "not null access Aquarius.Actions.Actionable'Class)";
         end if;
      end Handler_Spec;

   begin
      Result := Make_Tree ("procedure_declaration", Handler_Spec & ";");
      P.Set_Property (Plugin.Property_Ada_Spec_Tree, Result);

      Result := Make_Tree ("procedure_declaration",
                           Handler_Spec & Handler_Body);
      P.Set_Property (Plugin.Property_Ada_Body_Tree, Result);

      Result := Make_Tree ("statement", Binder_Statement);
      P.Set_Property (Plugin.Property_Action_Binder, Result);
   end Action_Handler_After;

   --------------
   -- Ada_Case --
   --------------

   function Ada_Case (S : String) return String is
      use Ada.Characters.Handling;
      Capitalise : Boolean := True;
      Result     : String  := S;
   begin
      for I in Result'Range loop
         if Capitalise then
            if Is_Lower (Result (I)) then
               Result (I) := To_Upper (Result (I));
            end if;
            Capitalise := False;
         end if;
         if Result (I) = '_' then
            Capitalise := True;
         end if;
      end loop;
      return Result;
   end Ada_Case;

   -------------------
   -- Create_Binder --
   -------------------

   function Create_Binder (Plugin_Name   : String;
                           Project       : Aquarius.Programs.Program_Tree)
                          return Aquarius.Programs.Program_Tree
   is

      use Aquarius.Programs;

      Action_Groups   : constant Aquarius.Trees.Array_Of_Objects :=
        Project.All_Properties (Plugin.Property_Action_Group);
      Action_Triggers : constant Aquarius.Trees.Array_Of_Objects :=
        Project.All_Properties (Plugin.Property_Action_Trigger);
      Action_Binders  : constant Aquarius.Trees.Array_Of_Objects :=
        Project.All_Properties (Plugin.Property_Action_Binder);

      function Action_Decls return Array_Of_Program_Trees;
      function Action_Inits return Array_Of_Program_Trees;

      ------------------
      -- Action_Decls --
      ------------------

      function Action_Decls return Array_Of_Program_Trees is
         Result : Array_Of_Program_Trees (Action_Groups'Range);
      begin
         for I in Result'Range loop
            Result (I) :=
              Make_Tree ("declaration",
                         Ada_Case (Action_Groups (I).Name) &
                           ": Aquarius.Actions.Action_Group;");
         end loop;
         return Result;
      end Action_Decls;

      ------------------
      -- Action_Inits --
      ------------------

      function Action_Inits return Array_Of_Program_Trees is
         Result : Array_Of_Program_Trees (Action_Groups'Range);
      begin
         for I in Result'Range loop
            declare
               Group_Name   : constant String :=
                 Action_Groups (I).Name;
               Trigger_Name : constant String :=
                 Action_Triggers (I).Name;
            begin
               Result (I) :=
                 Make_Tree ("statement",
                            "Grammar.Add_Action_Group (""" &
                              Group_Name & """, " &
                              "Aquarius.Actions." &
                              Ada_Case (Trigger_Name) &
                              "_Trigger, " &
                              Ada_Case (Group_Name) & ");");
            end;
         end loop;
         return Result;
      end Action_Inits;

      Binder_Body     : constant Program_Tree :=
        Plugin.Ada_Grammar.Make_Program_Tree
        ("handled_sequence_of_statements");

      Binder_Block    : constant Program_Tree :=
        Plugin.Ada_Grammar.Make_Program_Tree ("block");

      Context         : Aquarius.Programs.Parser.Parse_Context;

   begin
      Aquarius.Programs.Parser.Repeat_Child (Binder_Body, Action_Inits);

      for I in Action_Binders'Range loop
         Aquarius.Programs.Parser.Repeat_Child
           (Binder_Body, (1 => Program_Tree (Action_Binders (I))));
      end loop;

      Aquarius.Programs.Parser.Initialise_Parse_Context
        (Context, Plugin.Ada_Grammar, Binder_Block, False, False);

      declare
         Decls : constant Array_Of_Program_Trees := Action_Decls;
      begin
         for I in Decls'Range loop
            Aquarius.Programs.Parser.Parse_Tree (Decls (I), Context);
         end loop;
      end;

      Aquarius.Programs.Parser.Parse_Token
        (Aquarius.Tokens.Get_Token (Plugin.Ada_Grammar.Frame, "begin"),
         Aquarius.Source.No_Source_Position,
         "begin",
         Context);

      Aquarius.Programs.Parser.Parse_Tree (Binder_Body, Context);

      Aquarius.Programs.Parser.Parse_Token
        (Aquarius.Tokens.Get_Token (Plugin.Ada_Grammar.Frame, "end"),
         Aquarius.Source.No_Source_Position,
         "end",
         Context);

      Aquarius.Programs.Parser.Parse_Token
        (Aquarius.Tokens.Get_Token (Plugin.Ada_Grammar.Frame, "identifier"),
         Aquarius.Source.No_Source_Position,
         "Bind_Actions",
         Context);

      return Make_Tree ("procedure_declaration",
                        "   procedure Bind_Actions (Plugin : in out " &
                          Plugin_Name & "_Type;" &
                          "Grammar : in Aquarius.Grammars.Aquarius_Grammar" &
                          ") is",
                        Binder_Block,
                        ";");
   end Create_Binder;

   -----------------
   -- Create_Body --
   -----------------

   function Create_Body (Plugin_Name   : String;
                         Project_Name  : String;
                         Package_Name  : String;
                         Project       : Aquarius.Programs.Program_Tree)
                        return Aquarius.Programs.Program_Tree
   is
      use Ada.Characters.Latin_1;
      use Aquarius.Programs;
      Package_Withs : constant String :=
        To_Package_Withs
        (Project_Name,
         Project.All_Properties (Plugin.Property_Dependent));
      Body_Decls   : constant Program_Tree :=
        Repeat_Child ("list_of_declarations",
                      Plugin.Property_Ada_Spec_Tree,
                      Project.Direct_Children ("declaration"));
   begin

      Aquarius.Trees.Add_Child (Aquarius.Trees.Tree (Body_Decls),
                                Create_Binder (Plugin_Name, Project));

      Repeat_Child (Body_Decls,
                    Plugin.Property_Ada_Body_Tree,
                    Project.Direct_Children ("declaration"));

      return Make_Tree ("compilation_unit",
                        "with Aquarius.Actions;" & LF &
                        "with Aquarius.Programs;" & LF &
                          Package_Withs & LF &
                          "package body " & Package_Name & " is",
                        Body_Decls,
                        "end " & Package_Name & ";");
   end Create_Body;

   -----------------
   -- Create_Spec --
   -----------------

   function Create_Spec (Plugin_Name   : String;
                         Project_Name  : String;
                         Package_Name  : String)
                        return Aquarius.Programs.Program_Tree
   is
      use Ada.Characters.Latin_1;
   begin
      return Make_Tree ("compilation_unit",
                        "with " & Project_Name & ";"
                        & "with Aquarius.Grammars;"
                        & "package " & Package_Name & " is" & LF
                        & "   procedure Bind_Actions (Plugin : in out "
                        & Plugin_Name & "_Type;"
                        & "Grammar : in Aquarius.Grammars.Aquarius_Grammar);"
                        & "end " & Package_Name & ";");
   end Create_Spec;

   -----------------------
   -- Declaration_After --
   -----------------------

   procedure Declaration_After
     (Decl_Actionable : not null access Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      D  : constant Program_Tree := Program_Tree (Decl_Actionable);
      Ds : constant Array_Of_Program_Trees := D.Direct_Children;
   begin
      if Ds'Length = 1 then
         if Ds (Ds'First).Has_Property (Plugin.Property_Ada_Spec_Tree) then
            D.Set_Property
              (Plugin.Property_Ada_Spec_Tree,
               Ds (Ds'First).Property (Plugin.Property_Ada_Spec_Tree));
         end if;
         if Ds (Ds'First).Has_Property (Plugin.Property_Ada_Body_Tree) then
            D.Set_Property
              (Plugin.Property_Ada_Body_Tree,
               Ds (Ds'First).Property (Plugin.Property_Ada_Body_Tree));
         end if;
      end if;
   end Declaration_After;

   ----------
   -- Load --
   ----------

   overriding
   procedure Load (Plugin  : not null access Plugin_Plugin;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar)
   is
      Builder : Actions.Action_Group;
   begin
      Load (Aquarius_Plugin_Type (Plugin.all)'Access, Grammar);

      Grammar.Add_Action_Group ("builder",
                                Actions.Parse_Trigger,
                                Builder);

      Plugin.Register_Action
        ("action_group_declaration", Builder, Aquarius.After,
         Action_Group_After'Access);

      Plugin.Register_Action
        ("action_handler", Builder, Aquarius.After,
         Action_Handler_After'Access);

      Plugin.Register_Action
        ("declaration", Builder, Aquarius.After,
         Declaration_After'Access);

      Plugin.Register_Action
        ("package_declaration", Builder, Aquarius.After,
         Package_Declaration_After'Access);

      Plugin.Register_Action
        ("package_declaration", "qualified_identifier",
         Builder, Aquarius.After,
         Package_After_Qualified_Identifier'Access);

      Plugin.Register_Action
        ("package_declaration", "identifier",
         Builder, Aquarius.After,
         Package_After_Identifier'Access);

      Plugin.Register_Action
        ("project", "name", Builder, Aquarius.After,
         Project_After_Name'Access);

      Plugin.Register_Action
        ("project", Builder, Aquarius.After,
         Project_After'Access);

      Plugin.Ada_Grammar := Aquarius.Grammars.Manager.Get_Grammar ("ada");

      Grammar.Create_Property (Plugin.Property_Ada_Spec_Tree,
                               "ada-spec-tree",
                               False, True);
      Grammar.Create_Property (Plugin.Property_Ada_Body_Tree, "ada-body-tree",
                               False, True);

      Grammar.Create_Property (Plugin.Property_Package_Name,
                               "plugin-package-name",
                               True, True);

      Grammar.Create_Property (Plugin.Property_Internal_Package_Name,
                               "plugin-internal-package-name",
                               True, True);

      Grammar.Create_Property (Plugin.Property_Plugin_Name,
                               "plugin-plugin-name",
                               True, True);

      Grammar.Create_Property (Plugin.Property_Project_Name,
                               "plugin-project-name",
                               True, True);

      Grammar.Create_Property (Plugin.Property_Dependent,
                               "plugin-dependent",
                               False, True);

      Grammar.Create_Property (Plugin.Property_Action_Group,
                               "plugin-action-group",
                               False, True);

      Grammar.Create_Property (Plugin.Property_Package_Group,
                               "plugin-package-group",
                               True, True);

      Grammar.Create_Property (Plugin.Property_Action_Trigger,
                               "plugin-action-trigger",
                               False, True);

      Grammar.Create_Property (Plugin.Property_Action_Binder,
                               "plugin-action-binder",
                               False, True);

      Global_Plugin_Plugin := Plugin_Plugin_Access (Plugin);
   end Load;

   --------------------------------
   -- Make_Internal_Package_Name --
   --------------------------------

   function Make_Internal_Package_Name
     (Sub_Names : Aquarius.Programs.Array_Of_Program_Trees)
     return String
   is
   begin
      if Sub_Names'Length = 0 then
         return "";
      elsif Sub_Names'Length = 1 then
         return Sub_Names (Sub_Names'First).Text;
      else
         return Sub_Names (Sub_Names'First).Text & "_" &
           Make_Internal_Package_Name (Sub_Names (Sub_Names'First + 1 ..
                                                    Sub_Names'Last));
      end if;
   end Make_Internal_Package_Name;

   -------------------------
   -- Make_Qualified_Name --
   -------------------------

   function Make_Qualified_Name
     (Sub_Names : Aquarius.Programs.Array_Of_Program_Trees)
     return String
   is
   begin
      if Sub_Names'Length = 0 then
         return "";
      elsif Sub_Names'Length = 1 then
         return Sub_Names (Sub_Names'First).Text;
      else
         return Sub_Names (Sub_Names'First).Text & "." &
           Make_Qualified_Name (Sub_Names (Sub_Names'First + 1 ..
                                             Sub_Names'Last));
      end if;
   end Make_Qualified_Name;

   ---------------
   -- Make_Tree --
   ---------------

   function Make_Tree (Top_Node : String;
                       Source_Text : String)
                      return Aquarius.Programs.Program_Tree
   is
      Result : constant Aquarius.Programs.Program_Tree :=
        Plugin.Ada_Grammar.Make_Program_Tree (Top_Node);
   begin
      Aquarius.Programs.Parser.Parse_Tree (Result, Source_Text);
      return Result;
   end Make_Tree;

   ---------------
   -- Make_Tree --
   ---------------

   function Make_Tree (Top_Node    : String;
                       Before_Text : String;
                       Child       : Aquarius.Programs.Program_Tree;
                       After_Text  : String)
                      return Aquarius.Programs.Program_Tree
   is
      Result : constant Aquarius.Programs.Program_Tree :=
        Plugin.Ada_Grammar.Make_Program_Tree (Top_Node);
   begin
      Aquarius.Programs.Parser.Parse_Tree (Result, Before_Text,
                                           Child, After_Text);
      return Result;
   end Make_Tree;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Plugin : Plugin_Plugin) return String is
      pragma Unreferenced (Plugin);
   begin
      return "plugin";
   end Name;

   ------------------------------
   -- Package_After_Identifier --
   ------------------------------

   procedure Package_After_Identifier
     (Package_Actionable    : not null access Actions.Actionable'Class;
      Identifier_Actionable : not null access Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Pack : constant Program_Tree := Program_Tree (Package_Actionable);
      Name : constant String := Program_Tree (Identifier_Actionable).Text;
   begin
      Pack.Set_Property (Plugin.Property_Package_Group,
                         Aquarius.Names.Name_Value (Name));
   end Package_After_Identifier;

   ------------------------------
   -- Package_After_Identifier --
   ------------------------------

   procedure Package_After_Qualified_Identifier
     (Package_Actionable    : not null access Actions.Actionable'Class;
      Identifier_Actionable : not null access Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      Pack : constant Program_Tree := Program_Tree (Package_Actionable);
      Q : constant Program_Tree := Program_Tree (Identifier_Actionable);
      Ids : constant Array_Of_Program_Trees :=
        Q.Direct_Children;
      Name : constant String := Make_Qualified_Name (Ids);
      Internal_Name : constant String :=
        Make_Internal_Package_Name (Ids);
   begin
      Pack.Set_Property (Plugin.Property_Package_Name,
                         Aquarius.Names.Name_Value (Name));
      Pack.Set_Property (Plugin.Property_Internal_Package_Name,
                         Aquarius.Names.Name_Value (Internal_Name));
   end Package_After_Qualified_Identifier;

   -------------------------------
   -- Package_Declaration_After --
   -------------------------------

   procedure Package_Declaration_After
     (Package_Decl_Actionable : not null access Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      P : constant Program_Tree := Program_Tree (Package_Decl_Actionable);
      Q : constant Program_Tree := P.Program_Child ("qualified_identifier");
      Ids : constant Array_Of_Program_Trees :=
        Q.Direct_Children;
      Q_Name : constant String := Make_Qualified_Name (Ids);
      I_Name : constant String := Make_Internal_Package_Name (Ids);
      Decls  : constant Array_Of_Program_Trees :=
        P.Direct_Children ("declaration");
   begin
      declare
         Result_Spec  : constant Program_Tree :=
           Make_Tree ("package_declaration",
                      "package " & I_Name & " is",
                      Repeat_Child ("list_of_declarations",
                                    Plugin.Property_Ada_Spec_Tree,
                                    Decls),
                      "end " & I_Name & ";");
         Result_Body  : constant Program_Tree :=
           Make_Tree ("package_declaration",
                      "package body " & I_Name & " is",
                      Repeat_Child ("list_of_declarations",
                                    Plugin.Property_Ada_Body_Tree,
                                    Decls),
                      "end " & I_Name & ";");
      begin
         P.Set_Property (Plugin.Property_Ada_Spec_Tree,
                         Result_Spec);
         P.Set_Property (Plugin.Property_Ada_Body_Tree,
                         Result_Body);
         P.Set_Property (Plugin.Property_Dependent,
                         Aquarius.Names.Name_Value (Q_Name));
      end;
   end Package_Declaration_After;

   ------------
   -- Plugin --
   ------------

   function Plugin return Plugin_Plugin_Access is
   begin
      return Global_Plugin_Plugin;
   end Plugin;

   -------------------
   -- Project_After --
   -------------------

   procedure Project_After
     (Project_Actionable : not null access Actions.Actionable'Class)
   is
      use Aquarius.Programs;
      use Ada.Characters.Handling;
      use Ada.Characters.Latin_1;
      Project : constant Program_Tree := Program_Tree (Project_Actionable);
      Plugin_Name  : constant String :=
        Project.Property (Plugin.Property_Plugin_Name).Name;
      Project_Name  : constant String :=
        Project.Property (Plugin.Property_Project_Name).Name;
      Package_Name : constant String := Project_Name & ".Generated";

      Result_Spec  : constant Program_Tree :=
        Create_Spec (Plugin_Name, Project_Name, Package_Name);

      Result_Body  : constant Program_Tree :=
        Create_Body (Plugin_Name, Project_Name, Package_Name, Project);

      Path : String := To_Lower (Package_Name);
   begin
      Project.Set_Property (Plugin.Property_Ada_Spec_Tree,
                            Result_Spec);
      for I in Path'Range loop
         if Path (I) = '.' then
            Path (I) := '-';
         end if;
      end loop;

      Write_Package (Result_Spec, Path & ".ads");
      Write_Package (Result_Body, Path & ".adb");
   end Project_After;

   ------------------------
   -- Project_After_Name --
   ------------------------

   procedure Project_After_Name
     (Project_Actionable  : not null access Actions.Actionable'Class;
      Name_Actionable     : not null access Actions.Actionable'Class)
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Programs;

      Project      : constant Program_Tree :=
                       Program_Tree (Project_Actionable);
      Name         : constant Program_Tree := Program_Tree (Name_Actionable);
      Ids          : constant Array_Of_Program_Trees :=
        Name.Direct_Children (Skip_Separators => True);
      Project_Name : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Ids'Range loop
         if Project_Name /= "" then
            Project_Name := Project_Name & ".";
         end if;
         Project_Name := Project_Name & Ids (I).Text;
      end loop;

      Project.Set_Property (Plugin.Property_Project_Name,
                            Aquarius.Names.Name_Value
                              (To_String (Project_Name)));
      Project.Set_Property (Plugin.Property_Plugin_Name,
                            Aquarius.Names.Name_Value (Ids (Ids'Last).Text));
   end Project_After_Name;

   ------------------
   -- Repeat_Child --
   ------------------

   function Repeat_Child
     (Top_Node    : String;
      Property    : Aquarius.Properties.Property_Type;
      Child       : Aquarius.Programs.Array_Of_Program_Trees)
     return Aquarius.Programs.Program_Tree
   is
      use Aquarius.Programs;
      Result : constant Program_Tree :=
        Plugin.Ada_Grammar.Make_Program_Tree (Top_Node);
   begin
      Repeat_Child (Result, Property, Child);
      return Result;
   end Repeat_Child;

   ------------------
   -- Repeat_Child --
   ------------------

   procedure Repeat_Child
     (Result      : Aquarius.Programs.Program_Tree;
      Property    : Aquarius.Properties.Property_Type;
      Child       : Aquarius.Programs.Array_Of_Program_Trees)
   is
      use Aquarius.Programs;
      Ada_Child : Array_Of_Program_Trees (Child'Range);
      Count     : Natural := 0;
   begin
      for I in Child'Range loop
         if Child (I).Has_Property (Property) then
            Count := Count + 1;
            declare
               use Aquarius.Trees;
               Value : constant Aquarius_Object :=
                 Child (I).Property (Property);
            begin
               if Value = null then
                  raise Constraint_Error with
                    "have a property, but it is null";
               end if;

               Ada_Child (Count) := Program_Tree (Value);
            end;
         end if;
      end loop;
      Aquarius.Programs.Parser.Repeat_Child (Result, Ada_Child (1 .. Count));
   end Repeat_Child;

   ----------------------
   -- To_Package_Withs --
   ----------------------

   function To_Package_Withs (Parent : String;
                              Props  : Aquarius.Trees.Array_Of_Objects)
                             return String
   is

      function To_Withs (P : Aquarius.Trees.Array_Of_Objects) return String;

      --------------
      -- To_Withs --
      --------------

      function To_Withs (P : Aquarius.Trees.Array_Of_Objects) return String is
      begin
         if P'Length = 1 then
            return Parent & "." & P (P'First).Name & ";";
         else
            return Parent & "." & P (P'First).Name & "," &
              To_Withs (P (P'First + 1 .. P'Last));
         end if;
      end To_Withs;

   begin
      if Props'Length = 0 then
         return "";
      else
         return "with " & To_Withs (Props);
      end if;
   end To_Package_Withs;

   -------------
   -- Version --
   -------------

   overriding
   function Version (Plugin : Plugin_Plugin) return String is
      pragma Unreferenced (Plugin);
   begin
      return "0.1";
   end Version;

   -------------------
   -- Write_Package --
   -------------------

   procedure Write_Package (P    : Aquarius.Programs.Program_Tree;
                            Path : String)
   is
      use Ada.Text_IO;

      Renderer : Aquarius.Rendering.Aquarius_Renderer :=
                   Aquarius.Rendering.Manager.Renderer ("text");
      File     : File_Type;

   begin

      Aquarius.Programs.Arrangements.Arrange (P);
      Renderer.Set_Theme (Aquarius.Themes.Active_Theme);

      Ada.Text_IO.Put_Line ("Writing to: " & Path);

      Create (File, Out_File, Path);
      Set_Output (File);

      Aquarius.Programs.Arrangements.Render
        (Program   => P,
         Renderer  => Renderer,
         Point     => Aquarius.Trees.Cursors.Left_Of_Tree (P),
         Partial   => "");

      New_Line;

      Set_Output (Standard_Output);
      Close (File);

   end Write_Package;

end Aquarius.Plugins.Plugin;
