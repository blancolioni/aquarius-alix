with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

with WL.String_Maps;

with Aquarius.Config_Paths;

package body Aquarius.Grammars.Aqua_Gen is

   procedure Generate_Class
     (Language_Name : String;
      Syntax        : Aquarius.Syntax.Syntax_Tree);

   procedure Generate_Empty_Class
     (Language_Name : String;
      Class_Name    : String;
      File_Name     : String);

   function To_Mixed_Case (Name : String) return String;

   function To_File_Name (Name : String) return String;

   ------------------------
   -- Add_Ancestor_Class --
   ------------------------

   procedure Add_Ancestor_Class
     (Generator : in out Aqua_Generator_Type'Class;
      Root      : Aquarius.Syntax.Syntax_Tree;
      Group     : Aquarius.Actions.Action_Group;
      Name      : String)
   is
   begin
      Generator.Ancestors.Append
        (Ancestor_Record'
           (Root  => Root,
            Group => Group,
            Name  => Aquarius.Names.To_Aquarius_Name (Name)));
   end Add_Ancestor_Class;

   ------------
   -- Create --
   ------------

   procedure Create
     (Generator : in out Aqua_Generator_Type'Class;
      Grammar   : Aquarius_Grammar;
      Path      : String)
   is
   begin
      Generator.Grammar := Grammar;
      Generator.Path := Aquarius.Names.To_Aquarius_Name (Path);
      Generator.Ancestors.Clear;
   end Create;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Generator : Aqua_Generator_Type'Class)
   is
      procedure Generate_Group
        (Group : Aquarius.Actions.Action_Group);

      procedure Generate_Group
        (Group : Aquarius.Actions.Action_Group)
      is
      begin
         Generator.Generate_Bindings
           (Group_Name => Aquarius.Actions.Action_Group_Name (Group));
      end Generate_Group;

   begin
      Generator.Generate_Empty_Class (Generator.Grammar.Name);

      Generator.Generate_Bindings ("syntax");

      for Trigger in Aquarius.Actions.Action_Execution_Trigger loop
         Aquarius.Actions.Iterate
           (List    => Generator.Grammar.Action_Groups,
            Trigger => Trigger,
            Process => Generate_Group'Access);
      end loop;
   end Execute;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Grammar : Aquarius_Grammar)
   is
   begin
      Generate_Empty_Class (Grammar.Name, Grammar.Name, Grammar.Name);
      Generate_Empty_Class
        (Grammar.Name,
         Grammar.Name & ".Syntax", Grammar.Name & "-syntax");

      for Syntax of Grammar.Non_Terminals loop
         Generate_Class (Grammar.Name, Syntax);
      end loop;
   end Generate;

   --------------------------
   -- Generate_Ada_Binding --
   --------------------------

   procedure Generate_Ada_Binding
     (Grammar : Aquarius_Grammar;
      Path    : String)
   is
      use Ada.Characters.Handling;
      use Ada.Text_IO;
      File : File_Type;
      Base_Package_Path : constant String :=
                            Path & "/" & To_Lower (Grammar.Name) & "_binding";
      Spec_Path         : constant String :=
                            Base_Package_Path & ".ads";
--        Body_Path         : constant String :=
--                              Base_Package_Path & ".adb";
      Package_Name      : constant String :=
                            To_Mixed_Case (Grammar.Name & "_Binding");
   begin
      Create (File, Out_File, Spec_Path);
      Set_Output (File);

      Put_Line ("with Ada.Text_IO;");
      New_Line;
      Put_Line ("with Aquarius.Programs;");
      New_Line;
      Put_Line ("package " & Package_Name & " is");
      New_Line;

      for Syntax of Grammar.Non_Terminals loop
         declare
            Base_Name : constant String := To_Mixed_Case (Syntax.Name);
            Type_Name : constant String := Base_Name & "_Type";
         begin
            Put_Line ("   type " & Type_Name & " is private;");
            New_Line;
            Put_Line ("   function Import_" & Base_Name);
            Put_Line ("     (Tree : Aquarius.Programs.Program_Tree)");
            Put_Line ("     return " & Type_Name & ";");
            New_Line;
         end;
      end loop;

      Put_Line ("private");
      New_Line;

      for Syntax of Grammar.Non_Terminals loop
         Put_Line ("   type " & To_Mixed_Case (Syntax.Name & "_Record;"));
         Put_Line ("   type " & To_Mixed_Case (Syntax.Name & "_Type")
                   & " is access "
                   & To_Mixed_Case (Syntax.Name & "_Record;"));
         New_Line;
      end loop;

      Put_Line ("end " & Package_Name & ";");
      Set_Output (Standard_Output);
      Close (File);
   end Generate_Ada_Binding;

   ----------------------
   -- Generate_Binding --
   ----------------------

   procedure Generate_Binding
     (Gen        : Aqua_Generator_Type'Class;
      Group_Name : String;
      Rule       : Aquarius.Syntax.Syntax_Tree)
   is
      use Ada.Text_IO;

      use Aquarius.Syntax;

      package Found_Syntax_Maps is
        new WL.String_Maps (String);

      Found_Syntax : Found_Syntax_Maps.Map;

      Have_Feature : Boolean := False;

      Suffix : constant String := "";

      function Tree_Reference
        (Tree : Aquarius.Syntax.Syntax_Tree)
         return String
      is (To_Mixed_Case (Gen.Grammar.Name)
          & "."
          & To_Mixed_Case (Group_Name)
            & "."
          & To_Mixed_Case (Tree.Name)
          & Suffix);

      File : File_Type;

      Class_Name : constant String := Tree_Reference (Rule);

      procedure Write_Inheritance;

      procedure Scan_Features
        (Tree     : Aquarius.Syntax.Syntax_Tree;
         Repeated : Boolean;
         Optional : Boolean);

      -------------------
      -- Scan_Features --
      -------------------

      procedure Scan_Features
        (Tree     : Aquarius.Syntax.Syntax_Tree;
         Repeated : Boolean;
         Optional : Boolean)
      is

         procedure Put_Feature (Prefix : String);

         -----------------
         -- Put_Feature --
         -----------------

         procedure Put_Feature (Prefix : String) is
            Feature_Name : constant String :=
                             To_Mixed_Case (Prefix & Tree.Name);
         begin
            New_Line;
            Put ("   " & Feature_Name);
            if Tree.Syntax_Class = Terminal then
               Put_Line (" (Text : String)");
            else
               Put_Line (" (Child : " & Tree_Reference (Tree) & ")");
            end if;
            Put_Line ("      do");
            Put_Line ("      end");
         end Put_Feature;

      begin
         if Tree.Syntax_Class = Terminal then
            if not Aquarius.Tokens.Is_Reserved
              (Tree.Frame, Tree.Token)
            then
               declare
                  Name : constant String :=
                           To_Mixed_Case
                             (Aquarius.Tokens.Get_Name
                                (Tree.Frame, Tree.Token));
               begin
                  if Name /= "$symbol"
                    and then not Found_Syntax.Contains (Name)

                  then
                     if not Have_Feature then
                        New_Line;
                        Put_Line ("feature");
                        Have_Feature := True;
                     end if;

                     New_Line;

                     if Group_Name = "syntax" then
                        Put_Line
                          ("   "
                           & To_Mixed_Case
                             (Aquarius.Tokens.Get_Name
                                  (Tree.Frame, Tree.Token))
                           & " : "
                           & (if Optional then "detachable " else "")
                           & (if Repeated then "List[String]" else "String"));
                     else
                        Put_Feature ("Before_");
                        Put_Feature ("After_");
                     end if;
                     Found_Syntax.Insert (Name, "");
                  end if;
               end;
            end if;
         elsif Tree.Name /= "" then
            if Group_Name /= "syntax" then
               if not Have_Feature then
                  New_Line;
                  Put_Line ("feature");
                  Have_Feature := True;
               end if;

               if not Found_Syntax.Contains (Tree.Name) then
                  Put_Feature ("Before_");
                  Put_Feature ("After_");
                  Found_Syntax.Insert (Tree.Name, "");
               end if;
            end if;
         else
            for I in 1 .. Tree.Child_Count loop
               Scan_Features
                 (Aquarius.Syntax.Syntax_Tree (Tree.Child (I)),
                  Repeated or else Tree.Repeatable,
                  Optional or else Tree.Optional
                  or else Tree.Syntax_Class = Choice);
            end loop;
         end if;
      end Scan_Features;

      -----------------------
      -- Write_Inheritance --
      -----------------------

      procedure Write_Inheritance is
         First : Boolean := True;
      begin
         if Group_Name = "syntax" then
            Put_Line ("inherit");
            Put_Line ("   Aquarius.Trees.Program_Tree");
            First := False;
         end if;

         for Element of Gen.Ancestors loop
            if Aquarius.Actions.Action_Group_Name (Element.Group)
              = Group_Name
            then
               if First then
                  Put_Line ("inherit");
                  First := False;
               end if;
               Put_Line ("   " & Aquarius.Names.To_String (Element.Name));
            end if;
         end loop;
      end Write_Inheritance;

   begin
      Create (File, Out_File,
              Ada.Directories.Compose
                (Aquarius.Names.To_String (Gen.Path),
                 To_File_Name (Class_Name)));

      Set_Output (File);
      Put_Line ("class " & Class_Name);
      New_Line;
      Write_Inheritance;

      for I in 1 .. Rule.Child_Count loop
         Scan_Features
           (Aquarius.Syntax.Syntax_Tree (Rule.Child (I)),
            Rule.Repeatable,
            Rule.Optional or else Rule.Syntax_Class = Choice);
      end loop;
      New_Line;
      Put_Line ("end");
      Set_Output (Standard_Output);
      Close (File);
   end Generate_Binding;

   -----------------------
   -- Generate_Bindings --
   -----------------------

   procedure Generate_Bindings
     (Gen        : Aqua_Generator_Type'Class;
      Group_Name : String)
   is
   begin
      Gen.Generate_Empty_Class (Gen.Grammar.Name & "." & Group_Name);

      for Syntax of Gen.Grammar.Non_Terminals loop
         Gen.Generate_Binding (Group_Name, Syntax);
      end loop;
   end Generate_Bindings;

   --------------------
   -- Generate_Class --
   --------------------

   procedure Generate_Class
     (Language_Name : String;
      Syntax        : Aquarius.Syntax.Syntax_Tree)
   is
      use Ada.Characters.Handling;
      use Ada.Text_IO;

      use Aquarius.Syntax;

      package Found_Syntax_Maps is
        new WL.String_Maps (String);

      Found_Syntax : Found_Syntax_Maps.Map;

      Have_Feature : Boolean := False;

      procedure Scan_Features
        (Tree     : Aquarius.Syntax.Syntax_Tree;
         Repeated : Boolean;
         Optional : Boolean);

      -------------------
      -- Scan_Features --
      -------------------

      procedure Scan_Features
        (Tree     : Aquarius.Syntax.Syntax_Tree;
         Repeated : Boolean;
         Optional : Boolean)
      is

         procedure Put_Feature (Prefix : String);

         -----------------
         -- Put_Feature --
         -----------------

         procedure Put_Feature (Prefix : String) is
            Feature_Name : constant String :=
                             To_Mixed_Case (Prefix & Tree.Name);
         begin
            New_Line;
            Put_Line ("   " & Feature_Name
                      & " (Child : "
                      & To_Mixed_Case (Language_Name)
                      & ".Syntax."
                      & To_Mixed_Case (Tree.Name & "_Node")
                      & ")");
            Put_Line ("      do");
            Put_Line ("      end");
         end Put_Feature;

      begin
         if Tree.Syntax_Class = Terminal then
            if not Aquarius.Tokens.Is_Reserved
              (Tree.Frame, Tree.Token)
            then
               declare
                  Name : constant String :=
                           To_Mixed_Case
                             (Aquarius.Tokens.Get_Name
                                (Tree.Frame, Tree.Token));
               begin
                  if not Found_Syntax.Contains (Name) then
                     if not Have_Feature then
                        New_Line;
                        Put_Line ("feature");
                        Have_Feature := True;
                     end if;

                     New_Line;
                     Put_Line
                       ("   "
                        & To_Mixed_Case
                          (Aquarius.Tokens.Get_Name (Tree.Frame, Tree.Token))
                        & " : "
                        & (if Optional then "detachable " else "")
                        & (if Repeated then "List[String]" else "String"));
                     Found_Syntax.Insert (Name, "");
                  end if;
               end;
            end if;
         elsif Tree.Name /= "" then
            if False then
               if not Found_Syntax.Contains (Tree.Name) then
                  Put_Feature ("Before_");
                  Put_Feature ("After_");
                  Found_Syntax.Insert (Tree.Name, "");
               end if;
            end if;
         else
            for I in 1 .. Tree.Child_Count loop
               Scan_Features
                 (Aquarius.Syntax.Syntax_Tree (Tree.Child (I)),
                  Repeated or else Tree.Repeatable,
                  Optional or else Tree.Optional
                  or else Tree.Syntax_Class = Choice);
            end loop;
         end if;
      end Scan_Features;

      File : File_Type;

      Class_Name : constant String :=
                     To_Mixed_Case (Language_Name)
                   & ".Syntax."
                     & To_Mixed_Case (Syntax.Name & "_Node");

   begin
      Create (File, Out_File,
              Aquarius.Config_Paths.Config_File
                ("aqua/generated/"
                 & To_Lower (Language_Name)
                 & "-syntax-"
                 & To_Lower (Syntax.Name)
                 & "_node"
                 & ".aqua"));

      Set_Output (File);
      Put_Line ("class " & Class_Name);
      New_Line;
      Put_Line ("inherit");
      Put_Line ("   Aquarius.Trees.Program_Tree");
      for I in 1 .. Syntax.Child_Count loop
         Scan_Features
           (Aquarius.Syntax.Syntax_Tree (Syntax.Child (I)),
            Syntax.Repeatable,
            Syntax.Optional or else Syntax.Syntax_Class = Choice);
      end loop;
      New_Line;
      Put_Line ("end");
      Set_Output (Standard_Output);
      Close (File);
   end Generate_Class;

   --------------------------
   -- Generate_Empty_Class --
   --------------------------

   procedure Generate_Empty_Class
     (Gen        : Aqua_Generator_Type'Class;
      Class_Name : String)
   is
      use Ada.Text_IO;

      File : File_Type;

      File_Name : constant String := To_File_Name (Class_Name);
   begin
      Create (File, Out_File,
              Ada.Directories.Compose
                (Aquarius.Names.To_String (Gen.Path), File_Name));
      Set_Output (File);
      Put_Line ("class");
      Put_Line ("   " & To_Mixed_Case (Class_Name));
      New_Line;
      Put_Line ("end");
      Set_Output (Standard_Output);
      Close (File);
   exception
      when Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line
           ("directory not found or not writeable: "
            & Aquarius.Names.To_String (Gen.Path));
         raise;
   end Generate_Empty_Class;

   --------------------------
   -- Generate_Empty_Class --
   --------------------------

   procedure Generate_Empty_Class
     (Language_Name : String;
      Class_Name    : String;
      File_Name     : String)
   is
      pragma Unreferenced (Language_Name);
      use Ada.Characters.Handling;
      use Ada.Text_IO;

      File : File_Type;

   begin
      Create (File, Out_File,
              Aquarius.Config_Paths.Config_File
                ("aqua/generated/"
                 & To_Lower (File_Name)
                 & ".aqua"));

      Set_Output (File);
      Put_Line ("class " & To_Mixed_Case (Class_Name));
      New_Line;
      Put_Line ("end");
      Set_Output (Standard_Output);
      Close (File);
   end Generate_Empty_Class;

   -------------------
   -- To_Mixed_Case --
   -------------------

   function To_File_Name (Name : String) return String is
      use Ada.Characters.Handling;
      Result : String := Name;
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         else
            Result (I) := To_Lower (Result (I));
         end if;
      end loop;
      return Result & ".aqua";
   end To_File_Name;

   -------------------
   -- To_Mixed_Case --
   -------------------

   function To_Mixed_Case (Name : String) return String is
      use Ada.Characters.Handling;
      Result : String := Name;
      First  : Boolean := True;
   begin
      for I in Result'Range loop
         if Result (I) = '_' then
            First := True;
         elsif First then
            Result (I) := To_Upper (Result (I));
            First := False;
         end if;
      end loop;
      return Result;
   end To_Mixed_Case;

end Aquarius.Grammars.Aqua_Gen;
