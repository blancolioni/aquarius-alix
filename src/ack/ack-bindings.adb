with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with WL.String_Maps;

with Ack.Bindings.Actions;
with Ack.Classes;
with Ack.Compile;
with Ack.Errors;
with Ack.Features;
with Ack.Types;

with Aquarius.Syntax;
with Aquarius.Trees;

package body Ack.Bindings is

   Report_Calls          : constant Boolean := False;
   Report_Implicit_Calls : constant Boolean := False;

   package Link_Name_To_Class_Maps is
     new WL.String_Maps
       (Ack.Classes.Constant_Class_Entity, Ack.Classes."=");

   ----------------------
   -- Load_Ack_Binding --
   ----------------------

   function Load_Ack_Binding
     (Action_File_Path : String;
      Base_Aqua_Path   : String;
      Image            : Aqua.Images.Image_Type;
      Grammar          : Aquarius.Grammars.Aquarius_Grammar;
      Group            : Aquarius.Actions.Action_Group;
      Trigger          : Aquarius.Actions.Action_Execution_Trigger)
      return Boolean
   is
      pragma Unreferenced (Trigger);

      Action_File : Ada.Text_IO.File_Type;

      References    : List_Of_Entities.List;
      Binding_Table : Ack.Bindings.Actions.Ack_Binding_Table;
      Local_Classes : Link_Name_To_Class_Maps.Map;

      function Is_Group_Reference
        (Class    : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Property : not null access constant Root_Entity_Type'Class)
         return Boolean;

      procedure Load_Class
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      procedure Add_Feature_Binding
        (Class        : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Feature      : not null access constant
           Ack.Root_Entity_Type'Class);

      procedure Start_Binding
        (Parent_Tree      : String;
         Child_Tree       : String;
         Parent_Link_Name : String;
         Child_Link_Name  : String;
         Position         : Binding_Position);

      procedure Finish_Binding;

      procedure Check_Bindings
        (Tree_Name : String);

      procedure Check_Conforming_Children
        (Parent_Name : String;
         Child_Name  : String);

      procedure Check_Allocated
        (Class     : String;
         Tree_Name : String);

      -------------------------
      -- Add_Feature_Binding --
      -------------------------

      procedure Add_Feature_Binding
        (Class        : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Feature      : not null access constant
           Ack.Root_Entity_Type'Class)
      is
         Feature_Name : constant String := Feature.Standard_Name;
         Index         : constant Natural :=
                           Ada.Strings.Fixed.Index (Feature_Name, "_");
         Position_Name : constant String :=
                           (if Index > 0
                            then Feature_Name (Feature_Name'First .. Index - 1)
                            else "");
         Position      : constant Binding_Position :=
                           (if Position_Name = "after"
                            then After
                            else Before);
         Child_Tree    : constant String :=
                           Feature_Name (Index + 1 .. Feature_Name'Last);
         Parent_Tree   : constant String := Class.Standard_Name;
         Group_Property : constant String := Class.Link_Name;
      begin

         if not Grammar.Have_Syntax (Parent_Tree) then
            return;
         end if;

         if not Local_Classes.Contains (Class.Standard_Name) then
            Local_Classes.Insert
              (Class.Standard_Name,
               Ack.Classes.Constant_Class_Entity (Class));
         end if;

         if Ack.Features.Is_Feature (Feature)
           and then Ack.Features.Feature_Entity_Record (Feature.all)
           .Is_Property
           and then Is_Group_Reference
             (Class,
              Ack.Features.Feature_Entity_Record'Class (Feature.all)'Access)
         then
            References.Append (Feature);
            return;
         end if;

         if Position_Name = ""
           or else not Grammar.Have_Syntax (Parent_Tree)
           or else (Child_Tree /= "" and then Child_Tree /= "node"
                    and then not Grammar.Have_Syntax (Child_Tree))
         then
            return;
         end if;

         if Index > 0
           and then (Position_Name = "before"
                     or else Position_Name = "after")
         then

            if Child_Tree = "node" then
               Start_Binding
                 (Parent_Tree      => Parent_Tree,
                  Child_Tree       => Child_Tree,
                  Parent_Link_Name => Class.Link_Name,
                  Child_Link_Name  => "",
                  Position         => Position);

               Ada.Text_IO.Put_Line
                 (Action_File,
                  "   call "
                  & "tree." & Group_Property & "."
                  & Class.Link_Name
                  & "."
                  & Position_Name & "_node"
                  & "(tree." & Group_Property & ")");
            else
               declare
                  Child_Argument : constant Entity_Type :=
                                     Feature.Argument (1);
                  Child_Type     : constant Entity_Type :=
                                     Child_Argument.Get_Type;
               begin

                  Start_Binding
                    (Parent_Tree      => Parent_Tree,
                     Child_Tree       => Child_Tree,
                     Parent_Link_Name => Class.Link_Name,
                     Child_Link_Name  => Child_Type.Link_Name,
                     Position         => Position);

                  if Child_Type.Standard_Name = "string" then
                     if False then
                        Ada.Text_IO.Put_Line
                          (Action_File,
                           "   IO.put_line (""" & Parent_Tree
                           & "/" & Child_Tree
                           & " -> "" & child.text.to_lower)");
                     end if;

                     Ada.Text_IO.Put_Line
                       (Action_File,
                        "   call "
                        & "tree." & Group_Property & "."
                        & Class.Link_Name
                        & "."
                        & Position_Name & "_" & Child_Tree
                        & "(tree." & Group_Property & ","
                        & "child.text);");
                  elsif Is_Group_Reference
                    (Class, Child_Argument)
                  then

                     if not Local_Classes.Contains
                       (Child_Type.Standard_Name)
                     then
                        declare
                           use Ack.Classes;
                           use Ack.Types;
                           Child_Class : constant Constant_Class_Entity :=
                                           Type_Entity (Child_Type).Class;
                        begin
                           Local_Classes.Insert
                             (Child_Class.Standard_Name, Child_Class);
                        end;
                     end if;

                     if Report_Calls then
                        Ada.Text_IO.Put_Line
                          (Action_File,
                           "   IO.put_line (""Before call: "
                           & Position_Name & " "
                           & Parent_Tree
                           & "/" & Child_Tree
                           & " -> "" & child."
                           & Child_Type.Link_Name
                           & ".Image)");
                     end if;

                     if Position_Name = "before" then
                        declare
                           procedure Call_With_Conforming_Child
                             (Ancestor_Class : not null access constant
                                Ack.Classes.Class_Entity_Record'Class;
                              Call_Name      : String);

                           --------------------------------
                           -- Call_With_Conforming_Child --
                           --------------------------------

                           procedure Call_With_Conforming_Child
                             (Ancestor_Class : not null access constant
                                Ack.Classes.Class_Entity_Record'Class;
                              Call_Name      : String)
                           is
                           begin
                              Ada.Text_IO.Put_Line
                                (Action_File,
                                 "   call "
                                 & "tree." & Group_Property & "."
                                 & Ancestor_Class.Link_Name
                                 & "."
                                 & Call_Name
                                 & "(tree." & Group_Property & ","
                                 & "child." & Child_Type.Link_Name & ");");
                           end Call_With_Conforming_Child;

                        begin
                           Class.Scan_Conforming_Child_Ancestors
                             (Child     =>
                                Ack.Types.Type_Entity (Child_Type).Class,
                              Process   =>
                                Call_With_Conforming_Child'Access);
                        end;
                     end if;

                     Ada.Text_IO.Put_Line
                       (Action_File,
                        "   call "
                        & "tree." & Group_Property & "."
                        & Class.Link_Name
                        & "."
                        & Position_Name & "_" & Child_Tree
                        & "(tree." & Group_Property & ","
                        & "child." & Child_Type.Link_Name & ");");

                     if Report_Calls then
                        Ada.Text_IO.Put_Line
                          (Action_File,
                           "   IO.put_line (""After Call: "
                           & Position_Name & " "
                           & Parent_Tree
                           & "/" & Child_Tree
                           & " -> "" & child."
                           & Child_Type.Link_Name
                           & ".Image)");
                     end if;

                  else
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "warning: "
                        & Feature.Full_Name & ": cannot bind argument "
                        & Child_Argument.Declared_Name
                        & " of type "
                        & Child_Type.Full_Name);
                  end if;
               end;
            end if;

            Finish_Binding;

         end if;
      end Add_Feature_Binding;

      ---------------------
      -- Check_Allocated --
      ---------------------

      procedure Check_Allocated
        (Class     : String;
         Tree_Name : String)
      is
      begin
         Ada.Text_IO.Put_Line
           (Action_File,
            "   if not " & Tree_Name & "." & Class & " then");
         Ada.Text_IO.Put_Line
           (Action_File,
            "      " & Tree_Name & "." & Class & " :=");
         Ada.Text_IO.Put_Line
           (Action_File,
            "        "
            & Class
            & "$allocate");
         if Report_Calls then
            Ada.Text_IO.Put_Line
              (Action_File,
               "      IO.Put_Line (""Allocated " & Tree_Name & ": "" & "
               & Tree_Name & "." & Class & ".Image)");
         end if;

         Ada.Text_IO.Put_Line
           (Action_File,
            "   end if");
      end Check_Allocated;

      --------------------
      -- Check_Bindings --
      --------------------

      procedure Check_Bindings
        (Tree_Name : String)
      is
         Tree     : constant Aquarius.Syntax.Syntax_Tree :=
                      Grammar.Get_Syntax (Tree_Name);
         Children : constant Aquarius.Trees.Array_Of_Trees :=
                      Tree.Get_Named_Children;
      begin
         if Local_Classes.Contains (Tree_Name) then
            for Child of Children loop
               if not Ack.Bindings.Actions.Have_Binding
                 (Table            => Binding_Table,
                  Parent_Tree_Name => Tree_Name,
                  Child_Tree_Name  => Child.Name,
                  Position         => Before)
               then
                  Ack.Bindings.Actions.Create_Binding
                    (Table            => Binding_Table,
                     Parent_Tree_Name => Tree_Name,
                     Child_Tree_Name  => Child.Name,
                     Position         => Before);

                  declare
                     use all type Aquarius.Syntax.Node_Class;
                     Syntax : constant Aquarius.Syntax.Syntax_Tree :=
                                Aquarius.Syntax.Syntax_Tree (Child);
                  begin
                     if Syntax.Syntax_Class in Choice | Non_Terminal
                       and then Local_Classes.Contains (Child.Name)
                     then
                        Check_Conforming_Children (Tree_Name, Child.Name);
                     end if;
                  end;
               end if;
            end loop;
         end if;
      end Check_Bindings;

      -------------------------------
      -- Check_Conforming_Children --
      -------------------------------

      procedure Check_Conforming_Children
        (Parent_Name : String;
         Child_Name  : String)
      is
         use Ack.Classes;
         Parent_Class : constant Constant_Class_Entity :=
                          Local_Classes.Element (Parent_Name);
         Child_Class  : constant Constant_Class_Entity :=
                          Local_Classes.Element (Child_Name);

         First_Call   : Boolean := True;

         procedure Add_Call
           (Ancestor_Class : not null access constant
              Class_Entity_Record'Class;
            Call_Name      : String);

         --------------
         -- Add_Call --
         --------------

         procedure Add_Call
           (Ancestor_Class : not null access constant
              Class_Entity_Record'Class;
            Call_Name      : String)
         is
         begin
            if First_Call then
               Start_Binding
                 (Parent_Tree      => Parent_Name,
                  Child_Tree       => Child_Name,
                  Parent_Link_Name => Parent_Class.Link_Name,
                  Child_Link_Name  => Child_Class.Link_Name,
                  Position         => Before);
               First_Call := False;
            end if;

            if Report_Implicit_Calls then
               Ada.Text_IO.Put_Line
                 (Action_File,
                  "   IO.Put_Line ("""
                  & Call_Name
                  & ": "
                  & Parent_Class.Standard_Name
                  & "/"
                  & Child_Class.Standard_Name
                  & """)");
            end if;

            Ada.Text_IO.Put_Line
              (Action_File,
               "   call "
               & "tree." & Parent_Class.Link_Name & "."
               & Ancestor_Class.Link_Name
               & "."
               & Call_Name
               & "(tree." & Parent_Class.Link_Name & ","
               & "child." & Child_Class.Link_Name & ");");

         end Add_Call;

      begin

         Parent_Class.Scan_Conforming_Child_Ancestors
           (Child_Class, Add_Call'Access);

         if not First_Call then
            Finish_Binding;
         end if;

      end Check_Conforming_Children;

      --------------------
      -- Finish_Binding --
      --------------------

      procedure Finish_Binding is
      begin
         Ada.Text_IO.Put_Line
           (Action_File, "end;");
         Ada.Text_IO.New_Line (Action_File);
      end Finish_Binding;

      ------------------------
      -- Is_Group_Reference --
      ------------------------

      function Is_Group_Reference
        (Class    : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Property : not null access constant
           Root_Entity_Type'Class)
         return Boolean
      is
         use Ada.Strings.Fixed;
         Property_Type : constant Entity_Type := Property.Get_Type;
         Property_Link_Name : constant String := Property_Type.Link_Name;
         Class_Link_Name    : constant String := Class.Link_Name;
         Property_Top_Name  : constant String :=
                                Property_Link_Name
                                  (Property_Link_Name'First
                                   .. Index (Property_Link_Name, "__"));
         Class_Top_Name     : constant String :=
                                Class_Link_Name
                                  (Class_Link_Name'First
                                   .. Index (Class_Link_Name, "__"));
         Count              : Natural := 0;
      begin
         for I in Property_Link_Name'First .. Property_Link_Name'Last - 1 loop
            if Property_Link_Name (I .. I + 1) = "__" then
               Count := Count + 1;
            end if;
         end loop;

         return Count = 2
           and then Property_Link_Name /= Class_Link_Name
           and then Property_Top_Name = Class_Top_Name;
      end Is_Group_Reference;

      ----------------
      -- Load_Class --
      ----------------

      procedure Load_Class
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         Source_Path : constant String :=
                         Ada.Directories.Full_Name (Directory_Entry);
         Source_Name : constant String :=
                         Ada.Directories.Base_Name
                           (Source_Path);
         Match_Name  : constant String :=
                         Grammar.Name
                         & "-"
                         & Aquarius.Actions.Action_Group_Name (Group)
                       & "-";
         Tree_Name   : constant String :=
                         (if Source_Name'Length > Match_Name'Length
                          and then Source_Name (1 .. Match_Name'Length)
                          = Match_Name
                          then Source_Name (Match_Name'Length + 1
                            .. Source_Name'Last)
                            else "");
      begin
         References.Clear;
         if Tree_Name /= "" then
            Ack.Bindings.Actions.Add_Tree (Binding_Table, Tree_Name);
         end if;

         Ack.Compile.Compile_Class
           (Source_Path, Image,
            Add_Feature_Binding'Access);
      end Load_Class;

      -------------------
      -- Start_Binding --
      -------------------

      procedure Start_Binding
        (Parent_Tree      : String;
         Child_Tree       : String;
         Parent_Link_Name : String;
         Child_Link_Name  : String;
         Position         : Binding_Position)
      is
         Position_Name : constant String :=
                           (case Position is
                               when Before => "before",
                               when After  => "after");
      begin
         if Child_Tree = "node" then
            Ada.Text_IO.Put_Line
              (Action_File,
               Position_Name & " " & Parent_Tree & " do");
         else
            Ada.Text_IO.Put_Line
              (Action_File,
               Position_Name & " " & Parent_Tree
               & "/" & Child_Tree & " do");

            Ack.Bindings.Actions.Create_Binding
              (Table            => Binding_Table,
               Parent_Tree_Name => Parent_Tree,
               Child_Tree_Name  => Child_Tree,
               Position         => Position);

         end if;

         Check_Allocated (Parent_Link_Name, "tree");

         for Reference of References loop
            Ada.Text_IO.Put_Line
              (Action_File,
               "   if tree." & Reference.Get_Type.Link_Name & " then");
            Ada.Text_IO.Put_Line
              (Action_File,
               "      tree." & Parent_Link_Name
               & "." & Parent_Link_Name
               & "." & Reference.Standard_Name & " :=");
            Ada.Text_IO.Put_Line
              (Action_File,
               "        tree." & Reference.Get_Type.Link_Name);
            Ada.Text_IO.Put_Line
              (Action_File,
               "   end if");
         end loop;

         Ada.Text_IO.Put_Line
           (Action_File,
            "   if tree." & Parent_Link_Name
            & ".aquarius__trees__program_tree then");
         Ada.Text_IO.Put_Line
           (Action_File,
            "      tree." & Parent_Link_Name
            & ".aquarius__trees__program_tree := tree");
         Ada.Text_IO.Put_Line
           (Action_File, "   end if");

         if Child_Tree /= "node"
           and then Child_Link_Name /= "string"
         then
            Check_Allocated
              (Child_Link_Name, "child");
            Ada.Text_IO.Put_Line
              (Action_File,
               "   if child." & Child_Link_Name
               & ".aquarius__trees__program_tree then");
            Ada.Text_IO.Put_Line
              (Action_File,
               "      child." & Child_Link_Name
               & ".aquarius__trees__program_tree := child");
            Ada.Text_IO.Put_Line
              (Action_File, "   end if");

         end if;

      end Start_Binding;

      Group_Name : constant String :=
                     Aquarius.Actions.Action_Group_Name (Group);

   begin
      Ada.Text_IO.Create (Action_File, Ada.Text_IO.Out_File,
                          Action_File_Path);

      Ada.Directories.Search
        (Base_Aqua_Path,
         Grammar.Name & "-" & Group_Name & "*.aqua",
         Process => Load_Class'Access);

      Ack.Bindings.Actions.Scan_Trees
        (Binding_Table, Check_Bindings'Access);

      Ada.Text_IO.Close (Action_File);

      return not Ack.Errors.Has_Errors;

--        if not Ack.Errors.Has_Errors then
--           Load_Action_File
--             (Full_Path => Action_File_Path,
--              Group     => Group,
--              Image     => Image);
--        end if;

   end Load_Ack_Binding;

end Ack.Bindings;
