with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with Aquarius.Errors;
with Aquarius.Source;

package body Aquarius.Actions.Scanner is

   use Aquarius.Programs;

   No_Arguments : Array_Of_Program_Trees (1 .. 0);

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   procedure Scan
     (Processor : in out Action_Processor_Interface'Class;
      Action    : in Aquarius.Programs.Program_Tree);

   procedure Scan_Sequence
     (Processor : in out Action_Processor_Interface'Class;
      Sequence  : in Aquarius.Programs.Array_Of_Program_Trees);

   procedure Scan_Aggregate
     (Processor : in out Action_Processor_Interface'Class;
      Elements  : in Aquarius.Programs.Array_Of_Program_Trees);

   procedure Scan_Array_Aggregate
     (Processor : in out Action_Processor_Interface'Class;
      Elements  : in Aquarius.Programs.Array_Of_Program_Trees);

   procedure Scan_Action_Binding
     (Processor : in out Action_Processor_Interface'Class;
      Header    : Aquarius.Programs.Program_Tree;
      Definition : Aquarius.Programs.Program_Tree);

   procedure Scan_Object_Reference
     (Processor   : in out Action_Processor_Interface'Class;
      Reference   : in Aquarius.Programs.Program_Tree;
      Destination : Boolean := False);

   procedure Write_Position
     (Processor  : in out Action_Processor_Interface'Class;
      Action     : Aquarius.Programs.Program_Tree);

   procedure With_Package
     (Processor  : in out Action_Processor_Interface'Class;
      Name       : String)
   is null;

   ---------------------
   -- Add_Frame_Entry --
   ---------------------

   procedure Add_Frame_Entry
     (Processor : in out Action_Processor_Interface'Class;
      Name      : String;
      Offset    : Integer)
   is
   begin
      Processor.Frame_Table.Insert
        (Name, (Stack_Offset, Offset));
   exception
      when E : others =>
         raise Constraint_Error with
           "unable to add " & Name & " to frame table: "
             & Ada.Exceptions.Exception_Message (E);
   end Add_Frame_Entry;

   ---------------------
   -- Add_Frame_Entry --
   ---------------------

   procedure Add_Frame_Entry
     (Processor     : in out Action_Processor_Interface'Class;
      Name          : String;
      Internal_Name : String)
   is
   begin
      Processor.Frame_Table.Insert
        (Name,
         (Register_Name,
          Ada.Strings.Unbounded.To_Unbounded_String (Internal_Name)));
   exception
      when E : others =>
         raise Constraint_Error with
           "unabled to add " & Name & " to frame table: "
           & Ada.Exceptions.Exception_Message (E);
   end Add_Frame_Entry;

   ----------------------
   -- Add_Global_Entry --
   ----------------------

   procedure Add_Global_Entry
     (Processor : in out Action_Processor_Interface'Class;
      Name      : String;
      Immediate : Boolean)
   is
   begin
      Processor.External_Table.Insert
        (Name,
         (Is_Immediate => Immediate,
          Is_Function  => False,
          External_Name => Ada.Strings.Unbounded.To_Unbounded_String (Name)));
   end Add_Global_Entry;

   -------------------------
   -- Add_Global_Function --
   -------------------------

   procedure Add_Global_Function
     (Processor : in out Action_Processor_Interface'Class;
      Name      : String)
   is
   begin
      Processor.External_Table.Insert
        (Name,
         (Is_Immediate => False,
          Is_Function  => True,
          External_Name => Ada.Strings.Unbounded.To_Unbounded_String (Name)));
   end Add_Global_Function;

   -----------------------------
   -- Current_Source_Location --
   -----------------------------

   procedure Current_Source_Location
     (Processor      : in out Action_Processor_Interface'Class;
      Line           : in Natural;
      Column         : in Natural)
   is
   begin
      if Line /= Processor.Last_Line
        or else Column /= Processor.Last_Col
      then
         Processor.Put_Source_Location (Line, Column);
         Processor.Last_Line := Line;
         Processor.Last_Col := Column;
      end if;
   end Current_Source_Location;

   ------------------
   -- Delete_Frame --
   ------------------

   procedure Delete_Frame
     (Processor : in out Action_Processor_Interface'Class)
   is
   begin
      Processor.Frame_Table.Clear;
   end Delete_Frame;

   ------------------------
   -- Delete_Frame_Entry --
   ------------------------

   procedure Delete_Frame_Entry
     (Processor : in out Action_Processor_Interface'Class;
      Name      : String)
   is
   begin
      Processor.Frame_Table.Delete (Name);
   end Delete_Frame_Entry;

   --------------------
   -- Frame_Contains --
   --------------------

   function Frame_Contains
     (Processor : Action_Processor_Interface'Class;
      Name      : String)
      return Boolean
   is
   begin
      return Processor.Frame_Table.Contains (Name);
   end Frame_Contains;

   ---------------------------------
   -- Global_Environment_Contains --
   ---------------------------------

   function Global_Environment_Contains
     (Processor : Action_Processor_Interface'Class;
      Name      : String)
      return Boolean
   is
   begin
      return Processor.External_Table.Contains (Name);
   end Global_Environment_Contains;

   -----------
   -- Group --
   -----------

   function Group (Processor : Action_Processor_Interface'Class)
                   return Action_Group
   is
   begin
      return Processor.Group;
   end Group;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Processor : in out Action_Processor_Interface'Class;
      Action    : in Aquarius.Programs.Program_Tree)
   is
   begin

      Write_Position (Processor, Action);

      if Action.Name = "top_level" then
         Scan (Processor, Action.Program_Child ("compilation_unit"));
      elsif Action.Name = "compilation_unit" then
         Scan (Processor,
               Action.Program_Child ("sequence_of_top_level_declarations"));
      elsif Action.Name = "sequence_of_top_level_declarations" then
         Scan_Sequence
           (Processor, Action.Direct_Children ("top_level_declaration"));
      elsif Action.Name = "top_level_declaration" then
         Scan (Processor, Action.Chosen_Tree);
      elsif Action.Name = "with_declaration" then
         declare
            Packages : constant Array_Of_Program_Trees :=
                         Action.Direct_Children ("identifier");
         begin
            for Package_Terminal of Packages loop
               With_Package (Processor, Package_Terminal.Text);
            end loop;
         end;
      elsif Action.Name = "list_of_local_declarations" then
         Scan_Sequence
           (Processor, Action.Direct_Children ("local_declaration"));
      elsif Action.Name = "local_declaration" then
         declare
            List : constant Program_Tree :=
                     Action.Program_Child ("local_variable_list");
         begin
            Processor.Declare_Local_Variables
              (Names        => List.Direct_Children ("identifier"),
               Inital_Value => Action.Program_Child ("expression"));
         end;
      elsif Action.Name = "sequence_of_statements" then
         declare
            Children : constant Array_Of_Program_Trees :=
                         Action.Direct_Children ("statement");
         begin
            Scan_Sequence (Processor, Children);
         end;
      elsif Action.Name = "statement" then
         declare
            Child : constant Program_Tree :=
                      Action.Chosen_Tree;
         begin
            Scan (Processor, Child);
         end;
      elsif Action.Name = "action_declaration" then
         Scan_Action_Binding
           (Processor, Action.Program_Child ("action_header"),
            Action.Program_Child ("action_definition"));
      elsif Action.Name = "function_declaration" then
         declare
            Name : constant String :=
                     Action.Program_Child ("identifier").Text;
            Argument_Tree : constant Program_Tree :=
                              Action.Program_Child ("arguments");
            Arguments     : constant Array_Of_Program_Trees :=
                              (if Argument_Tree = null
                               then Empty_Program_Tree_Array
                               else Argument_Tree.Direct_Children
                                 ("identifier"));
            Locals        : constant Program_Tree :=
                              Action.Program_Child
                                ("list_of_local_declarations");

         begin
            Processor.Add_Global_Function (Name);
            Processor.Start_Function
              (Name      => Name,
               Arguments => Arguments,
               Locals    => Locals.Direct_Children ("local_declaration"));
            Scan (Processor,
                  Action.Program_Child ("list_of_local_declarations"));
            Scan (Processor,
                  Action.Program_Child ("sequence_of_statements"));
            Processor.End_Function;
         end;

      elsif Action.Name = "null_statement" then
         null;
      elsif Action.Name = "procedure_call_statement" then
         Scan_Object_Reference (Processor,
                                Action.Program_Child ("object_reference"));
         Processor.Clear_Result;
      elsif Action.Name = "for_loop_statement" then
         Scan (Processor, Action.Program_Child ("iterator_loop"));
      elsif Action.Name = "iterator_loop" then
         declare
            Identifier      : constant Program_Tree :=
                                Action.Program_Child ("identifier");
            Identifier_Name : constant String :=
                                Identifier.Text;
            Object          : constant Program_Tree :=
                                Action.Program_Child ("object_reference");
            Loop_Statement  : constant Program_Tree :=
                                Action.Program_Child ("loop_statement");
            Statements      : constant Program_Tree :=
                                Loop_Statement.Program_Child
                                  ("sequence_of_statements");
         begin
            Scan_Object_Reference (Processor, Object);
            Processor.Iterator_Statement
              (Identifier => Identifier_Name,
               Statements => Statements);
         end;
      elsif Action.Name = "if_statement" then
         declare
            Children : constant Aquarius.Programs.Array_Of_Program_Trees :=
                         Action.Direct_Children;
            Expressions : Array_Of_Program_Trees (Children'Range);
            Statements  : Array_Of_Program_Trees (Children'Range);
            Expression_Count : Natural := 0;
            Statement_Count  : Natural := 0;
         begin
            for T of Children loop
               if T.Name = "expression" then
                  Expression_Count := Expression_Count + 1;
                  Expressions (Expression_Count) := T;
               elsif T.Name = "sequence_of_statements" then
                  Statement_Count := Statement_Count + 1;
                  Statements (Statement_Count) := T;
               end if;
            end loop;
            pragma Assert (Statements'Length in
                             Expressions'Length .. Expressions'Length + 1);

            Processor.If_Statement
              (Expressions (1 .. Expression_Count),
               Statements (1 .. Statement_Count));

         end;
      elsif Action.Name = "assignment_statement" then
         Scan_Expression (Processor,  Action.Program_Child ("expression"));
         Scan_Object_Reference
           (Processor,
            Action.Program_Child ("object_reference"),
            Destination => True);
      elsif Action.Name = "return_statement" then
         Scan_Expression (Processor,  Action.Program_Child ("expression"));
         Processor.Pop_Return_Value;
      else
         raise Constraint_Error with
           "unable to understand action: " & Action.Name
           & ": " & Action.Image;
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Action.Show_Location & ": " & Ada.Exceptions.Exception_Name (E)
            & " " & Ada.Exceptions.Exception_Message (E));
         raise;
   end Scan;

   -----------------
   -- Scan_Action --
   -----------------

   procedure Scan_Action
     (Processor : in out Action_Processor_Interface'Class;
      Action    : in Aquarius.Programs.Program_Tree)
   is
   begin
      Scan (Processor, Action);
   end Scan_Action;

   -------------------------
   -- Scan_Action_Binding --
   -------------------------

   procedure Scan_Action_Binding
     (Processor : in out Action_Processor_Interface'Class;
      Header    : Aquarius.Programs.Program_Tree;
      Definition : Aquarius.Programs.Program_Tree)
   is
      Action_Time       : constant Rule_Position :=
                            Rule_Position'Value
                              (Header.Program_Child
                                 ("action_time").Chosen_Tree.Name);
      Contexts          : constant Array_Of_Program_Trees :=
                            Header.Direct_Children ("action_context");
      Action_Binding    : constant Program_Tree :=
                            Definition.Program_Child
                              ("sequence_of_statements");
   begin
      for Context of Contexts loop
         declare
            Names       : constant Array_Of_Program_Trees :=
                            Context.Direct_Children;
            Parent_Name       : constant String :=
                                  Names (Names'First).Text;
            Child_Name        : constant String :=
                                  (if Names'Length > 1
                                   then Names (Names'First + 1).Text
                                   else "");
         begin
            Processor.Action_Header
              (Position => Action_Time,
               Parent   => Parent_Name,
               Child    => Child_Name);
         end;
      end loop;

      Processor.Start_Action_Body;
      Scan_Sequence
        (Processor, Action_Binding.Direct_Children ("statement"));
      Processor.End_Action_Body;
      Processor.Frame_Table.Clear;

   end Scan_Action_Binding;

   ------------------
   -- Scan_Actions --
   ------------------

   procedure Scan_Actions
     (Processor : in out Action_Processor_Interface'Class;
      Top       : Aquarius.Programs.Program_Tree;
      Group     : Action_Group)
   is
   begin
      Processor.Top := Top;
      Processor.Group := Group;

      Processor.Start_Process (Top.Source_File_Name,
                               Action_Group_Name (Group));
      Processor.Source_File
        (Top.Source_Directory,
         Top.Source_File_Name);

      Scan (Processor, Top);
      Processor.End_Process;
   end Scan_Actions;

   --------------------
   -- Scan_Aggregate --
   --------------------

   procedure Scan_Aggregate
     (Processor : in out Action_Processor_Interface'Class;
      Elements  : in Aquarius.Programs.Array_Of_Program_Trees)
   is
   begin
      Processor.Start_Aggregate ("map");
      for E of Elements loop
         declare
            Name : constant String :=
                     E.Program_Child ("identifier").Text;
         begin
            Processor.Start_Aggregate_Element (Name);
            Scan_Expression (Processor, E.Program_Child ("expression"));
            Processor.End_Aggregate_Element (Name);
         end;
      end loop;
      Processor.End_Aggregate;
   end Scan_Aggregate;

   --------------------------
   -- Scan_Array_Aggregate --
   --------------------------

   procedure Scan_Array_Aggregate
     (Processor : in out Action_Processor_Interface'Class;
      Elements  : in Aquarius.Programs.Array_Of_Program_Trees)
   is
   begin
      Processor.Start_Aggregate ("array");
      for E of Elements loop
         Processor.Start_Aggregate_Element ("");
         Scan_Expression (Processor, E);
         Processor.End_Aggregate_Element ("");
      end loop;
      Processor.End_Aggregate;
   end Scan_Array_Aggregate;

   ---------------------
   -- Scan_Expression --
   ---------------------

   procedure Scan_Expression
     (Processor  : in out Action_Processor_Interface'Class;
      Expression : in Aquarius.Programs.Program_Tree)
   is
   begin
      if Expression.Name = "expression"
        or else Expression.Name = "relation"
        or else Expression.Name = "simple_expression"
        or else Expression.Name = "term"
      then
         declare
            Children : constant Array_Of_Program_Trees :=
                         Expression.Direct_Children;
            Operator : Program_Tree;
            Index    : Positive := Children'First;
         begin
            Scan_Expression (Processor, Children (Index));
            Index := Index + 1;
            while Index <= Children'Last loop
               Operator := Program_Tree (Children (Index).First_Leaf);
               Index := Index + 1;
               Scan_Expression (Processor, Children (Index));
               Index := Index + 1;
               Processor.Operator (Operator.Text);
            end loop;
         end;
      elsif Expression.Name = "factor" then
         declare
            Operator : constant Program_Tree :=
                         Expression.Program_Child
                           ("unary_primary_operator");
            Primary  : constant Program_Tree :=
                         Expression.Program_Child
                           ("primary");
         begin
            Scan_Expression (Processor, Primary);
            if Operator /= null then
               Processor.Operator (Operator.Chosen_Tree.Name);
            end if;
         end;
      elsif Expression.Name = "primary" then
         declare
            Child : constant Program_Tree := Expression.Chosen_Tree;
         begin
            if Child.Name = "object_reference" then
               Scan_Object_Reference (Processor, Child);
            elsif Child.Name = "numeric_literal" then
               Processor.Literal_Number (Integer'Value (Child.Text));
            elsif Child.Name = "string_literal" then
               declare
                  Raw_Text : constant String := Child.Text;
                  String_Text : String (Raw_Text'Range);
                  Count       : Natural := String_Text'First - 1;
                  Index       : Positive := Raw_Text'First + 1;
               begin
                  --  Remove surrounding quotes, and convert two double quotes
                  --  into one.
                  while Index < Raw_Text'Last loop
                     if Index < Raw_Text'Last - 1
                       and then Raw_Text (Index) = '"'
                       and then Raw_Text (Index + 1) = '"'
                     then
                        Index := Index + 1;
                     end if;
                     Count := Count + 1;
                     String_Text (Count) := Raw_Text (Index);
                     Index := Index + 1;
                  end loop;

                  Processor.Push_String_Literal
                    (String_Text  (String_Text'First .. Count));
               end;
            elsif Child.Name = "parenthesised_expression" then
               Scan_Expression (Processor,
                                Expression.Program_Child ("expression"));
            elsif Child.Name = "null" then
               Processor.Literal_Null;
            elsif Child.Name = "aggregate" then
               Scan_Aggregate
                 (Processor,
                  Child.Program_Child
                    ("aggregate_element_list").Direct_Children
                      ("aggregate_element"));
            elsif Child.Name = "array_aggregate" then
               Scan_Array_Aggregate
                 (Processor,
                  Child.Program_Child
                    ("expression_list").Direct_Children
                      ("expression"));
            elsif Child.Name = "new_expression" then
               Scan_Object_Reference
                 (Processor, Child.Program_Child ("object_reference"));
               Processor.Allocate;

               declare
                  Arg_Tree : constant Program_Tree :=
                               Child.Program_Child ("actual_argument_list");
                  Arguments : constant Array_Of_Program_Trees :=
                                (if Arg_Tree = null then No_Arguments
                                 else Arg_Tree.Direct_Children ("expression"));
               begin
                  if Arguments'Length > 0 then
                     for Arg of reverse Arguments loop
                        Processor.Scan_Expression (Arg);
                     end loop;
                     Processor.Get_Property ("__init__", Arguments'Length);
                  end if;
               end;
            elsif Child.Name = "if_expression" then
               declare
                  Children         : constant Array_Of_Program_Trees :=
                                       Child.Direct_Children;
                  Expressions      : Array_Of_Program_Trees (Children'Range);
                  Expression_Count : Natural := 0;
               begin
                  for T of Children loop
                     if T.Name = "expression" then
                        Expression_Count := Expression_Count + 1;
                        Expressions (Expression_Count) := T;
                     end if;
                  end loop;

                  pragma Assert (Expression_Count >= 3);
                  pragma Assert (Expression_Count mod 2 = 1);

                  Processor.If_Then_Else_Expression
                    (Expressions (1 .. Expression_Count));
               end;

            else
               raise Constraint_Error with
                 "cannot process expression primary: " & Child.Name;
            end if;
         end;
      else
         raise Constraint_Error with
           "cannot evaluate expression: " & Expression.Name;
      end if;

   end Scan_Expression;

   ---------------------------
   -- Scan_Object_Reference --
   ---------------------------

   procedure Scan_Object_Reference
     (Processor   : in out Action_Processor_Interface'Class;
      Reference   : in Aquarius.Programs.Program_Tree;
      Destination : Boolean := False)
   is
      Qs : constant Array_Of_Program_Trees :=
             Reference.Direct_Children ("name_qualifier");
      Ops : constant Array_Of_Program_Trees :=
              Reference.Direct_Children ("object_operator");
--        Q            : Program_Tree := Qs (Qs'First);
--        Id           : Program_Tree := Q.Program_Child ("identifier");
      Start_Index  : Positive := 1;

      function Component_Name (Q : Program_Tree) return String
      is (Q.Program_Child ("identifier").Text);

   begin

      if Qs'Length > 1 then
         Processor.Start_Object_Reference;
      end if;

      for I in reverse Qs'Range loop
         declare
            First     : constant Boolean := I = Qs'First;
            Last      : constant Boolean := I = Qs'Last;
            Q         : constant Program_Tree := Qs (I);
--              Component : constant String := Component_Name (Q);
            Arg_Tree  : constant Program_Tree :=
                          Q.Program_Child ("actual_argument_list");
            Op        : constant String :=
                          (if First then ""
                           else Ops (I - 1).Chosen_Tree.Text);
            Arguments : constant Array_Of_Program_Trees :=
                          (if Arg_Tree = null then No_Arguments
                           else Arg_Tree.Direct_Children ("expression"));
         begin

            if Arguments'Length > 0
              or else (not First and then Op /= ".")
            then

               if Last and then Destination
                 and then (Arguments'Length > 0
                            or else (Op /= "." and then Op /= ""))
               then
                  raise Constraint_Error
                    with "invalid target for assignment: "
                    & Reference.Concatenate_Children;
               end if;

               if Arguments'Length > 0 then
                  for Arg of reverse Arguments loop
                     Processor.Scan_Expression (Arg);
                  end loop;
               elsif Op = "." then
                  null;
               elsif Op = "/" or else Op = "^" or else Op = "|" then
                  Processor.Push_String_Literal
                    (Component_Name (Q));
               elsif not First or else Op /= "" then
                  raise Constraint_Error with
                    "cannot process qualifier: " & Op;
               end if;
            end if;
         end;
      end loop;

      for I in Qs'Range loop
         declare
            First     : constant Boolean := I = Qs'First;
            Last      : constant Boolean := I = Qs'Last;
            Q         : constant Program_Tree := Qs (I);
--              Component : constant String := Component_Name (Q);
            Arg_Tree  : constant Program_Tree :=
                          Q.Program_Child ("actual_argument_list");
            Op        : constant String :=
                          (if First then ""
                           else Ops (I - 1).Chosen_Tree.Text);
            Arguments : constant Array_Of_Program_Trees :=
                          (if Arg_Tree = null then No_Arguments
                           else Arg_Tree.Direct_Children ("expression"));
         begin

            if Last
              or else Arguments'Length > 0
              or else (not First and then Op /= ".")
            then

               for J in Start_Index .. I loop

                  declare
                     Component : constant String :=
                                   Component_Name (Qs (J));
                  begin
                     if J = Qs'First then
                        if Processor.Frame_Table.Contains (Component) then
                           declare
                              use Ada.Strings.Unbounded;
                              Start_Entry : constant Frame_Entry :=
                                              Processor.Frame_Table.Element
                                                (Component);
                           begin
                              if Destination and then Qs'Length = 1 then
                                 case Start_Entry.Entry_Type is
                                    when Stack_Offset =>
                                       Processor.Pop_Frame_Entry
                                         (Start_Entry.Offset);
                                    when Register_Name =>
                                       Processor.Pop_External_Entry
                                         (To_String (Start_Entry.Name));
                                 end case;
                              else
                                 case Start_Entry.Entry_Type is
                                    when Stack_Offset =>
                                       Processor.Push_Frame_Entry
                                         (Start_Entry.Offset);
                                    when Register_Name =>
                                       Processor.Push_External_Entry
                                         (To_String (Start_Entry.Name), False);
                                 end case;
                              end if;
                           end;
                        else
                           if not Processor.External_Table.Contains
                             (Component)
                           then
                              Processor.Declare_External_Function
                                (Component);
                              Processor.External_Table.Insert
                                (Component,
                                 (Is_Immediate => False,
                                  Is_Function  => True,
                                  External_Name =>
                                    Ada.Strings.Unbounded.To_Unbounded_String
                                      (Component)));
                           end if;

                           declare
                              Ext : constant External_Entry :=
                                      Processor.External_Table.Element
                                        (Component);
                           begin
                              if Destination and then Qs'Length = 1 then
                                 if Ext.Is_Immediate then
                                    Aquarius.Errors.Error
                                      (Qs (J),
                                       "cannot assign to immediate");
                                 elsif Ext.Is_Function then
                                    Aquarius.Errors.Error
                                      (Qs (J),
                                       "cannot assign to function");
                                 else
                                    Processor.Pop_External_Entry
                                      (Ada.Strings.Unbounded.To_String
                                         (Ext.External_Name));
                                 end if;
                              elsif Ext.Is_Function then
                                 Processor.Call_Function
                                   (Ada.Strings.Unbounded.To_String
                                      (Ext.External_Name),
                                    Arguments'Length);
                              else
                                 Processor.Push_External_Entry
                                   (Ada.Strings.Unbounded.To_String
                                      (Ext.External_Name),
                                    Immediate => Ext.Is_Immediate);
                              end if;
                           end;
                        end if;

                     else
                        declare
                           Op        : constant String :=
                                         Ops (J - 1).Chosen_Tree.Text;
                        begin
                           if Op = "." then

                              if J = Qs'Last and then Destination then
                                 Processor.Set_Property (Component);
                              elsif J = I then
                                 Processor.Get_Property
                                   (Component, Arguments'Length);
                              else
                                 Processor.Get_Property
                                   (Component, 0);
                              end if;
                           elsif Op = "/" then
                              Processor.Get_Property ("tree_child", 1);
                           elsif Op = "^" then
                              Processor.Get_Property ("tree_ancestor", 1);
                           elsif Op = "|" then
                              Processor.Get_Property
                                ("tree_inherited_property", 1);
                           else
                              raise Constraint_Error with
                                "cannot process qualifier: " & Op;
                           end if;
                        end;
                     end if;
                  end;
               end loop;

               Start_Index := I + 1;

            end if;

         end;
      end loop;

      if Qs'Length > 1 then
         Processor.Finish_Object_Reference;
      end if;

   end Scan_Object_Reference;

   -------------------
   -- Scan_Sequence --
   -------------------

   procedure Scan_Sequence
     (Processor : in out Action_Processor_Interface'Class;
      Sequence  : in Aquarius.Programs.Array_Of_Program_Trees)
   is
   begin
      for Item of Sequence loop
         Write_Position (Processor, Item);
         Scan (Processor, Item);
      end loop;
   end Scan_Sequence;

   ----------------------
   -- Source_Base_Name --
   ----------------------

   function Source_Base_Name
     (Processor : Action_Processor_Interface'Class)
      return String
   is
   begin
      return Ada.Directories.Base_Name
        (Processor.Top.Source_File_Name);
   end Source_Base_Name;

   -----------------
   -- Source_Path --
   -----------------

   function Source_Path
     (Processor : Action_Processor_Interface'Class)
      return String
   is
   begin
      return Aquarius.Source.Get_Full_Path (Processor.Top.Source);
   end Source_Path;

   ------------------------
   -- Source_Simple_Name --
   ------------------------

   function Source_Simple_Name
     (Processor : Action_Processor_Interface'Class)
      return String
   is
   begin
      return Processor.Top.Source_File_Name;
   end Source_Simple_Name;

   --------------------
   -- Write_Position --
   --------------------

   procedure Write_Position
     (Processor  : in out Action_Processor_Interface'Class;
      Action     : Aquarius.Programs.Program_Tree)
   is
      use type Aquarius.Source.Source_File;
   begin
      if Action.Source /= Aquarius.Source.No_Source_File then
         Processor.Current_Source_Location
           (Action.Location_Line,
            Action.Location_Column);
      end if;
   end Write_Position;

end Aquarius.Actions.Scanner;
