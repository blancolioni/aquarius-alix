with Aquarius.Source;

package body Aquarius.Actions.Scanner is

   use Aquarius.Programs;

   procedure Scan
     (Processor : in out Action_Processor_Interface'Class;
      Action    : in Aquarius.Programs.Program_Tree);

   procedure Scan_Sequence
     (Processor : in out Action_Processor_Interface'Class;
      Sequence  : in Aquarius.Programs.Array_Of_Program_Trees);

   procedure Scan_Aggregate
     (Processor : in out Action_Processor_Interface'Class;
      Elements  : in Aquarius.Programs.Array_Of_Program_Trees);

   procedure Scan_Action_Binding
     (Processor : in out Action_Processor_Interface'Class;
      Header    : Aquarius.Programs.Program_Tree;
      Definition : Aquarius.Programs.Program_Tree);

   procedure Scan_Object_Reference
     (Processor : in out Action_Processor_Interface'Class;
      Reference : in Aquarius.Programs.Program_Tree;
      Context   : in Object_Reference_Context);

   procedure Write_Position
     (Processor  : in out Action_Processor_Interface'Class;
      Action     : Aquarius.Programs.Program_Tree);

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
               Action.Program_Child ("sequence_of_statements"));
      elsif Action.Name = "sequence_of_statements" then
         declare
            Children : constant Array_Of_Program_Trees :=
                         Action.Direct_Children;
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
      elsif Action.Name = "action_statement" then
         Scan_Action_Binding
           (Processor, Action.Program_Child ("action_header"),
            Action.Program_Child ("action_definition"));
      elsif Action.Name = "null_statement" then
         null;
      elsif Action.Name = "new_statement" then
         Scan_Object_Reference (Processor,
                                Action.Program_Child ("object_reference"),
                                Allocation);
      elsif Action.Name = "procedure_call_statement" then
         Scan_Object_Reference (Processor,
                                Action.Program_Child ("object_reference"),
                                Call);
      elsif Action.Name = "for_loop_statement" then
         Scan (Processor, Action.Program_Child ("iterator_loop"));
      elsif Action.Name = "iterator_loop" then
         declare
--              Identifier : constant Program_Tree :=
--                             Action.Program_Child ("identifier");
            Identifier_Name : constant String := "";
--                                  (if Identifier /= null
--                                   then Identifier.Text
--                                   else "");
            Object          : constant Program_Tree :=
                                Action.Program_Child ("object_reference");
            Loop_Statement  : constant Program_Tree :=
                                Action.Program_Child ("loop_statement");
            Statements      : constant Program_Tree :=
                                Loop_Statement.Program_Child
                                  ("sequence_of_statements");
         begin
            Scan_Object_Reference (Processor, Object, Evaluation);
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
         Scan_Object_Reference
           (Processor,
            Action.Program_Child ("object_reference"),
            Assignment_Target);
         Scan_Expression (Processor,  Action.Program_Child ("expression"));
         Processor.Assign;
      end if;
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
      Scan_Sequence (Processor, Action_Binding.Direct_Children);
      Processor.End_Action_Body;

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
      Processor.Start_Process (Top, Group);
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
      Processor.Start_Aggregate;
      for E of Elements loop
         Processor.Start_Aggregate_Element
           (E.Program_Child ("identifier").Text);
         Scan_Expression (Processor, E.Program_Child ("expression"));
         Processor.End_Aggregate_Element;
      end loop;
      Processor.End_Aggregate;
   end Scan_Aggregate;

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
               Scan_Object_Reference (Processor, Child, Evaluation);
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

                  Processor.Literal_String
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
     (Processor : in out Action_Processor_Interface'Class;
      Reference : in Aquarius.Programs.Program_Tree;
      Context   : in Object_Reference_Context)
   is
      Qs : constant Array_Of_Program_Trees :=
             Reference.Direct_Children ("name_qualifier");
      Ops : constant Array_Of_Program_Trees :=
              Reference.Direct_Children ("object_operator");
      No_Arguments : Array_Of_Program_Trees (1 .. 0);
      Q            : Program_Tree := Qs (Qs'First);
      Id           : Program_Tree := Q.Program_Child ("identifier");
      Args         : Program_Tree := Q.Program_Child ("actual_argument_list");
   begin
      Processor.Start_Object_Reference
        (Context    => Context,
         Identifier => Id.Text,
         Arguments  => (if Args /= null
                        then Args.Direct_Children ("expression")
                        else No_Arguments),
         Last       => Qs'Length = 1);

      for I in Qs'First + 1 .. Qs'Last loop
         Q := Qs (I);
         Id := Q.Program_Child ("identifier");
         Args := Q.Program_Child ("actual_argument_list");

         declare
            Component : constant String := Id.Text;
            Last      : constant Boolean := I = Qs'Last;
            Op        : constant String := Ops (I - 1).Chosen_Tree.Text;
         begin
            if Op = "." then
               Processor.Component_Selector
                 (Identifier => Id.Text,
                  Arguments  => (if Args /= null
                                 then Args.Direct_Children ("expression")
                                 else No_Arguments),
                  Last       => I = Qs'Last);
            elsif Op = "/" then
               Processor.Subtree_Selector (Component, Last);
            elsif Op = "^" then
               Processor.Ancestor_Selector (Component, Last);
            else
               raise Constraint_Error with
                 "cannot process qualifier: " & Op;
            end if;
         end;
      end loop;

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
