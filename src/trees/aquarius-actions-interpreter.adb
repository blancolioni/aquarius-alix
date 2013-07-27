with Ada.Text_IO;

package body Aquarius.Actions.Interpreter is

   use Aquarius.Programs;

   procedure Interpret (Action : Aquarius.Programs.Program_Tree;
                        Node   : Aquarius.Programs.Program_Tree);

   procedure Interpret_Case_Options
     (Value   : Aquarius.Programs.Program_Tree;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree);

   procedure Interpret_If_Statement
     (Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree);

   procedure Scan (Action : Aquarius.Programs.Program_Tree;
                   Top    : Aquarius.Programs.Program_Tree);

   function Evaluate
     (Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Aquarius.Programs.Program_Tree;

   function Evaluate_Object_Reference
     (Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Aquarius.Programs.Program_Tree;

   function Get_Tree
     (Context : Program_Tree;
      Name    : String)
      return Program_Tree;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Aquarius.Programs.Program_Tree
   is
   begin
      if Action.Name = "expression" then
         return Evaluate (Action.Program_Child ("relation"), Node);
      elsif Action.Name = "relation" then
         return Evaluate (Action.Program_Child ("simple_expression"), Node);
      elsif Action.Name = "simple_expression" then
         return Evaluate (Action.Program_Child ("term"), Node);
      elsif Action.Name = "term" then
         return Evaluate (Action.Program_Child ("factor"), Node);
      elsif Action.Name = "factor" then
         return Evaluate (Action.Program_Child ("primary"), Node);
      elsif Action.Name = "primary" then
         declare
            Child : constant Program_Tree := Action.Chosen_Tree;
         begin
            if Child.Name = "object_reference" then
               return Evaluate_Object_Reference (Child.Chosen_Tree, Node);
            elsif Child.Name = "numeric_literal" then
               return Child;
            else
               return Child;
            end if;
         end;
      else
         Ada.Text_IO.Put_Line
           ("cannot evaluate expression: " & Action.Name);
         return Node;
      end if;
   end Evaluate;

   -------------------------------
   -- Evaluate_Object_Reference --
   -------------------------------

   function Evaluate_Object_Reference
     (Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Aquarius.Programs.Program_Tree
   is
      Start      : constant Program_Tree :=
                     (if Action.Name = "explicit_object_reference"
                      then Get_Tree
                        (Node,
                         Action.Program_Child ("identifier").Standard_Text)
                      else Node);
      Qualifiers : constant Array_Of_Program_Trees :=
                     Action.Direct_Children ("name_qualifier");
      pragma Unreferenced (Qualifiers);
   begin
      return Start;
   end Evaluate_Object_Reference;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree
     (Context : Program_Tree;
      Name    : String)
      return Program_Tree
   is
   begin
      if Name = "tree" then
         return Context;
      else
         return Context.Program_Child (Name);
      end if;
   end Get_Tree;

   ---------------
   -- Interpret --
   ---------------

   procedure Interpret (Action : Aquarius.Programs.Program_Tree;
                        Node   : Aquarius.Programs.Program_Tree)
   is
   begin
      if False then
         Ada.Text_IO.Put_Line ("action: " & Action.Name);
         Ada.Text_IO.Put_Line ("node: " & Node.Name);
      end if;
      if Action.Name = "compilation_unit" then
         Interpret (Action.Program_Child ("sequence_of_statements"), Node);
      elsif Action.Name = "sequence_of_statements" then
         declare
            Children : constant Array_Of_Program_Trees :=
                         Action.Direct_Children;
         begin
            for Child of Children loop
               Interpret (Child, Node);
            end loop;
         end;
      elsif Action.Name = "statement" then
         declare
            Child : constant Program_Tree :=
                      Action.Chosen_Tree;
         begin
            Interpret (Child, Node);
         end;
      elsif Action.Name = "for_loop_statement" then
         Interpret (Action.Chosen_Tree, Node);
      elsif Action.Name = "for_all_loop" then
         declare
            Loop_Statement : constant Program_Tree :=
                               Action.Program_Child ("loop_statement");
            Sequence       : constant Program_Tree :=
                               Loop_Statement.Program_Child
                                 ("sequence_of_statements");
         begin
            Scan (Sequence, Node);
         end;
      elsif Action.Name = "case_statement" then
         declare
            Expression : constant Program_Tree :=
                           Action.Program_Child ("expression");
            Case_Value : constant Program_Tree :=
                           Evaluate (Expression, Node);
            Case_Options : constant Array_Of_Program_Trees :=
                             Action.Direct_Children ("case_option");
         begin
            Interpret_Case_Options
              (Value   => Case_Value,
               Options => Case_Options,
               Node    => Node);
         end;
      elsif Action.Name = "if_statement" then
         Interpret_If_Statement (Action.Direct_Children, Node);
      end if;
   end Interpret;

   ----------------------
   -- Interpret_Action --
   ----------------------

   procedure Interpret_Action
     (Action    : Aquarius.Programs.Program_Tree;
      Target    : Aquarius.Programs.Program_Tree)
   is
   begin
      Interpret (Action, Target);
   end Interpret_Action;

   ----------------------------
   -- Interpret_Case_Options --
   ----------------------------

   procedure Interpret_Case_Options
     (Value   : Aquarius.Programs.Program_Tree;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree)
   is
   begin
      for Option of Options loop
         if Value.Name = Option.Program_Child ("identifier").Standard_Text then
            Interpret (Option.Program_Child ("sequence_of_statements"),
                       Node);
            return;
         end if;
      end loop;
   end Interpret_Case_Options;

   ----------------------------
   -- Interpret_If_Statement --
   ----------------------------

   procedure Interpret_If_Statement
     (Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree)
   is
      Success    : Boolean := False;
   begin
      for Option of Options loop
         if Option.Name = "if" then
            null;
         elsif Option.Name = "expression" then
            declare
               Check : constant Program_Tree :=
                         Evaluate (Option, Node);
            begin
               Success := Check /= null;
            end;
         elsif Option.Name = "sequence_of_statements" then
            if Success then
               Interpret (Option, Node);
               return;
            end if;
         elsif Option.Name = "else" then
            Success := True;
         elsif Option.Name = "elsif" then
            null;
         elsif Option.Name = "then" then
            null;
         end if;
      end loop;
   end Interpret_If_Statement;

   ----------
   -- Scan --
   ----------

   procedure Scan (Action : Aquarius.Programs.Program_Tree;
                   Top    : Aquarius.Programs.Program_Tree)
   is
      Children : constant Array_Of_Program_Trees :=
                   Top.Direct_Children;
   begin
      Interpret (Action, Top);
      for I in Children'Range loop
         Scan (Action, Children (I));
      end loop;
   end Scan;

end Aquarius.Actions.Interpreter;
