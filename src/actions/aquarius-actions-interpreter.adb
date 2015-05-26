with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aquarius.Grammars.Manager;
with Aquarius.Properties;
with Aquarius.Properties.String_Sets;
with Aquarius.Syntax;

with Aquarius.VM;
with Aquarius.VM.Library;
with Aquarius.VM.Maps;

with Aquarius.Loader;
with Aquarius.Messages.Console;

package body Aquarius.Actions.Interpreter is

   use Aquarius.Programs;

   Trace : constant Boolean := False;

   procedure Trace_Message
     (Message : String);

   type Assignment_Target_Type is (No_Target, Property_Target, Object_Target,
                                   Environment_Target);

   type Assignment_Target
     (Target_Type : Assignment_Target_Type := No_Target)
   is
      record
         case Target_Type is
            when No_Target =>
               null;
            when Object_Target =>
               Object   : access Aquarius.VM.Object_Interface'Class;
               Object_Prop : Ada.Strings.Unbounded.Unbounded_String;
            when Property_Target =>
               Tree     : Program_Tree;
               Property : Ada.Strings.Unbounded.Unbounded_String;
            when Environment_Target =>
               Env      : Aquarius.VM.VM_Environment;
               Name     : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   type Aquarius_Writer is
     new Root_Aquarius_Object with
      record
         File_Name : Ada.Strings.Unbounded.Unbounded_String;
         File      : access Ada.Text_IO.File_Type;
      end record;

   overriding function Name
     (Writer : Aquarius_Writer)
      return String
   is (Ada.Strings.Unbounded.To_String (Writer.File_Name));

   type Action_Interpreter is new Action_Execution_Interface with
      record
         Action : Aquarius.Programs.Program_Tree;
      end record;

   overriding procedure Execute
     (Executor : Action_Interpreter;
      Item     : not null access Actionable'Class);

   overriding procedure Execute
     (Executor : Action_Interpreter;
      Parent   : not null access Actionable'Class;
      Child    : not null access Actionable'Class);

   procedure Execute_Action
     (Action    : Aquarius.Programs.Program_Tree;
      Parent    : Aquarius.Programs.Program_Tree;
      Child     : Aquarius.Programs.Program_Tree);

   function Fn_Ada_Body_Name
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_Ada_Specification_Name
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_Create_Set
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_Equal
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_Join
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_New_Line
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_Add
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_Load
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_Put_Line
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_Put
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_Set_Output
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_To_File_Name
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   function Fn_To_Lower
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value;

   Got_Library : Boolean := False;
   Library     : Aquarius.VM.VM_Environment;

   procedure Create_Library (Env : Aquarius.VM.VM_Environment);

   procedure Error
     (Action  : Program_Tree;
      Node    : Program_Tree;
      Message : String);

   procedure Interpret (Env    : Aquarius.VM.VM_Environment;
                        Action : Aquarius.Programs.Program_Tree;
                        Node   : Aquarius.Programs.Program_Tree);

   procedure Interpret_Case_Options
     (Env     : Aquarius.VM.VM_Environment;
      Value   : Aquarius.VM.VM_Value;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree);

   procedure Interpret_If_Statement
     (Env     : Aquarius.VM.VM_Environment;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree);

   procedure Scan
     (Env    : Aquarius.VM.VM_Environment;
      Action : Aquarius.Programs.Program_Tree;
      Top    : Aquarius.Programs.Program_Tree);

   function Evaluate
     (Env    : Aquarius.VM.VM_Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Aquarius.VM.VM_Value;

   function Evaluate_If_Expression
     (Env    : Aquarius.VM.VM_Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Aquarius.VM.VM_Value;

   function Evaluate_Record_Selector
     (Env       : Aquarius.VM.VM_Environment;
      Current   : Aquarius.VM.VM_Value;
      Selector  : String)
      return Aquarius.VM.VM_Value;

   function Evaluate_Object_Reference
     (Env    : Aquarius.VM.VM_Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Aquarius.VM.VM_Value;

   function Apply_Operator
     (Env      : Aquarius.VM.VM_Environment;
      Operator : String;
      Left     : Aquarius.VM.VM_Value;
      Right    : Aquarius.VM.VM_Value)
      return Aquarius.VM.VM_Value;

   function Get_Object_Reference_Start
     (Env    : Aquarius.VM.VM_Environment;
      Action : Program_Tree;
      Node   : Program_Tree)
      return Program_Tree;

   function Get_Assignment_Target
     (Env    : Aquarius.VM.VM_Environment;
      Action : Program_Tree;
      Node   : Program_Tree)
      return Assignment_Target;

   function Get_Tree
     (Env     : Aquarius.VM.VM_Environment;
      Context : Program_Tree;
      Name    : String)
      return Program_Tree;

   procedure Set
     (Target : Assignment_Target;
      Value  : Aquarius.VM.VM_Value);

   --------------------
   -- Apply_Operator --
   --------------------

   function Apply_Operator
     (Env      : Aquarius.VM.VM_Environment;
      Operator : String;
      Left     : Aquarius.VM.VM_Value;
      Right    : Aquarius.VM.VM_Value)
      return Aquarius.VM.VM_Value
   is
      use Aquarius.VM;
      Op : constant VM_Value := Get_Value (Env, Operator);
   begin
      if Op = Null_Value then
         Trace_Message ("unknown operator: " & Operator);
         return Left;
      else
         return VM.Apply (Op, Env, (Left, Right));
      end if;
   end Apply_Operator;

   ------------------
   -- Bind_Actions --
   ------------------

   procedure Bind_Actions
     (Action    : Aquarius.Programs.Program_Tree;
      Group     : Action_Group;
      Grammar   : Aquarius.Grammars.Aquarius_Grammar)
   is

      procedure Bind (Statement : Program_Tree);

      procedure Bind_Declaration (Declaration : Program_Tree);

      ----------
      -- Bind --
      ----------

      procedure Bind (Statement : Program_Tree) is
         Action_Time : constant String :=
                         Statement.Program_Child
                           ("action_time").Chosen_Tree.Name;
         Action_Context : constant Array_Of_Program_Trees :=
                            Statement.Program_Child
                              ("action_context").Direct_Children;
         Action_Definition : constant Program_Tree :=
                               Statement.Program_Child ("action_definition");
         Action_Binding    : constant Action_Interpreter :=
                               (Action =>
                                 Action_Definition.Program_Child
                                  ("sequence_of_statements"));
         Parent_Name       : constant String :=
                               Action_Context (Action_Context'First).Text;
         Child_Name        : constant String :=
                               (if Action_Context'Length > 1
                                then Action_Context (Action_Context'First + 1)
                                .Text
                                else "");
         Parent_Node       : constant Aquarius.Syntax.Syntax_Tree :=
                               Grammar.Get_Definition (Parent_Name);
         Child_Node        : constant Aquarius.Syntax.Syntax_Tree :=
                               (if Child_Name = "" then null
                                else Grammar.Get_Definition (Child_Name));
      begin
         if Child_Name = "" then
            Parent_Node.Set_Action
              (Group    => Group,
               Position => Rule_Position'Value (Action_Time),
               Action   => Action_Binding);
         else
            Parent_Node.Set_Action
              (Child    => Child_Node,
               Group    => Group,
               Position => Rule_Position'Value (Action_Time),
               Action   => Action_Binding);
         end if;
      end Bind;

      ----------------------
      -- Bind_Declaration --
      ----------------------

      procedure Bind_Declaration (Declaration : Program_Tree) is
         pragma Unreferenced (Declaration);
      begin
         null;
      end Bind_Declaration;

   begin
      if Action.Name = "action_statement" then
         Bind (Action);
      elsif Action.Name = "object_declaration" then
         Bind_Declaration (Action);
      else
         declare
            Children : constant Array_Of_Program_Trees :=
                         Action.Direct_Children;
         begin
            for Child of Children loop
               Bind_Actions (Child, Group, Grammar);
            end loop;
         end;
      end if;
   end Bind_Actions;

   --------------------
   -- Create_Library --
   --------------------

   procedure Create_Library (Env : Aquarius.VM.VM_Environment) is
      procedure Make (Name      : String;
                      Fn        : Aquarius.VM.Evaluator;
                      Arg_Count : Natural);

      ----------
      -- Make --
      ----------

      procedure Make (Name      : String;
                      Fn        : Aquarius.VM.Evaluator;
                      Arg_Count : Natural)
      is
      begin
         Aquarius.VM.Insert
           (Env   => Env,
            Name  => Name,
            Value => Aquarius.VM.To_Value (Fn, Arg_Count));
      end Make;

   begin
      Make ("ada_body_name",
            Fn_Ada_Body_Name'Access, 1);
      Make ("ada_specification_name",
            Fn_Ada_Specification_Name'Access, 1);
      Make ("+", Fn_Add'Access, 2);
      Make ("create_set", Fn_Create_Set'Access, 0);
      Make ("=", Fn_Equal'Access, 2);
      Make ("&", Fn_Join'Access, 2);
      Make ("load", Fn_Load'Access, 1);
      Make ("new_line", Fn_New_Line'Access, 0);
      Make ("put_line", Fn_Put_Line'Access, 1);
      Make ("put", Fn_Put'Access, 1);
      Make ("set_output", Fn_Set_Output'Access, 1);
      Make ("to_file_name", Fn_To_File_Name'Access, 2);
      Make ("to_lower", Fn_To_Lower'Access, 1);

   end Create_Library;

   -----------
   -- Error --
   -----------

   procedure Error
     (Action  : Program_Tree;
      Node    : Program_Tree;
      Message : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Action.Image);
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Node.Image);

      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Node.Show_Location & ": " &
         Action.Name & "/" & Node.Name & ": " & Message);
      raise Constraint_Error;
   end Error;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Env    : Aquarius.VM.VM_Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Aquarius.VM.VM_Value
   is
   begin
      if Trace then
         Trace_Message ("Evaluate: " & Action.Image);
         Trace_Message ("Node: " & Node.Image);
      end if;

      if Action.Name = "expression"
        or else Action.Name = "relation"
        or else Action.Name = "simple_expression"
        or else Action.Name = "term"
        or else Action.Name = "factor"
      then
         declare
            Children : constant Array_Of_Program_Trees :=
                         Action.Direct_Children;
            Result   : VM.VM_Value :=
                         Evaluate (Env, Children (Children'First), Node);
            Current  : VM.VM_Value;
            Operator : Program_Tree;
            Index    : Positive := Children'First + 1;
         begin
            while Index <= Children'Last loop
               Operator := Program_Tree (Children (Index).First_Leaf);
               Index := Index + 1;
               Current :=
                 Evaluate (Env, Children (Index), Node);
               Index := Index + 1;
               Result :=
                 Apply_Operator
                   (Env, Operator.Text, Result, Current);
            end loop;
            return Result;
         end;
      elsif Action.Name = "primary" then
         declare
            Child : constant Program_Tree := Action.Chosen_Tree;
         begin
            if Child.Name = "object_reference" then
               return Evaluate_Object_Reference (Env, Child.Chosen_Tree, Node);
            elsif Child.Name = "numeric_literal" then
               return VM.To_Value (Integer'Value (Child.Text));
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

                  return VM.To_Value
                    (String_Text (String_Text'First .. Count));
               end;
            elsif Child.Name = "parenthesised_expression" then
               return Evaluate
                 (Env, Action.Program_Child ("expression"), Node);
            elsif Child.Name = "if_expression" then
               return Evaluate_If_Expression (Env, Child, Node);
            elsif Child.Name = "null" then
               return VM.Null_Value;
            else
               return VM.To_Value (Child);
            end if;
         end;
      else
         Error (Action, Node, "cannot evaluate expression: " & Action.Name);
         return VM.Null_Value;
      end if;
   end Evaluate;

   ----------------------------
   -- Evaluate_If_Expression --
   ----------------------------

   function Evaluate_If_Expression
     (Env    : Aquarius.VM.VM_Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Aquarius.VM.VM_Value
   is
      Options : constant Array_Of_Program_Trees :=
                  Action.Direct_Children;
      Condition  : Boolean := False;
      Success    : Boolean := False;
   begin
      for Option of Options loop
         if Option.Name = "if"
           or else Option.Name = "elsif"
         then
            Condition := True;
            Success   := False;
         elsif Option.Name = "then" then
            null;
         elsif Option.Name = "else" then
            Success := True;
            Condition := False;
         elsif Option.Name = "expression" then
            if Condition then
               declare
                  Check : constant Aquarius.VM.VM_Value :=
                            Evaluate (Env, Option, Node);
               begin
                  Success := VM.To_Boolean (Check);
                  Condition := False;
               end;
            elsif Success then
               return Evaluate (Env, Option, Node);
            else
               Success := False;
            end if;
         end if;
      end loop;
      raise Constraint_Error with
        "this should not have happened";
   end Evaluate_If_Expression;

   -------------------------------
   -- Evaluate_Object_Reference --
   -------------------------------

   function Evaluate_Object_Reference
     (Env    : Aquarius.VM.VM_Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Aquarius.VM.VM_Value
   is
      Current : Aquarius.VM.VM_Value;
      Qualifiers : constant Array_Of_Program_Trees :=
                     Action.Direct_Children ("name_qualifier");
   begin
--        Trace_Message
--          ("Evaluate_Object_Reference: action = " & Action.Image);
--        Trace_Message
--          ("Evaluate_Object_Reference: node = " & Node.Image);

      if Action.Name = "object_reference" then
         return Evaluate_Object_Reference
           (Env, Action.Chosen_Tree, Node);
      end if;

      if Action.Name = "explicit_object_reference" then
         declare
            Name : constant String :=
                     Action.Program_Child ("identifier").Standard_Text;
         begin
            if Name = "tree" then
               Current := VM.To_Value (Node);
            else
               Current := VM.Get_Value (Env, Name);
            end if;
         end;
      else
         Current := VM.To_Value (Node);
      end if;

      for Q of Qualifiers loop
         declare
            Choice : constant Program_Tree :=
                       Q.Chosen_Tree;
         begin
            if Choice.Name = "record_selector" then
               declare
                  Component_Name : constant String :=
                                     Choice.Program_Child
                                       ("identifier").Standard_Text;
               begin
                  Current :=
                    Evaluate_Record_Selector
                      (Env, Current, Component_Name);
               exception
                  when E : others =>
                     Error (Choice, Node,
                            Ada.Exceptions.Exception_Message (E));
                     raise;
               end;
            elsif Choice.Name = "actual_argument_list" then
               declare
                  Arg_Trees  : constant Array_Of_Program_Trees :=
                                 Choice.Direct_Children ("actual_argument");
                  Arg_Values : VM.Array_Of_Values (Arg_Trees'Range);
               begin
                  for I in Arg_Trees'Range loop
                     Arg_Values (I) :=
                       Evaluate (Env,
                                 Arg_Trees (I).Program_Child ("expression"),
                                 Node);
                  end loop;
                  Current := VM.Apply (Current, Env, Arg_Values);
               end;
            elsif Choice.Name = "subtree_selector" then
               if Trace then
                  Trace_Message
                    ("subtree_selector: current = "
                     & VM.To_String (Current));
               end if;

               declare
                  Subtree_Name : constant String :=
                                   Choice.Program_Child
                                     ("identifier").Standard_Text;
               begin
                  if Trace then
                     Trace_Message
                       ("subtree_selector: subtree = " & Subtree_Name);
                  end if;

                  if VM.Has_Tree (Current) then
                     Current :=
                       VM.To_Value
                         (VM.To_Tree (Current).Breadth_First_Search
                          (Subtree_Name, Stop_At_Named => True));
                     if Trace then
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           "subtree_selector: new current = "
                           & VM.To_String (Current));
                     end if;
                  else
                     Error (Action, Node,
                            VM.To_String (Current) & ": not a tree");
                  end if;
               end;
            elsif Choice.Name = "ancestor_selector" then
               if Trace then
                  Trace_Message
                    ("ancestor_selector: current = "
                     & VM.To_String (Current));
               end if;

               declare
                  Ancestor_Name : constant String :=
                                   Choice.Program_Child
                                     ("identifier").Standard_Text;
               begin
                  if Trace then
                     Trace_Message
                       ("ancestor_selector: ancestor = " & Ancestor_Name);
                  end if;

                  if VM.Has_Tree (Current) then
                     declare
                        It : Program_Tree :=
                               Program_Tree (VM.To_Tree (Current));
                     begin
                        while It /= null
                          and then It.Name /= Ancestor_Name
                        loop
                           It := It.Program_Parent;
                        end loop;

                        if It = null then
                           raise Constraint_Error with
                           VM.To_String (Current)
                             & ": no such ancestor: " & Ancestor_Name;
                        else
                           Current := VM.To_Value (It);
                        end if;
                     end;

                     if Trace then
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           "subtree_selector: new current = "
                           & VM.To_String (Current));
                     end if;
                  else
                     Error (Action, Node,
                            VM.To_String (Current) & ": not a tree");
                  end if;
               end;
            else
               Error (Action, Node, "unimplemented: " & Choice.Name);
            end if;
         end;
      end loop;

      return Current;

   end Evaluate_Object_Reference;

   ------------------------------
   -- Evaluate_Record_Selector --
   ------------------------------

   function Evaluate_Record_Selector
     (Env       : Aquarius.VM.VM_Environment;
      Current   : Aquarius.VM.VM_Value;
      Selector  : String)
      return Aquarius.VM.VM_Value
   is
   begin
      if VM.Has_Object (Current)
        and then VM.To_Object (Current).Contains (Selector)
      then
         return VM.To_Object (Current).Get (Selector);
      end if;

      return VM.Evaluate
        (VM.Get_Method (Current, Selector),
         Env);
--
--        case Current.Value_Type is
--           when No_Value =>
--              return Current;
--           when Number_Value =>
--              raise Constraint_Error with
--                "numbers can't be accessed like records yet";
--           when String_Value =>
--              raise Constraint_Error with
--                "strings can't be accessed like records yet";
--           when Library_Function_Value =>
--              declare
--                 No_Arguments : Array_Of_Values (1 .. 0);
--                 Value : constant Aquarius.VM.VM_Value :=
--                           Library_Function_Vector.Element
--                             (Current.Function_Index)
--                             (Env, No_Arguments);
--              begin
--                 if Value.Value_Type = Library_Function_Value then
--                    raise Constraint_Error with
--                      "library functions can't be accessed like records";
--                 else
--                    return Evaluate_Record_Selector
--                      (Env, Value, Selector);
--                 end if;
--              end;
--           when Object_Value =>
--              if Current.Object.all in Program_Tree_Type'Class then
--                 declare
--                    P : constant Program_Tree :=
--                          Program_Tree (Current.Object);
--                    Grammar    : constant Grammars.Aquarius_Grammar :=
--                                   Aquarius.Trees.Properties.Get_Grammar (P);
--                 begin
--                    Ada.Text_IO.Put_Line
--                      (Ada.Text_IO.Standard_Error,
--                       "record_selector: "
--                       & P.Name
--                       & "."
--                       & Selector
--                       & " = "
--                       & (if P.Program_Child (Selector) /= null
--                         then P.Program_Child (Selector).Name
--                         else "<>"));
--
--                    if Grammar.Have_Property_Type (Selector) then
--                       return (Object_Value,
--                               P.Property
--                                 (Grammar.Get_Property_Type (Selector)));
--                    elsif P.Program_Child (Selector) /= null then
--                       return (Object_Value,
--                               P.Program_Child (Selector));
--                    elsif P.Is_Choice
--                      and then P.Chosen_Tree.Program_Child (Selector) /= null
--                    then
--                       return (Object_Value,
--                               P.Chosen_Tree.Program_Child (Selector));
--                    else
--                       return (Value_Type => No_Value);
--                    end if;
--                 end;
--              else
--                 declare
--                    Property_Function : constant Aquarius.VM.VM_Value :=
--                                          Lookup (Env, "__prop_" & Selector);
--                 begin
--                    null;
--                 end;
--              end if;
--        end case;
   end Evaluate_Record_Selector;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Executor : Action_Interpreter;
      Item     : not null access Actionable'Class)
   is
   begin
      Execute_Action (Executor.Action, Program_Tree (Item), null);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Executor : Action_Interpreter;
      Parent   : not null access Actionable'Class;
      Child    : not null access Actionable'Class)
   is
   begin
      Execute_Action (Executor.Action,
                      Program_Tree (Parent), Program_Tree (Child));
   end Execute;

   --------------------
   -- Execute_Action --
   --------------------

   procedure Execute_Action
     (Action    : Aquarius.Programs.Program_Tree;
      Parent    : Aquarius.Programs.Program_Tree;
      Child     : Aquarius.Programs.Program_Tree)
   is
   begin
      if not Got_Library then
         Library :=
           VM.New_Environment ("library", VM.Library.Standard_Library);
         Create_Library (Library);
         Got_Library := True;
      end if;

      declare
         use Aquarius.VM;
         Env : VM_Environment :=
                 New_Environment ("action", Library);
      begin
         Insert (Env, "top",
                 To_Value (Parent.Program_Root));
         Insert (Env, Parent.Name, To_Value (Parent));
         Insert (Env, "tree", To_Value (Parent));
         Insert (Env, "parent", To_Value (Parent));
         if Child /= null then
            Insert (Env, Child.Name, To_Value (Child));
            Insert (Env, "child", To_Value (Child));
         end if;

         Interpret (Env, Action, Parent);

         if Has_Value (Env, "__output") then
            declare
               Writer : constant access Aquarius_Writer :=
                          Aquarius_Writer
                            (To_Property
                               (Get_Value (Env, "__output")).all)'Access;
            begin
               Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
               Ada.Text_IO.Close (Writer.File.all);
            end;
         end if;
         VM.Release_Environment (Env);
      exception
         when others =>
            VM.Release_Environment (Env);
            raise;
      end;
   end Execute_Action;

   ----------------------
   -- Fn_Ada_Body_Name --
   ----------------------

   function Fn_Ada_Body_Name
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Ada.Characters.Handling;
      Package_Name : constant String :=
                       VM.To_String (Arguments (Arguments'First));
      Result : String := Package_Name;
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         else
            Result (I) := To_Lower (Result (I));
         end if;
      end loop;
      return VM.To_Value (Result & ".adb");
   end Fn_Ada_Body_Name;

   -------------------------------
   -- Fn_Ada_Specification_Name --
   -------------------------------

   function Fn_Ada_Specification_Name
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Ada.Characters.Handling;
      Package_Name : constant String :=
                       VM.To_String (Arguments (Arguments'First));
      Result : String := Package_Name;
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         else
            Result (I) := To_Lower (Result (I));
         end if;
      end loop;
      return VM.To_Value (Result & ".ads");
   end Fn_Ada_Specification_Name;

   ------------
   -- Fn_Add --
   ------------

   function Fn_Add
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      Left : VM.VM_Value renames Arguments (Arguments'First);
      Right : VM.VM_Value renames Arguments (Arguments'First + 1);
   begin
      return VM.To_Value (VM.To_Integer (Left) + VM.To_Integer (Right));
   end Fn_Add;

   -------------------
   -- Fn_Create_Set --
   -------------------

   function Fn_Create_Set
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      pragma Unreferenced (Arguments);
      New_Set : constant access Root_Aquarius_Object'Class :=
                  new Aquarius.Properties.String_Sets.String_Set_Property_Type;
   begin
      return VM.To_Value (New_Set);
   end Fn_Create_Set;

   --------------
   -- Fn_Equal --
   --------------

   function Fn_Equal
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Aquarius.VM;
   begin
      if Trace then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "equal: " & To_String (Arguments (Arguments'First))
            & ", " & To_String (Arguments (Arguments'First + 1)));
      end if;

      return VM.To_Value
        (VM.Equal
           (Arguments (Arguments'First),
            Arguments (Arguments'First + 1)));
   end Fn_Equal;

   -------------
   -- Fn_Join --
   -------------

   function Fn_Join
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
   begin
      return VM.To_Value (VM.To_String (Arguments (Arguments'First))
                          & VM.To_String (Arguments (Arguments'First + 1)));
   end Fn_Join;

   -------------
   -- Fn_Load --
   -------------

   function Fn_Load
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use type Aquarius.Grammars.Aquarius_Grammar;
      File_Path   : constant String :=
                      VM.To_String (Arguments (Arguments'First));
      Grammar     : constant Aquarius.Grammars.Aquarius_Grammar :=
                      Aquarius.Grammars.Manager.Get_Grammar_For_File
                        (File_Path);
      Input       : Aquarius.Programs.Program_Tree;
   begin

      if Grammar = null or else Grammar.Has_Errors then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "grammar file contains errors; exiting");
         return VM.Null_Value;
      end if;

      Ada.Text_IO.Put_Line ("Loading " & File_Path);

      Input :=
        Aquarius.Loader.Load_From_File (Grammar, File_Path);

      if Input = null then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Cannot load '" & File_Path &
                                 "'");
         return VM.Null_Value;
      end if;

      Ada.Text_IO.Put_Line ("done");

      Grammar.Run_Action_Trigger (Input,
                                  Aquarius.Actions.Semantic_Trigger);

      declare
         use Aquarius.Messages;
         List : Message_List;
      begin
         Input.Get_Messages (List);
         if Message_Count (List) > 0 then
            if Highest_Level (List) > Warning then
               Aquarius.Messages.Console.Show_Messages (List);
            end if;
         else
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "no messages");
         end if;
      end;

      return VM.To_Value (Input);

   end Fn_Load;

   -----------------
   -- Fn_New_Line --
   -----------------

   function Fn_New_Line
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      pragma Unreferenced (Arguments);
   begin
      Ada.Text_IO.New_Line;
      return VM.Null_Value;
   end Fn_New_Line;

   ------------
   -- Fn_Put --
   ------------

   function Fn_Put
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
   begin
      for Arg of Arguments loop
         if Aquarius.VM.Has_Tree (Arg) then
            Ada.Text_IO.Put
              (Program_Tree (Aquarius.VM.To_Tree (Arg)).Concatenate_Children);
         else
            Ada.Text_IO.Put (VM.To_String (Arg));
         end if;
      end loop;
      return VM.Null_Value;
   end Fn_Put;

   -----------------
   -- Fn_Put_Line --
   -----------------

   function Fn_Put_Line
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
   begin
      for Arg of Arguments loop
         Ada.Text_IO.Put (VM.To_String (Arg));
      end loop;
      Ada.Text_IO.New_Line;
      return VM.Null_Value;
   end Fn_Put_Line;

   -------------------
   -- Fn_Set_Output --
   -------------------

   function Fn_Set_Output
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      Name : constant String :=
               VM.To_String (Arguments (Arguments'First));
      Writer : constant access Aquarius_Writer :=
                 new Aquarius_Writer'
                   (Ada.Strings.Unbounded.To_Unbounded_String (Name),
                    new Ada.Text_IO.File_Type);
      Value  : constant Aquarius.VM.VM_Value :=
                 VM.To_Value (Writer);
   begin
      Ada.Text_IO.Put_Line ("redirect output to: " & Name);
      Ada.Text_IO.Create (Writer.File.all, Ada.Text_IO.Out_File, Name);
      Ada.Text_IO.Set_Output (Writer.File.all);
      VM.Insert (Env, "__output", Value);
      return Value;
   end Fn_Set_Output;

   ---------------------
   -- Fn_To_File_Name --
   ---------------------

   function Fn_To_File_Name
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Ada.Characters.Handling;
      Package_Name : constant String :=
                       VM.To_String (Arguments (Arguments'First));
      Extension    : constant String :=
                       VM.To_String (Arguments (Arguments'First + 1));
      Result : String := Package_Name;
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         else
            Result (I) := To_Lower (Result (I));
         end if;
      end loop;
      return VM.To_Value (Result & "." & Extension);
   end Fn_To_File_Name;

   -----------------
   -- Fn_To_Lower --
   -----------------

   function Fn_To_Lower
     (Env       : Aquarius.VM.VM_Environment;
      Arguments : Aquarius.VM.Array_Of_Values)
      return Aquarius.VM.VM_Value
   is
      pragma Unreferenced (Env);
      use Ada.Characters.Handling;
      Original_Text : constant String :=
                        VM.To_String (Arguments (Arguments'First));
      Result : constant String := To_Lower (Original_Text);
   begin
      return VM.To_Value (Result);
   end Fn_To_Lower;

   ---------------------------
   -- Get_Assignment_Target --
   ---------------------------

   function Get_Assignment_Target
     (Env    : Aquarius.VM.VM_Environment;
      Action : Program_Tree;
      Node   : Program_Tree)
      return Assignment_Target
   is
      Qualifiers : constant Array_Of_Program_Trees :=
                     Action.Direct_Children ("name_qualifier");
   begin
      if Qualifiers'Length = 0 then
         return (Environment_Target, Env,
                 Ada.Strings.Unbounded.To_Unbounded_String
                   (Action.Program_Child ("identifier").Standard_Text));
      else
         declare
            use Ada.Strings.Unbounded;
            use Aquarius.VM;
            Start : constant Program_Tree :=
                      Get_Object_Reference_Start (Env, Action, Node);
            V     : VM_Value := To_Value (Start);
         begin
            for I in Qualifiers'Range loop
               declare
                  Q : constant Program_Tree := Qualifiers (I).Chosen_Tree;
               begin
                  if Q.Name = "record_selector" then
                     declare
                        Identifier    : constant Program_Tree :=
                                          Q.Program_Child ("identifier");
                        Property_Name : constant String :=
                                          Identifier.Standard_Text;
                     begin
                        if I = Qualifiers'Last then
                           if Has_Object (V) then
                              return (Object_Target, To_Object (V),
                                      To_Unbounded_String (Property_Name));
                           elsif Has_Tree (V) then
                              return (Property_Target,
                                      Program_Tree (To_Tree (V)),
                                      To_Unbounded_String (Property_Name));
                           else
                              raise Constraint_Error with
                                "invalid target: " & Node.Concatenate_Children;
                           end if;
                        else
                           if Has_Object (V) then
                              if not To_Object (V).Contains
                                (Property_Name)
                              then
                                 Error (Action, Node,
                                        "object has no method named '"
                                        & Property_Name & "'");
                              end if;
                              V := To_Object (V).Get (Property_Name);
                           elsif Has_Tree (V) then
                              V := To_Value (Program_Tree
                                             (To_Tree (V)).Property
                                             (Property_Name));
                           else
                              raise Constraint_Error with
                                "invalid target: " & Node.Concatenate_Children;
                           end if;
                        end if;
                     end;
                  elsif Q.Name = "subtree_selector" then
                     if Has_Tree (V) then
                        declare
                           Identifier    : constant Program_Tree :=
                                             Q.Program_Child ("identifier");
                           Property_Name : constant String :=
                                             Identifier.Standard_Text;
                           P : constant Program_Tree :=
                                 Program_Tree (To_Tree (V));
                           Child : constant Program_Tree :=
                                     P.Program_Child (Property_Name);
                        begin
                           if Child /= null then
                              V := To_Value (Child);
                           else
                              Error (Q, P, "no such child: " & Property_Name);
                           end if;
                        end;
                     else
                        Error (Q, Node, "not a tree: " & To_String (V));
                     end if;
                  else
                     Error (Q, Node, "unimplemented: " & Q.Name);
                  end if;
               end;
            end loop;
         end;
      end if;
      return (Target_Type => No_Target);

   end Get_Assignment_Target;

   --------------------------------
   -- Get_Object_Reference_Start --
   --------------------------------

   function Get_Object_Reference_Start
     (Env    : Aquarius.VM.VM_Environment;
      Action : Program_Tree;
      Node   : Program_Tree)
      return Program_Tree
   is
   begin
      if Action.Name = "explicit_object_reference" then
         declare
            Id : constant String :=
                   Action.Program_Child ("identifier").Standard_Text;
         begin
            return Get_Tree (Env, Node, Id);
         end;
      elsif Action.Name = "object_reference" then
         declare
            Chosen : constant Program_Tree := Action.Chosen_Tree;
         begin
            if Chosen.Name = "explicit_object_reference" then
               declare
                  Id : constant String :=
                         Chosen.Program_Child ("identifier").Standard_Text;
                  Result : constant Program_Tree :=
                             Get_Tree (Env, Node, Id);
               begin
                  return Result;
               end;
            else
               return Node;
            end if;
         end;
      else
         return Node;
      end if;
   end Get_Object_Reference_Start;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree
     (Env    : Aquarius.VM.VM_Environment;
      Context : Program_Tree;
      Name    : String)
      return Program_Tree
   is
   begin
      if Name = "tree" then
         return Context;
      else
         return Program_Tree (VM.To_Tree (VM.Get_Value (Env, Name)));
      end if;
   end Get_Tree;

   ---------------
   -- Interpret --
   ---------------

   procedure Interpret
     (Env    : Aquarius.VM.VM_Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
   is
   begin
      if Trace then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "action: " & Action.Image);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "node: " & Node.Image);
      end if;
      if Action.Name = "compilation_unit" then
         Interpret (Env,
                    Action.Program_Child ("sequence_of_statements"),
                    Node);
      elsif Action.Name = "sequence_of_statements" then
         declare
            Children : constant Array_Of_Program_Trees :=
                         Action.Direct_Children;
         begin
            for Child of Children loop
               Interpret (Env, Child, Node);
            end loop;
         end;
      elsif Action.Name = "statement" then
         declare
            Child : constant Program_Tree :=
                      Action.Chosen_Tree;
         begin
            Interpret (Env, Child, Node);
         end;
      elsif Action.Name = "declare_statement" then
         Interpret (Env, Action.Program_Child ("declaration"), Node);
      elsif Action.Name = "declaration" then
         Interpret (Env, Action.Program_Child ("object_declaration"), Node);
      elsif Action.Name = "object_declaration" then
         declare
            Declared_Identifier : constant Program_Tree :=
                                    Action.Program_Child ("identifier");
            Declared_Name       : constant String :=
                                    Declared_Identifier.Standard_Text;
            Initial_Value_Tree  : constant Program_Tree :=
                                    Action.Program_Child ("expression");
            Value               : constant Aquarius.VM.VM_Value :=
                                    (if Initial_Value_Tree /= null
                                     then Evaluate
                                       (Env, Initial_Value_Tree, Node)
                                     else VM.Null_Value);
         begin
            if Trace then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "declare " & Declared_Name
                  & " = " & Aquarius.VM.To_String (Value));
            end if;
            VM.Insert (Env, Declared_Name, Value);
         end;
      elsif Action.Name = "procedure_call_statement" then
         declare
            Object : constant Program_Tree :=
                       Action.Program_Child ("object_reference");
            Result : constant VM.VM_Value :=
                       Evaluate_Object_Reference
                         (Env, Object.Chosen_Tree, Node);
         begin
            if Trace then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Call returns: " & VM.To_String (Result));
            end if;
         end;

--           declare
--              use Ada.Strings.Unbounded;
--              use type VM.VM_Value;
--              Identifier : constant Program_Tree :=
--                             Action.Program_Child ("identifier");
--              Procedure_Name : constant String :=
--                                 Identifier.Standard_Text;
--              Argument_List  : constant Program_Tree :=
--                                 Action.Program_Child
           --  ("actual_argument_list");
--              Arg_Trees      : constant Array_Of_Program_Trees :=
--                                 (if Argument_List /= null
--                                  then Argument_List.Direct_Children
--                                    ("expression")
--                                  else Empty_Program_Tree_Array);
--              Arg_Values     : VM.Array_Of_Values (Arg_Trees'Range);
--              Fn_Value       : constant Aquarius.VM.VM_Value :=
--                                 VM.Get_Value (Env, Procedure_Name);
--           begin
--              if Fn_Value /= VM.Null_Value then
--                 for I in Arg_Trees'Range loop
--                    Arg_Values (I) :=
--                      Evaluate (Env, Arg_Trees (I), Node);
--                 end loop;
--                 declare
--                    Result : constant Aquarius.VM.VM_Value :=
--                               VM.Apply (Fn_Value, Env, Arg_Values);
--                    pragma Unreferenced (Result);
--                 begin
--                    null;
--                 end;
--              end if;
--           end;
      elsif Action.Name = "for_loop_statement" then
         Interpret (Env, Action.Chosen_Tree, Node);
      elsif Action.Name = "for_all_loop" then
         declare
            Loop_Statement : constant Program_Tree :=
                               Action.Program_Child ("loop_statement");
            Sequence       : constant Program_Tree :=
                               Loop_Statement.Program_Child
                                 ("sequence_of_statements");
         begin
            Scan (Env, Sequence, Node);
         end;
      elsif Action.Name = "for_tree_loop" then
         declare
            Loop_Value     : constant Aquarius.VM.VM_Value :=
                               Evaluate_Object_Reference
                                 (Env    => Env,
                                  Action =>
                                    Action.Program_Child
                                      ("object_reference"),
                                  Node   => Node);
            Loop_Statement : constant Program_Tree :=
                               Action.Program_Child ("loop_statement");
            Sequence       : constant Program_Tree :=
                               Loop_Statement.Program_Child
                                 ("sequence_of_statements");
            Loop_Env       : VM.VM_Environment :=
                               VM.New_Environment ("Tree_Loop",
                                                   Env);
         begin
            if Trace then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "for " & Node.Name & "/" & VM.To_String (Loop_Value));
            end if;
            VM.Insert (Loop_Env, "first_loop", VM.To_Value (True));
            if VM.Has_Tree (Loop_Value)
              and then VM.To_Tree (Loop_Value).all in Program_Tree_Type'Class
            then
               declare
                  Tree : constant Program_Tree :=
                           Program_Tree (VM.To_Tree (Loop_Value));
                  Children : constant Array_Of_Program_Trees :=
                               Tree.Direct_Children;
               begin
                  for I in Children'Range loop
                     Interpret (Loop_Env, Sequence, Children (I));

                     if I = Children'First then
                        VM.Replace (Loop_Env, "first_loop",
                                    VM.To_Value (False));
                     end if;

                  end loop;
               end;
            else
               Error (Action, Node,
                      "unable to loop with " & VM.To_String (Loop_Value));
            end if;

            VM.Release_Environment (Loop_Env);
         end;
      elsif Action.Name = "iterator_loop" then
         declare
            Iterator_Name  : constant String :=
                               Action.Program_Child
                                 ("identifier").Standard_Text;
            Loop_Value     : constant Aquarius.VM.VM_Value :=
                               Evaluate_Object_Reference
                                 (Env    => Env,
                                  Action =>
                                    Action.Program_Child
                                      ("object_reference"),
                                  Node   => Node);
            Loop_Statement : constant Program_Tree :=
                               Action.Program_Child ("loop_statement");
            Sequence       : constant Program_Tree :=
                               Loop_Statement.Program_Child
                                 ("sequence_of_statements");
            Loop_Env       : VM.VM_Environment :=
                               VM.New_Environment (Iterator_Name & "_Loop",
                                                   Env);
         begin
            if Trace then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "for " & Iterator_Name & " of "
                  & Node.Name & "/" & VM.To_String (Loop_Value));
            end if;

            VM.Insert (Loop_Env, "first_loop", VM.To_Value (True));

            if VM.Has_Tree (Loop_Value)
              and then VM.To_Tree (Loop_Value).all in Program_Tree_Type'Class
            then
               declare
                  Tree : constant Program_Tree :=
                           Program_Tree (VM.To_Tree (Loop_Value));
                  Children : constant Array_Of_Program_Trees :=
                               Tree.Direct_Children
                                 (Iterator_Name);
               begin
                  for I in Children'Range loop
                     if Trace then
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           (if VM.To_Boolean
                              (VM.Get_Value (Loop_Env, "first_loop"))
                            then "  first " else "  ")
                             & "loop: " & Children (I).Name);
                     end if;
                     Interpret (Loop_Env, Sequence, Children (I));
                     VM.Replace (Loop_Env, "first_loop", VM.To_Value (False));

                  end loop;
               end;
            elsif VM.Has_Property (Loop_Value)
              and then VM.To_Property (Loop_Value).all in
              Properties.String_Sets.String_Set_Property_Type'Class
            then
               declare
                  procedure Process (Value : String);

                  -------------
                  -- Process --
                  -------------

                  procedure Process (Value : String) is
                  begin
                     VM.Replace (Loop_Env, Iterator_Name,
                                 VM.To_Value (Value));
                     Interpret (Loop_Env, Sequence, Node);
                     VM.Replace (Loop_Env, "first_loop",
                                 VM.To_Value (False));
                  end Process;

               begin
                  VM.Insert (Loop_Env, Iterator_Name, VM.Null_Value);

                  Properties.String_Sets.String_Set_Property_Type'Class
                    (VM.To_Property (Loop_Value).all).Iterate
                    (Process'Access);
               end;
            elsif VM.Has_Property (Loop_Value)
              and then VM.To_Property (Loop_Value).all in
              VM.Maps.Map_Property_Type'Class
            then
               declare
                  procedure Process
                    (Value : VM.VM_Value);

                  -------------
                  -- Process --
                  -------------

                  procedure Process
                    (Value : VM.VM_Value)
                  is
                     --  pragma Unreferenced (Key);
                  begin
                     VM.Replace (Loop_Env, Iterator_Name, Value);
                     Interpret (Loop_Env, Sequence, Node);
                     VM.Replace (Loop_Env, "first_loop",
                                 VM.To_Value (False));
                  end Process;

               begin
                  VM.Insert (Loop_Env, Iterator_Name, VM.Null_Value);

                  VM.Maps.Map_Property_Type'Class
                    (VM.To_Property (Loop_Value).all).Iterate
                    (Process'Access);
               end;
            else
               Error (Action, Node,
                      "unable to loop with " & VM.To_String (Loop_Value));
            end if;

            VM.Release_Environment (Loop_Env);
         end;
      elsif Action.Name = "case_statement" then
         declare
            Expression : constant Program_Tree :=
                           Action.Program_Child ("expression");
            Case_Value : constant Aquarius.VM.VM_Value :=
                           Evaluate (Env, Expression, Node);
            Case_Options : constant Array_Of_Program_Trees :=
                             Action.Direct_Children ("case_option");
         begin
            Interpret_Case_Options
              (Env     => Env,
               Value   => Case_Value,
               Options => Case_Options,
               Node    => Node);
         end;
      elsif Action.Name = "if_statement" then
         Interpret_If_Statement (Env, Action.Direct_Children, Node);
      elsif Action.Name = "assignment_statement" then
         declare
            Target : constant Assignment_Target :=
                       Get_Assignment_Target
                         (Env,
                          Action.Program_Child ("object_reference"),
                          Node);
            Value  : constant Aquarius.VM.VM_Value :=
                       Evaluate (Env,
                                 Action.Program_Child ("expression"),
                                 Node);
         begin
            Set (Target, Value);
         end;
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
      if not Got_Library then
         Library :=
           VM.New_Environment ("library", VM.Library.Standard_Library);
         Create_Library (Library);
         Got_Library := True;
      end if;

      declare
         use Aquarius.VM;
         Env : VM_Environment :=
                 New_Environment ("action", Library);
      begin
         Insert (Env, "top",
                 To_Value (Target));
         Interpret (Env, Action, Target);
         if Has_Value (Env, "__output") then
            declare
               Writer : constant access Aquarius_Writer :=
                          Aquarius_Writer
                            (To_Property
                               (Get_Value (Env, "__output")).all)'Access;
            begin
               Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
               Ada.Text_IO.Close (Writer.File.all);
            end;
         end if;
         VM.Release_Environment (Env);
      exception
         when others =>
            VM.Release_Environment (Env);
            raise;
      end;
   end Interpret_Action;

   ----------------------------
   -- Interpret_Case_Options --
   ----------------------------

   procedure Interpret_Case_Options
     (Env    : Aquarius.VM.VM_Environment;
      Value   : Aquarius.VM.VM_Value;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree)
   is
      Choice : Program_Tree;
      Value_Text : constant String := VM.To_String (Value);
   begin

      if VM.Has_Tree (Value)
        and then VM.To_Tree (Value).all in Program_Tree_Type'Class
        and then Program_Tree (VM.To_Tree (Value)).Internal_Tree
        and then Program_Tree (VM.To_Tree (Value)).Is_Choice
        and then Program_Tree (VM.To_Tree (Value)).Child_Count = 1
      then
         Choice := Program_Tree (VM.To_Tree (Value)).Chosen_Tree;
      end if;

      for Option of Options loop
         declare
            Image : constant String :=
                      Option.Program_Child ("identifier").Standard_Text;
         begin
            if Value_Text = Image
              or else
                (Choice /= null and then Choice.Standard_Text = Image)
            then
               Interpret (Env,
                          Option.Program_Child ("sequence_of_statements"),
                          Node);
               return;
            end if;
         end;
      end loop;
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "exception raised in Interpret_Case_Options");
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "node: " & Node.Image);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "value: " & Aquarius.VM.Show (Value));
         raise;

   end Interpret_Case_Options;

   ----------------------------
   -- Interpret_If_Statement --
   ----------------------------

   procedure Interpret_If_Statement
     (Env    : Aquarius.VM.VM_Environment;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree)
   is
      Success    : Boolean := False;
   begin
      for Option of Options loop
         if Trace then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  Option.Image);
         end if;

         if Option.Name = "if" then
            null;
         elsif Option.Name = "expression" then
--              Ada.Text_IO.Put_Line
--                (Ada.Text_IO.Standard_Error,
--                 "Evaluating: " & Node.Image);
            declare
               Check : constant Aquarius.VM.VM_Value :=
                         Evaluate (Env, Option, Node);
            begin
               if Trace then
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "Result: " & VM.To_String (Check));
               end if;
               Success := VM.To_Boolean (Check);
            end;
         elsif Option.Name = "sequence_of_statements" then
            if Success then
               Interpret (Env, Option, Node);
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

   procedure Scan
     (Env    : Aquarius.VM.VM_Environment;
      Action : Aquarius.Programs.Program_Tree;
      Top    : Aquarius.Programs.Program_Tree)
   is
      Children : constant Array_Of_Program_Trees :=
                   Top.Direct_Children;
   begin
      if not Top.Internal_Tree then
         Interpret (Env, Action, Top);
      end if;

      for I in Children'Range loop
         Scan (Env, Action, Children (I));
      end loop;
   end Scan;

   ---------
   -- Set --
   ---------

   procedure Set
     (Target : Assignment_Target;
      Value  : Aquarius.VM.VM_Value)
   is
   begin
      case Target.Target_Type is
         when No_Target =>
            Ada.Text_IO.Put_Line ("assignment: no target");
         when Object_Target =>
            Target.Object.Set
              (Ada.Strings.Unbounded.To_String (Target.Object_Prop),
               Value);
         when Property_Target =>
            if Trace then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  (if Target.Tree.Has_Property ("full_name")
                   then Target.Tree.Property ("full_name").Name
                   else Target.Tree.Name)
                  & "."
                  & Ada.Strings.Unbounded.To_String (Target.Property)
                  & " := "
                  & VM.To_String (Value));
            end if;
            Target.Tree.Set_Property
              (Ada.Strings.Unbounded.To_String (Target.Property),
               VM.To_Property (Value));
         when Environment_Target =>
            if Trace then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Ada.Strings.Unbounded.To_String (Target.Name)
                  & " := "
                  & VM.To_String (Value));
            end if;
            VM.Replace (Target.Env,
                        Ada.Strings.Unbounded.To_String (Target.Name),
                        Value);
      end case;
   end Set;

   -------------------
   -- Trace_Message --
   -------------------

   procedure Trace_Message
     (Message : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "interpreter: " & Message);
   end Trace_Message;

end Aquarius.Actions.Interpreter;
