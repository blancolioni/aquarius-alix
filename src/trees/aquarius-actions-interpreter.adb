with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Aquarius.Grammars;
with Aquarius.Properties;
with Aquarius.Trees.Properties;

package body Aquarius.Actions.Interpreter is

   use Aquarius.Programs;

   type Assignment_Target_Type is (No_Target, Property_Target);

   type Assignment_Target
     (Target_Type : Assignment_Target_Type := No_Target)
   is
      record
         case Target_Type is
            when No_Target =>
               null;
            when Property_Target =>
               Tree     : Program_Tree;
               Property : Aquarius.Properties.Property_Type;
         end case;
      end record;

   type Expression_Value_Type is (No_Value, String_Value, Object_Value);

   type Expression_Value
     (Value_Type : Expression_Value_Type := No_Value) is
      record
         case Value_Type is
            when No_Value =>
               null;
            when String_Value =>
               String_Text : Ada.Strings.Unbounded.Unbounded_String;
            when Object_Value =>
               Object      : access Root_Aquarius_Object'Class;
         end case;
      end record;

   function To_String (Value : Expression_Value) return String;
   function To_Boolean (Value : Expression_Value) return Boolean;

   procedure Error
     (Action  : Program_Tree;
      Node    : Program_Tree;
      Message : String);

   procedure Interpret (Action : Aquarius.Programs.Program_Tree;
                        Node   : Aquarius.Programs.Program_Tree);

   procedure Interpret_Case_Options
     (Value   : Expression_Value;
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
      return Expression_Value;

   function Evaluate_Attribute
     (Attribute : Aquarius.Programs.Program_Tree;
      Object    : Expression_Value)
      return Expression_Value;

   function Evaluate_Object_Reference
     (Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Expression_Value;

   function Get_Assignment_Target
     (Action : Program_Tree;
      Node   : Program_Tree)
      return Assignment_Target;

   function Get_Tree
     (Context : Program_Tree;
      Name    : String)
      return Program_Tree;

   procedure Set
     (Target : Assignment_Target;
      Value  : Expression_Value);

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
        (Action.Name & "/" & Node.Name & ": " & Message);
      raise Constraint_Error;
   end Error;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Expression_Value
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
               return (Object_Value, Child);
            else
               return (Object_Value, Child);
            end if;
         end;
      else
         Ada.Text_IO.Put_Line
           ("cannot evaluate expression: " & Action.Name);
         return (Value_Type => No_Value);
      end if;
   end Evaluate;

   ------------------------
   -- Evaluate_Attribute --
   ------------------------

   function Evaluate_Attribute
     (Attribute : Aquarius.Programs.Program_Tree;
      Object    : Expression_Value)
      return Expression_Value
   is
      use Ada.Strings.Unbounded;
      Attribute_Name : constant String :=
                         Attribute.Program_Child ("identifier").Standard_Text;
   begin
      if Attribute_Name = "image" then
         case Object.Value_Type is
            when No_Value =>
               return (String_Value, Null_Unbounded_String);
            when String_Value =>
               return Object;
            when Object_Value =>
               if Object.Object.all in Program_Tree_Type'Class then
                  return (String_Value,
                          To_Unbounded_String
                            (Program_Tree
                               (Object.Object).Concatenate_Children));
               else
                  return (String_Value,
                          To_Unbounded_String (Object.Object.Name));
               end if;
         end case;
      else
         Ada.Text_IO.Put_Line (Attribute_Name & ": no such attribute");
         return (Value_Type => No_Value);
      end if;
   end Evaluate_Attribute;

   -------------------------------
   -- Evaluate_Object_Reference --
   -------------------------------

   function Evaluate_Object_Reference
     (Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Expression_Value
   is
      Start      : constant Program_Tree :=
                     (if Action.Name = "explicit_object_reference"
                      then Get_Tree
                        (Node,
                         Action.Program_Child ("identifier").Standard_Text)
                      else Node);
      Current : Expression_Value := (Object_Value, Start);
      Qualifiers : constant Array_Of_Program_Trees :=
                     Action.Direct_Children ("name_qualifier");
   begin
      for Q of Qualifiers loop
         declare
            Choice : constant Program_Tree :=
                       Q.Chosen_Tree;
         begin
            if Choice.Name = "attribute_reference" then
               Current :=
                 Evaluate_Attribute
                   (Choice, Current);
            end if;
         end;
      end loop;

      return Current;
   end Evaluate_Object_Reference;

   ---------------------------
   -- Get_Assignment_Target --
   ---------------------------

   function Get_Assignment_Target
     (Action : Program_Tree;
      Node   : Program_Tree)
      return Assignment_Target
   is
      Start      : constant Program_Tree :=
                     (if Action.Name = "explicit_object_reference"
                      then Get_Tree
                        (Node,
                         Action.Program_Child ("identifier").Standard_Text)
                      else Node);
      Qualifiers : constant Array_Of_Program_Trees :=
                     Action.Direct_Children ("name_qualifier");
      Grammar    : constant Grammars.Aquarius_Grammar :=
                     Aquarius.Trees.Properties.Get_Grammar (Node);
   begin
      if Qualifiers'Length = 0 then
         Error (Action, Node, "cannot assign to tree");
      else
         declare
            T : Program_Tree := Start;
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
                           return (Property_Target, T,
                                   Grammar.Get_Property_Type
                                     (Property_Name));
                        else
                           declare
                              Child : constant Program_Tree :=
                                        T.Program_Child (Property_Name);
                           begin
                              if Child /= null then
                                 T := Child;
                              else
                                 Error (Q, T, "no such child");
                              end if;
                           end;
                        end if;
                     end;
                  else
                     Error (Q, T, "unimplemented: " & Q.Name);
                  end if;
               end;
            end loop;
         end;
      end if;
      return (Target_Type => No_Target);

   end Get_Assignment_Target;

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
            Case_Value : constant Expression_Value :=
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
      elsif Action.Name = "assignment_statement" then
         declare
            Target : constant Assignment_Target :=
                       Get_Assignment_Target
                         (Action.Program_Child ("object_reference"),
                          Node);
            Value  : constant Expression_Value :=
                       Evaluate (Action.Program_Child ("expression"),
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
      Interpret (Action, Target);
   end Interpret_Action;

   ----------------------------
   -- Interpret_Case_Options --
   ----------------------------

   procedure Interpret_Case_Options
     (Value   : Expression_Value;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree)
   is
   begin
      for Option of Options loop
         if To_String (Value)
           = Option.Program_Child ("identifier").Standard_Text
         then
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
               Check : constant Expression_Value :=
                         Evaluate (Option, Node);
            begin
               Success := To_Boolean (Check);
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

   ---------
   -- Set --
   ---------

   procedure Set
     (Target : Assignment_Target;
      Value  : Expression_Value)
   is
   begin
      case Target.Target_Type is
         when No_Target =>
            null;
         when Property_Target =>
            Ada.Text_IO.Put_Line
              (Target.Tree.Name & "."
               & Aquarius.Properties.Get_Name (Target.Property)
               & " := "
               & To_String (Value));
            case Value.Value_Type is
               when No_Value =>
                  Target.Tree.Clear_Property (Target.Property);
               when String_Value =>
                  Target.Tree.Set_Property
                    (Target.Property,
                     Aquarius.Names.Name_Value
                       (Ada.Strings.Unbounded.To_String
                          (Value.String_Text)));
               when Object_Value =>
                  Target.Tree.Set_Property
                    (Target.Property, Value.Object);
            end case;
      end case;
   end Set;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Value : Expression_Value) return Boolean is
      use Ada.Strings.Unbounded;
   begin
      case Value.Value_Type is
         when No_Value =>
            return False;
         when String_Value =>
            return Value.String_Text /= Null_Unbounded_String;
         when Object_Value =>
            return Value.Object /= null;
      end case;
   end To_Boolean;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Expression_Value) return String is
      use Ada.Strings.Unbounded;
   begin
      case Value.Value_Type is
         when No_Value =>
            return "";
         when String_Value =>
            return To_String (Value.String_Text);
         when Object_Value =>
            return Value.Object.Name;
      end case;
   end To_String;

end Aquarius.Actions.Interpreter;
