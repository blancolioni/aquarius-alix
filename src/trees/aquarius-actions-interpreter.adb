with Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

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

   type Library_Function_Index is new Positive;

   type Expression_Value_Type is (No_Value,
                                  String_Value, Object_Value,
                                  Library_Function_Value);

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
            when Library_Function_Value =>
               Function_Index : Library_Function_Index;
         end case;
      end record;

   function To_Name (Value : Expression_Value) return String;
   function To_String (Value : Expression_Value) return String;
   function To_Boolean (Value : Expression_Value) return Boolean;

   package Environment_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => Expression_Value,
        Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Unbounded."=");

   type Environment_Record;
   type Environment is access Environment_Record;
   type Environment_Record is
      record
         Parent : Environment;
         Map    : Environment_Maps.Map;
         Output : access Ada.Text_IO.File_Type;
      end record;

   function Create_Environment
     (Parent : Environment)
      return Environment;

   procedure Release_Environment
     (Env : in out Environment);

   function Lookup
     (Env     : Environment;
      Context : Program_Tree;
      Name    : String)
      return Expression_Value;

   function Lookup
     (Env     : Environment;
      Name    : String)
      return Expression_Value
   is (Lookup (Env, null, Name));

   procedure Insert
     (Env   : Environment;
      Name  : String;
      Value : Expression_Value);

   type Array_Of_Values is array (Positive range <>) of Expression_Value;
   type Library_Function is access
     function (Env       : Environment;
               Arguments : Array_Of_Values) return Expression_Value;

   package Library_Vectors is
     new Ada.Containers.vectors
       (Library_Function_Index, Library_Function);

   Library_Function_Vector   : Library_Vectors.Vector;
   Attribute_Function_Vector : Library_Vectors.Vector;

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

   function Fn_Ada_Specification_Name
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value;

   function Fn_New_Line (Env       : Environment;
                         Arguments : Array_Of_Values) return Expression_Value;
   function Fn_Put_Line (Env       : Environment;
                         Arguments : Array_Of_Values) return Expression_Value;
   function Fn_Put (Env       : Environment;
                    Arguments : Array_Of_Values) return Expression_Value;
   function Fn_Set_Output
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value;

   function Attribute_Image
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value;

   function Attribute_Last
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value;

   Got_Library : Boolean := False;
   Library     : Environment;

   procedure Create_Library (Env : Environment);

   procedure Error
     (Action  : Program_Tree;
      Node    : Program_Tree;
      Message : String);

   procedure Interpret (Env    : Environment;
                        Action : Aquarius.Programs.Program_Tree;
                        Node   : Aquarius.Programs.Program_Tree);

   procedure Interpret_Case_Options
     (Env     : Environment;
      Value   : Expression_Value;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree);

   procedure Interpret_If_Statement
     (Env     : Environment;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree);

   procedure Scan
     (Env    : Environment;
      Action : Aquarius.Programs.Program_Tree;
      Top    : Aquarius.Programs.Program_Tree);

   function Evaluate
     (Env    : Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Expression_Value;

   function Evaluate_If_Expression
     (Env    : Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Expression_Value;

   function Evaluate_Attribute
     (Env       : Environment;
      Attribute : Aquarius.Programs.Program_Tree;
      Object    : Expression_Value)
      return Expression_Value;

   function Evaluate_Record_Selector
     (Env       : Environment;
      Current   : Expression_Value;
      Selector  : String)
      return Expression_Value;

   function Evaluate_Object_Reference
     (Env    : Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Expression_Value;

   function Get_Assignment_Target
     (Env    : Environment;
      Action : Program_Tree;
      Node   : Program_Tree)
      return Assignment_Target;

   function Get_Tree
     (Env     : Environment;
      Context : Program_Tree;
      Name    : String)
      return Program_Tree;

   procedure Set
     (Target : Assignment_Target;
      Value  : Expression_Value);

   ---------------------
   -- Attribute_Image --
   ---------------------

   function Attribute_Image
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value
   is
      pragma Unreferenced (Env);
      use Ada.Strings.Unbounded;
      Object : constant Expression_Value :=
                 Arguments (Arguments'First);
   begin
      case Object.Value_Type is
         when No_Value =>
            return (String_Value, Null_Unbounded_String);
         when String_Value =>
            return Object;
         when Library_Function_Value =>
            return (String_Value, To_Unbounded_String ("<library function>"));
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
   end Attribute_Image;

   --------------------
   -- Attribute_Last --
   --------------------

   function Attribute_Last
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value
   is
      pragma Unreferenced (Env);
      use Ada.Strings.Unbounded;
      Object : constant Expression_Value :=
                 Arguments (Arguments'First);
   begin
      case Object.Value_Type is
         when No_Value =>
            return (String_Value, Null_Unbounded_String);
         when String_Value =>
            return Object;
         when Library_Function_Value =>
            return (String_Value, To_Unbounded_String ("<library function>"));
         when Object_Value =>
            if Object.Object.all in Program_Tree_Type'Class then
               declare
                  Children : constant Array_Of_Program_Trees :=
                               Program_Tree (Object.Object).Direct_Children;
               begin
                  if Children'Length > 0 then
                     return (Object_Value, Children (Children'Last));
                  else
                     return Object;
                  end if;
               end;
            else
               return (String_Value,
                       To_Unbounded_String (Object.Object.Name));
            end if;
      end case;
   end Attribute_Last;

   ------------------------
   -- Create_Environment --
   ------------------------

   function Create_Environment
     (Parent : Environment)
      return Environment
   is
   begin
      return Env : constant Environment := new Environment_Record do
         Env.Parent := Parent;
      end return;
   end Create_Environment;

   --------------------
   -- Create_Library --
   --------------------

   procedure Create_Library (Env : Environment) is
      procedure Make (V    : in out Library_Vectors.Vector;
                      Name : String;
                      Fn   : Library_Function);

      ----------
      -- Make --
      ----------

      procedure Make (V    : in out Library_Vectors.Vector;
                      Name : String;
                      Fn   : Library_Function)
      is
      begin
         V.Append (Fn);
         Insert (Env, Name, (Library_Function_Value, V.Last_Index));
      end Make;

   begin
      Make (Library_Function_Vector,
            "ada_specification_name",
            Fn_Ada_Specification_Name'Access);
      Make (Library_Function_Vector, "new_line", Fn_New_Line'Access);
      Make (Library_Function_Vector, "put_line", Fn_Put_Line'Access);
      Make (Library_Function_Vector, "put", Fn_Put'Access);
      Make (Library_Function_Vector, "set_output", Fn_Set_Output'Access);

      Make (Attribute_Function_Vector, "__attr_image", Attribute_Image'Access);
      Make (Attribute_Function_Vector, "__attr_last", Attribute_Last'Access);

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
         Action.Name & "/" & Node.Name & ": " & Message);
      raise Constraint_Error;
   end Error;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Env    : Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Expression_Value
   is
   begin
      if Action.Name = "expression" then
         return Evaluate (Env, Action.Program_Child ("relation"), Node);
      elsif Action.Name = "relation" then
         return Evaluate
           (Env, Action.Program_Child ("simple_expression"), Node);
      elsif Action.Name = "simple_expression" then
         return Evaluate (Env, Action.Program_Child ("term"), Node);
      elsif Action.Name = "term" then
         return Evaluate (Env, Action.Program_Child ("factor"), Node);
      elsif Action.Name = "factor" then
         return Evaluate (Env, Action.Program_Child ("primary"), Node);
      elsif Action.Name = "primary" then
         declare
            Child : constant Program_Tree := Action.Chosen_Tree;
         begin
            if Child.Name = "object_reference" then
               return Evaluate_Object_Reference (Env, Child.Chosen_Tree, Node);
            elsif Child.Name = "numeric_literal" then
               return (Object_Value, Child);
            elsif Child.Name = "string_literal" then
               declare
                  String_Text : constant String := Child.Text;
               begin
                  return (String_Value,
                          Ada.Strings.Unbounded.To_Unbounded_String
                            (String_Text
                               (String_Text'First + 1 ..
                                  String_Text'Last - 1)));
               end;
            elsif Child.Name = "parenthesised_expression" then
               return Evaluate
                 (Env, Action.Program_Child ("expression"), Node);
            elsif Child.Name = "if_expression" then
               return Evaluate_If_Expression (Env, Child, Node);
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
     (Env    : Environment;
      Attribute : Aquarius.Programs.Program_Tree;
      Object    : Expression_Value)
      return Expression_Value
   is
      use Ada.Strings.Unbounded;
      Attribute_Name : constant String :=
                         Attribute.Program_Child ("identifier").Standard_Text;
      Library_Name   : constant String :=
                         "__attr_" & Attribute_Name;
      Library_Value  : constant Expression_Value :=
                         Lookup (Env, Library_Name);
   begin
      if Library_Value.Value_Type = Library_Function_Value then
         return Attribute_Function_Vector.Element
           (Library_Value.Function_Index)
           (Env, (1 => Object));
      else
         Ada.Text_IO.Put_Line (Attribute_Name & ": no such attribute");
         return (Value_Type => No_Value);
      end if;
   end Evaluate_Attribute;

   function Evaluate_If_Expression
     (Env    : Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Expression_Value
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
                  Check : constant Expression_Value :=
                            Evaluate (Env, Option, Node);
               begin
                  Success := To_Boolean (Check);
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
     (Env    : Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
      return Expression_Value
   is
      Current : Expression_Value;
      Qualifiers : constant Array_Of_Program_Trees :=
                     Action.Direct_Children ("name_qualifier");
   begin
      if Action.Name = "explicit_object_reference" then
         declare
            Name : constant String :=
                     Action.Program_Child ("identifier").Standard_Text;
         begin
            if Name = "tree" then
               Current := (Object_Value, Node);
            else
               Current := Lookup (Env, Node, Name);
            end if;
         end;
      else
         Current := (Object_Value, Node);
      end if;

      for Q of Qualifiers loop
         declare
            Choice : constant Program_Tree :=
                       Q.Chosen_Tree;
         begin
            if Choice.Name = "attribute_reference" then
               Current :=
                 Evaluate_Attribute
                   (Env, Choice, Current);
            elsif Choice.Name = "record_selector" then
               declare
                  Component_Name : constant String :=
                                     Choice.Program_Child
                                       ("identifier").Standard_Text;
               begin
                  Current :=
                    Evaluate_Record_Selector
                      (Env, Current, Component_Name);
               end;
            elsif Choice.Name = "actual_argument_list" then
               if Current.Value_Type /= Library_Function_Value then
                  Error (Q, Node,
                         "cannot treat value (" & To_String (Current)
                         & ") as function");
               else
                  declare
                     Arg_Trees  : constant Array_Of_Program_Trees :=
                                    Choice.Direct_Children ("actual_argument");
                     Arg_Values : Array_Of_Values (Arg_Trees'Range);
                  begin
                     for I in Arg_Trees'Range loop
                        Arg_Values (I) :=
                          Evaluate (Env,
                                    Arg_Trees (I).Program_Child ("expression"),
                                    Node);
                     end loop;
                     Current :=
                       Library_Function_Vector.Element
                         (Current.Function_Index)
                         (Env, Arg_Values);
                  end;
               end if;
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
     (Env       : Environment;
      Current   : Expression_Value;
      Selector  : String)
      return Expression_Value
   is
      pragma Unreferenced (Env);  --  for now
   begin
      case Current.Value_Type is
         when No_Value =>
            return Current;
         when String_Value =>
            raise Constraint_Error with
              "strings can't be accessed like records yet";
         when Library_Function_Value =>
            raise Constraint_Error with
              "library functions can't be accessed like records";
         when Object_Value =>
            if Current.Object.all in Program_Tree_Type'Class then
               declare
                  P : constant Program_Tree :=
                        Program_Tree (Current.Object);
                  Grammar    : constant Grammars.Aquarius_Grammar :=
                                 Aquarius.Trees.Properties.Get_Grammar (P);
               begin
                  if Grammar.Have_Property_Type (Selector) then
                     return (Object_Value,
                             P.Property
                               (Grammar.Get_Property_Type (Selector)));
                  elsif P.Program_Child (Selector) /= null then
                     return (Object_Value,
                             P.Program_Child (Selector));
                  else
                     return (Value_Type => No_Value);
                  end if;
               end;
            else
               raise Constraint_Error with
                 "cannot apply record selector to " & Current.Object.Name;
            end if;
      end case;
   end Evaluate_Record_Selector;

   -------------------------------
   -- Fn_Ada_Specification_Name --
   -------------------------------

   function Fn_Ada_Specification_Name
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value
   is
      pragma Unreferenced (Env);
      use Ada.Characters.Handling;
      Package_Name : constant String :=
                       To_String (Arguments (Arguments'First));
      Result : String := Package_Name;
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         else
            Result (I) := To_Lower (Result (I));
         end if;
      end loop;
      return (String_Value,
              Ada.Strings.Unbounded.To_Unbounded_String
                (Result & ".ads"));
   end Fn_Ada_Specification_Name;

   -----------------
   -- Fn_New_Line --
   -----------------

   function Fn_New_Line
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value
   is
      pragma Unreferenced (Env);
      pragma Unreferenced (Arguments);
   begin
      Ada.Text_IO.New_Line;
      return (Value_Type => No_Value);
   end Fn_New_Line;

   ------------
   -- Fn_Put --
   ------------

   function Fn_Put
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value
   is
      pragma Unreferenced (Env);
   begin
      for Arg of Arguments loop
--           if Arg.Value_Type = Object_Value then
--              Ada.Text_IO.Put_Line (Program_Tree (Arg.Object).Image);
--           end if;
         Ada.Text_IO.Put (To_String (Arg));
      end loop;
      return (Value_Type => No_Value);
   end Fn_Put;

   -----------------
   -- Fn_Put_Line --
   -----------------

   function Fn_Put_Line
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value
   is
      pragma Unreferenced (Env);
   begin
      for Arg of Arguments loop
         Ada.Text_IO.Put (To_String (Arg));
      end loop;
      Ada.Text_IO.New_Line;
      return (Value_Type => No_Value);
   end Fn_Put_Line;

   -------------------
   -- Fn_Set_Output --
   -------------------

   function Fn_Set_Output
     (Env       : Environment;
      Arguments : Array_Of_Values)
      return Expression_Value
   is
      Name : constant String := To_String (Arguments (Arguments'First));
      Writer : Aquarius_Writer :=
                 (Ada.Strings.Unbounded.To_Unbounded_String (Name),
                  new Ada.Text_IO.File_Type);
      Value  : constant Expression_Value :=
                 (Object_Value, new Aquarius_Writer'(Writer));
   begin
      Ada.Text_IO.Create (Writer.File.all, Ada.Text_IO.Out_File, Name);
      Ada.Text_IO.Set_Output (Writer.File.all);
      Env.Map.Insert
        (Ada.Strings.Unbounded.To_Unbounded_String ("__output"),
         Value);
      return Value;
   end Fn_Set_Output;

   ---------------------------
   -- Get_Assignment_Target --
   ---------------------------

   function Get_Assignment_Target
     (Env    : Environment;
      Action : Program_Tree;
      Node   : Program_Tree)
      return Assignment_Target
   is
      Start      : constant Program_Tree :=
                     (if Action.Name = "explicit_object_reference"
                      then Get_Tree
                        (Env, Node,
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
     (Env    : Environment;
      Context : Program_Tree;
      Name    : String)
      return Program_Tree
   is
      pragma Unreferenced (Env);
   begin
      if Name = "tree" then
         return Context;
      else
         return Context.Program_Child (Name);
      end if;
   end Get_Tree;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Env   : Environment;
      Name  : String;
      Value : Expression_Value)
   is
      use Ada.Strings.Unbounded;
      use Environment_Maps;
      Key : constant Unbounded_String :=
              To_Unbounded_String (Name);
      Position : constant Cursor := Env.Map.Find (Key);
   begin
      if Has_Element (Position) then
         Env.Map.Replace_Element (Position, Value);
      else
         Env.Map.Insert (Key, Value);
      end if;
   end Insert;

   ---------------
   -- Interpret --
   ---------------

   procedure Interpret
     (Env    : Environment;
      Action : Aquarius.Programs.Program_Tree;
      Node   : Aquarius.Programs.Program_Tree)
   is
   begin
      if False then
         Ada.Text_IO.Put_Line ("action: " & Action.Name);
         Ada.Text_IO.Put_Line ("node: " & Node.Name);
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
            Value               : constant Expression_Value :=
                                    (if Initial_Value_Tree /= null
                                     then Evaluate
                                       (Env, Initial_Value_Tree, Node)
                                     else (Value_Type => No_Value));
         begin
            Insert (Env, Declared_Name, Value);
         end;
      elsif Action.Name = "procedure_call_statement" then
         declare
            use Ada.Strings.Unbounded;
            Identifier : constant Program_Tree :=
                           Action.Program_Child ("identifier");
            Procedure_Name : constant String :=
                               Identifier.Standard_Text;
            Arg_Trees      : constant Array_Of_Program_Trees :=
                               Action.Direct_Children ("expression");
            Arg_Values     : Array_Of_Values (Arg_Trees'Range);
            Fn_Value       : constant Expression_Value :=
                               Lookup (Env, Procedure_Name);
         begin
            if Fn_Value.Value_Type = Library_Function_Value then
               for I in Arg_Trees'Range loop
                  Arg_Values (I) :=
                    Evaluate (Env, Arg_Trees (I), Node);
               end loop;
               declare
                  Result : constant Expression_Value :=
                             Library_Function_Vector.Element
                               (Fn_Value.Function_Index)
                               (Env, Arg_Values);
                  pragma Unreferenced (Result);
               begin
                  null;
               end;
            else
               Error (Action, Node, "undefined: " & Procedure_Name);
            end if;
         end;
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
            Loop_Value     : constant Expression_Value :=
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
         begin
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "for " & Node.Name & "/" & To_String (Loop_Value));
            if Loop_Value.Value_Type = Object_Value
              and then Loop_Value.Object.all in Program_Tree_Type'Class
            then
               declare
                  Tree : constant Program_Tree :=
                           Program_Tree (Loop_Value.Object);
                  Children : constant Array_Of_Program_Trees :=
                               Tree.Direct_Children;
               begin
                  for I in Children'Range loop
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "  loop: " & Children (I).Name);
                     Interpret (Env, Sequence, Children (I));
                  end loop;
               end;
            else
               Error (Action, Node,
                      "unable to loop with " & To_String (Loop_Value));
            end if;
         end;
      elsif Action.Name = "iterator_loop" then
         declare
            Iterator_Name  : constant String :=
                               Action.Program_Child
                                 ("identifier").Standard_Text;
            Loop_Value     : constant Expression_Value :=
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
         begin
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "for " & Iterator_Name & " of "
               & Node.Name & "/" & To_String (Loop_Value));
            if Loop_Value.Value_Type = Object_Value
              and then Loop_Value.Object.all in Program_Tree_Type'Class
            then
               declare
                  Tree : constant Program_Tree :=
                           Program_Tree (Loop_Value.Object);
                  Children : constant Array_Of_Program_Trees :=
                               Tree.Direct_Children
                                 (Iterator_Name);
               begin
                  for I in Children'Range loop
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "  loop: " & Children (I).Name);
                     Interpret (Env, Sequence, Children (I));
                  end loop;
               end;
            else
               Error (Action, Node,
                      "unable to loop with " & To_String (Loop_Value));
            end if;
         end;
      elsif Action.Name = "case_statement" then
         declare
            Expression : constant Program_Tree :=
                           Action.Program_Child ("expression");
            Case_Value : constant Expression_Value :=
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
            Value  : constant Expression_Value :=
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
         Library := Create_Environment (null);
         Create_Library (Library);
         Got_Library := True;
      end if;

      declare
         Env : Environment := Create_Environment (Library);
      begin
         Interpret (Env, Action, Target);
         Release_Environment (Env);
      exception
         when others =>
            Release_Environment (Env);
            raise;
      end;
   end Interpret_Action;

   ----------------------------
   -- Interpret_Case_Options --
   ----------------------------

   procedure Interpret_Case_Options
     (Env    : Environment;
      Value   : Expression_Value;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
      Node    : Aquarius.Programs.Program_Tree)
   is
      Choice : Program_Tree;
      Value_Text : constant String := To_Name (Value);
   begin
      if Value.Value_Type = Object_Value
        and then Value.Object.all in Program_Tree_Type'Class
        and then Program_Tree (Value.Object).Is_Choice
      then
         Choice := Program_Tree (Value.Object).Chosen_Tree;
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
   end Interpret_Case_Options;

   ----------------------------
   -- Interpret_If_Statement --
   ----------------------------

   procedure Interpret_If_Statement
     (Env    : Environment;
      Options : Aquarius.Programs.Array_Of_Program_Trees;
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
                         Evaluate (Env, Option, Node);
            begin
               Success := To_Boolean (Check);
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

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Env     : Environment;
      Context : Program_Tree;
      Name    : String)
      return Expression_Value
   is
      use Ada.Strings.Unbounded;
      Key : constant Unbounded_String :=
              To_Unbounded_String (Name);
   begin
      if Context /= null
        and then Context.Program_Child (Name) /= null
      then
         return (Object_Value, Context.Program_Child (Name));
      else
         declare
            M : Environment := Env;
         begin
            while M /= null loop
               if M.Map.Contains (Key) then
                  return M.Map.Element (Key);
               else
                  M := M.Parent;
               end if;
            end loop;
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "undefined: " & Name);
            return (Value_Type => No_Value);
         end;
      end if;
   end Lookup;

   -------------------------
   -- Release_Environment --
   -------------------------

   procedure Release_Environment
     (Env : in out Environment)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Environment_Record, Environment);
      Writer : constant Expression_Value :=
                 Lookup (Env, "__output");
   begin
      if Writer.Value_Type = Object_Value then
         Ada.Text_IO.Set_Output
           (Ada.Text_IO.Standard_Output);
         Ada.Text_IO.Close (Aquarius_Writer (Writer.Object.all).File.all);
      end if;
      Free (Env);
   end Release_Environment;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Env    : Environment;
      Action : Aquarius.Programs.Program_Tree;
      Top    : Aquarius.Programs.Program_Tree)
   is
      Children : constant Array_Of_Program_Trees :=
                   Top.Direct_Children;
   begin
      Interpret (Env, Action, Top);
      for I in Children'Range loop
         Scan (Env, Action, Children (I));
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
              (Ada.Text_IO.Standard_Error,
               Target.Tree.Name & "."
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
               when Library_Function_Value =>
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "can't store a library function in a property yet");
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
         when Library_Function_Value =>
            return True;  -- wrong, of course
         when String_Value =>
            return Value.String_Text /= Null_Unbounded_String;
         when Object_Value =>
            return Value.Object /= null;
      end case;
   end To_Boolean;

   -------------
   -- To_Name --
   -------------

   function To_Name (Value : Expression_Value) return String is
      use Ada.Strings.Unbounded;
   begin
      case Value.Value_Type is
         when No_Value =>
            return "_";
         when Library_Function_Value =>
            return "<library_function>";
         when String_Value =>
            return To_String (Value.String_Text);
         when Object_Value =>
            return Value.Object.Name;
      end case;
   end To_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Expression_Value) return String is
      use Ada.Strings.Unbounded;
   begin
      case Value.Value_Type is
         when No_Value =>
            return "_";
         when Library_Function_Value =>
            return "<library_function>";
         when String_Value =>
            return To_String (Value.String_Text);
         when Object_Value =>
            if Value.Object.all in Program_Tree_Type'Class then
               return Program_Tree (Value.Object).Concatenate_Children;
            else
               return Value.Object.Name;
            end if;
      end case;
   end To_String;

end Aquarius.Actions.Interpreter;
