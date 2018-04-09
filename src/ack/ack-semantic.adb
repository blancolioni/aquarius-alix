with Ada.Text_IO;

with WL.String_Sets;

with Komnenos.Entities.Tables;

with Aquarius.Loader;
with Aquarius.Programs.Komnenos_Entities;

with Ack.Files;
with Ack.Parser;

with Ack.Classes;
with Ack.Features;
with Ack.Types;
with Ack.Variables;

with Ack.Environment;

with Ack.Errors;

package body Ack.Semantic is

   Trace_Class_Analysis : constant Boolean := False;

   function Load_Class
     (Referrer : Aquarius.Programs.Program_Tree;
      Parent   : not null access Root_Entity_Type'Class;
      Name     : Name_Id)
      return Ack.Classes.Class_Entity;

   function Get_Top_Level_Type
     (Name : String)
      return Ack.Types.Type_Entity;

   type Tuple_Arity_Range is range 2 .. 20;

   Tuple_Classes : array (Tuple_Arity_Range) of Ack.Classes.Class_Entity :=
                     (others => null);

   function Load_Tuple_Class
     (Arity : Tuple_Arity_Range)
      return Ack.Classes.Class_Entity;

   function Forward_Iterable return Ack.Classes.Class_Entity;

   function Property_Feature_Node
     (Node : Node_Id)
     return Boolean
     with Pre => Kind (Node) = N_Feature_Declaration;

   function Analyse_Class_Header
     (Class  : Node_Id;
      Header : Node_Id)
     return Ack.Classes.Class_Entity;

   procedure Analyse_Formal_Generics
     (Class           : Ack.Classes.Class_Entity;
      Formal_Generics : Node_Id);

   procedure Analyse_Class_Name
     (Context       : Ack.Classes.Class_Entity;
      Class_Name    : Node_Id;
      Defining_Name : Boolean);

   procedure Analyse_Notes
     (Class : Ack.Classes.Class_Entity;
      Notes : Node_Id);

   procedure Analyse_Feature_Name
     (Class    : Ack.Classes.Class_Entity;
      Exports  : Node_Id;
      Feature  : Node_Id)
     with Pre => Kind (Feature) = N_Feature_Declaration;

   procedure Analyse_Feature_Header
     (Class   : Ack.Classes.Class_Entity;
      Exports : Node_Id;
      Feature : Node_Id)
     with Pre => Kind (Feature) = N_Feature_Declaration;

   procedure Analyse_Feature_Body
     (Class   : Ack.Classes.Class_Entity;
      Exports : Node_Id;
      Feature : Node_Id)
     with Pre => Kind (Feature) = N_Feature_Declaration;

   procedure Analyse_Features
     (Class   : Ack.Classes.Class_Entity;
      Node    : Node_Id;
      Analyse : not null access
        procedure (Class : Ack.Classes.Class_Entity;
                   Exports  : Node_Id;
                   Node  : Node_Id))
     with Pre => Kind (Node) = N_Features;

   procedure Analyse_Inheritance
     (Class       : Ack.Classes.Class_Entity;
      Inheritance : Node_Id);

   procedure Analyse_Inherit
     (Class   : Ack.Classes.Class_Entity;
      Inherit : Node_Id);

   procedure Analyse_Entity_Declaration_Groups
     (Class      : Ack.Classes.Class_Entity;
      Feature    : Ack.Features.Feature_Entity;
      Group_List : Node_Id;
      Local      : Boolean);

   procedure Analyse_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) in N_Type;

   procedure Analyse_Class_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) = N_Class_Type;

   procedure Analyse_Anchored_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) = N_Anchored_Type;

   procedure Analyse_Tuple_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) = N_Tuple_Type;

   procedure Analyse_Assertion
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Assertion : Node_Id;
      Process   : not null access
        procedure (Tag : Name_Id;
                   Condition : Node_Id));

   procedure Analyse_Effective_Routine
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Routine   : Node_Id);

   procedure Analyse_Compound
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Compound  : Node_Id);

   procedure Analyse_Assignment
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Assignment : Node_Id);

   procedure Analyse_Conditional
     (Class       : Ack.Classes.Class_Entity;
      Container   : not null access Root_Entity_Type'Class;
      Conditional : Node_Id);

   procedure Analyse_Creation
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Creation  : Node_Id);

   procedure Analyse_Loop
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Loop_Node : Node_Id);

   procedure Analyse_Check
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Check     : Node_Id)
   is null;

   procedure Analyse_Retry
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Retry      : Node_Id);

   procedure Analyse_Boolean_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression      : Node_Id);

   procedure Analyse_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Expression      : Node_Id);

   procedure Analyse_Operator
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Operator_Node   : Node_Id);

   procedure Analyse_Precursor
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Precursor       : Node_Id);

   procedure Analyse_Tuple_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Expression      : Node_Id);

   procedure Analyse_Actual_Arguments
     (Class            : Ack.Classes.Class_Entity;
      Container        : not null access Root_Entity_Type'Class;
      Entity           : Entity_Type;
      Actual_List_Node : Node_Id);

   ------------------------------
   -- Analyse_Actual_Arguments --
   ------------------------------

   procedure Analyse_Actual_Arguments
     (Class            : Ack.Classes.Class_Entity;
      Container        : not null access Root_Entity_Type'Class;
      Entity           : Entity_Type;
      Actual_List_Node : Node_Id)
   is
   begin
      if Actual_List_Node /= No_Node then
         declare
            Actuals : constant Array_Of_Nodes :=
                        To_Array
                          (Node_Table.Element
                             (Actual_List_Node).List);
         begin
            if Entity.Argument_Count = 0 then
               Error (Actual_List_Node, E_Does_Not_Accept_Arguments);
            elsif Actuals'Length > Entity.Argument_Count then
               Error (Actual_List_Node, E_Too_Many_Arguments);
            elsif Actuals'Length < Entity.Argument_Count then
               Error (Actual_List_Node, E_Insufficient_Arguments);
            else
               for I in Actuals'Range loop
                  Analyse_Expression
                    (Class           => Class,
                     Container       => Container,
                     Expression_Type => Entity.Argument (I).Get_Type,
                     Expression      => Actuals (I));
               end loop;
            end if;
         end;
      end if;
   end Analyse_Actual_Arguments;

   ---------------------------
   -- Analyse_Anchored_Type --
   ---------------------------

   procedure Analyse_Anchored_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
   is
      Feature_Name : constant Name_Id :=
                       Get_Name (Type_Node);
      Type_Entity  : constant Ack.Types.Type_Entity :=
                       Ack.Types.New_Anchored_Type
                         (Type_Node, Class, Feature_Name);
   begin
      Set_Entity (Type_Node, Type_Entity);
   end Analyse_Anchored_Type;

   -----------------------
   -- Analyse_Assertion --
   -----------------------

   procedure Analyse_Assertion
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Assertion : Node_Id;
      Process   : not null access
        procedure (Tag : Name_Id;
                   Condition : Node_Id))
   is

      procedure Analyse_Clause (Clause : Node_Id);

      --------------------
      -- Analyse_Clause --
      --------------------

      procedure Analyse_Clause (Clause : Node_Id) is
      begin
         Analyse_Boolean_Expression (Class, Container, Expression (Clause));
         Process (Get_Name (Clause), Expression (Clause));
      end Analyse_Clause;

   begin
      Scan (Assertion_Clauses (Assertion), Analyse_Clause'Access);
   end Analyse_Assertion;

   ------------------------
   -- Analyse_Assignment --
   ------------------------

   procedure Analyse_Assignment
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Assignment : Node_Id)
   is
      Target : constant String :=
                 To_Standard_String (Get_Name (Variable (Assignment)));
   begin
      if Container.Contains (Target) then
         declare
            Entity : constant Ack.Entity_Type :=
                       Container.Get (Target);
         begin
            Analyse_Expression (Class, Container, Entity.Get_Type,
                                Expression (Assignment));
            Set_Entity (Variable (Assignment), Entity);
         end;
      else
         Error (Variable (Assignment), E_Undeclared_Name);
      end if;

   end Analyse_Assignment;

   --------------------------------
   -- Analyse_Boolean_Expression --
   --------------------------------

   procedure Analyse_Boolean_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression      : Node_Id)
   is
   begin
      Analyse_Expression
        (Class, Container,
         Get_Top_Level_Type ("boolean"),
         Expression);
   end Analyse_Boolean_Expression;

   -------------------------------
   -- Analyse_Class_Declaration --
   -------------------------------

   procedure Analyse_Class_Declaration
     (Node : Node_Id)
   is
      Notes_Node       : constant Node_Id := Notes (Node);
      Inheritance_Node : constant Node_Id := Inheritance (Node);
      Creators_Node    : constant Node_Id := Class_Creators (Node);
      Features_Node    : constant Node_Id := Class_Features (Node);
      Class            : constant Ack.Classes.Class_Entity :=
                           Analyse_Class_Header (Node, Class_Header (Node));
   begin

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Analysing: " & Class.Qualified_Name);
      end if;

      if Notes_Node in Real_Node_Id then
         Analyse_Notes (Class, Notes_Node);
      end if;

      if Creators_Node in Real_Node_Id then
         declare
            procedure Add_Creator_Clause
              (Node : Node_Id);

            ------------------------
            -- Add_Creator_Clause --
            ------------------------

            procedure Add_Creator_Clause
              (Node : Node_Id)
            is
               procedure Add_Creator_Name (Creator_Node : Node_Id);

               ----------------------
               -- Add_Creator_Name --
               ----------------------

               procedure Add_Creator_Name (Creator_Node : Node_Id) is
               begin
                  Class.Add_Creator
                    (Get_Name (Creator_Node));
               end Add_Creator_Name;

            begin
               Scan (Creator_List (Node), Add_Creator_Name'Access);
            end Add_Creator_Clause;

         begin
            Scan (Creator_Clauses (Creators_Node),
                  Add_Creator_Clause'Access);
         end;
      end if;

      if Features_Node in Real_Node_Id then
         if Trace_Class_Analysis then
            Ada.Text_IO.Put_Line
              ("Analysing feature names: " & Class.Qualified_Name);
         end if;

         Analyse_Features (Class, Features_Node,
                           Analyse_Feature_Name'Access);
      end if;

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Analysing inheritance: " & Class.Qualified_Name);
      end if;

      if Inheritance_Node /= No_Node then
         Analyse_Inheritance (Class, Inheritance_Node);
      elsif Class.Standard_Name /= "any" then
         Class.Inherit
           (Ack.Types.New_Class_Type
              (Node       => Node,
               Class      =>
                 Load_Class
                   (Get_Program (Node), Ack.Environment.Top_Level,
                    Get_Name_Id ("Any")),
               Detachable => False));
      end if;

      if Features_Node in Real_Node_Id then
         if Trace_Class_Analysis then
            Ada.Text_IO.Put_Line
              ("Analysing feature headers: " & Class.Qualified_Name);
         end if;
         Analyse_Features
           (Class, Features_Node, Analyse_Feature_Header'Access);
      end if;

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("  binding: " & Class.Qualified_Name);
      end if;

      Class.Bind;

      if Features_Node in Real_Node_Id then
         if Trace_Class_Analysis then
            Ada.Text_IO.Put_Line
              ("  feature bodies: " & Class.Qualified_Name);
         end if;
         Analyse_Features (Class, Features_Node,
                           Analyse_Feature_Body'Access);
      end if;

      if not Class.Deferred then
         declare
            procedure Check_Effective
              (Feature : not null access constant
                 Ack.Features.Feature_Entity_Record'Class);

            ---------------------
            -- Check_Effective --
            ---------------------

            procedure Check_Effective
              (Feature : not null access constant
                 Ack.Features.Feature_Entity_Record'Class)
            is
            begin
               if Feature.Deferred then
                  Error
                    (Node   => Class.Declaration_Node,
                     Kind   => E_Requires_Definition,
                     Entity => Constant_Entity_Type (Feature));
               end if;
            end Check_Effective;

         begin
            Class.Scan_Features (Check_Effective'Access);
         end;
      end if;

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("  virtual and object tables: " & Class.Qualified_Name);
      end if;

      Class.Create_Memory_Layout;

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Finished: " & Class.Qualified_Name);
      end if;

   end Analyse_Class_Declaration;

   --------------------------
   -- Analyse_Class_Header --
   --------------------------

   function Analyse_Class_Header
     (Class  : Node_Id;
      Header : Node_Id)
      return Ack.Classes.Class_Entity
   is
      Formal_Generics_Node : constant Node_Id := Formal_Generics (Header);
      Result : Ack.Classes.Class_Entity;
   begin
      Analyse_Class_Name (null, Class_Name (Header),
                          Defining_Name => True);
      Result := Ack.Classes.Get_Class_Entity (Class_Name (Header));

      if Node_Table.Element (Header).Deferred then
         Result.Set_Deferred;
      elsif Node_Table.Element (Header).Expanded then
         Result.Set_Expanded;
      elsif Node_Table.Element (Header).Frozen then
         Result.Set_Frozen;
      end if;

      Set_Entity (Class, Result);
      if Formal_Generics_Node /= No_Node then
         Analyse_Formal_Generics (Result, Formal_Generics_Node);
      end if;
      return Result;
   end Analyse_Class_Header;

   ------------------------
   -- Analyse_Class_Name --
   ------------------------

   procedure Analyse_Class_Name
     (Context       : Ack.Classes.Class_Entity;
      Class_Name    : Node_Id;
      Defining_Name : Boolean)
   is
      use Ack.Classes;
      use type List_Of_Nodes.Cursor;
      List : constant List_Of_Nodes.List :=
               List_Table.Element (Identifiers (Class_Name)).List;
      Position : List_Of_Nodes.Cursor := List.First;
      Class_Context : Class_Entity := Context;
      Last : constant List_Of_Nodes.Cursor :=
               (if Defining_Name
                then List.Last
                else List_Of_Nodes.No_Element);

      Parent        : Ack.Classes.Class_Entity;
      Referrer : constant Aquarius.Programs.Program_Tree :=
                        Get_Program (Class_Name);
   begin

      if not Defining_Name
        and then Natural (List.Length) = 1
      then
         --  this might be a local type, not a class
         declare
            Local_Node : constant Node_Id := List.First_Element;
            Local_Name : constant Name_Id := Get_Name (Local_Node);
         begin
            if Context.Contains (Local_Name) then
               if Has_Entity (Class_Name) then
                  raise Constraint_Error with
                  Get_Program (Local_Node).Show_Location
                    & "node already has an entity: "
                    & Get_Entity (Class_Name).Description;
               end if;
               Set_Entity (Class_Name, Context.Get (Local_Name));
               return;
            end if;
         end;
      end if;

      if Defining_Name then
         Parent := null;
      else
         Parent := null;
         declare
            Element_Node : constant Node_Id :=
                             List_Of_Nodes.Element (Position);
            Element_Name : constant Name_Id :=
                             Node_Table.Element (Element_Node).Name;
         begin
            while Class_Context /= null loop
               Parent :=
                 Load_Class (Referrer, Class_Context, Element_Name);
               exit when Parent /= null;
               Class_Context := Class_Context.Class_Declaration_Context;
            end loop;
            if Parent = null then
               Parent :=
                 Load_Class (Referrer, Ack.Environment.Top_Level,
                             Element_Name);
            end if;
         end;

         if Parent = null then
            Error (List_Of_Nodes.Element (Position), E_Undeclared_Name);
            return;
         end if;

         List_Of_Nodes.Next (Position);

      end if;

      while Position /= Last loop
         declare
            Element_Node : constant Node_Id :=
                             List_Of_Nodes.Element (Position);
            Element_Name : constant Name_Id :=
                             Node_Table.Element (Element_Node).Name;
            New_Parent   : constant Class_Entity :=
                             Load_Class
                               (Referrer        => Referrer,
                                Parent          =>
                                  (if Parent = null
                                   then Ack.Environment.Top_Level
                                   else Parent),
                                Name            => Element_Name);
         begin
            if New_Parent = null then
               Error (Element_Node,
                      (if Parent = null
                       then E_Undeclared_Name
                       else E_No_Child));
               Parent :=
                 Ack.Classes.New_Class
                   (Element_Name, Parent, Element_Node);
            else
               Parent := New_Parent;
            end if;
         end;
         List_Of_Nodes.Next (Position);
      end loop;

      if Defining_Name then
         declare
            Last_Node : constant Node_Id :=
                          List_Of_Nodes.Element (Last);
            Last_Name : constant Name_Id :=
                          Node_Table.Element (Last_Node).Name;
            New_Class : constant Class_Entity :=
                          Ack.Classes.New_Class
                            (Last_Name, Parent, Last_Node);
         begin
            if Parent = null then
               Ack.Environment.Top_Level.Insert (New_Class);
            else
               Parent.Insert (New_Class);
            end if;
            Set_Entity (Last_Node, New_Class);
            Parent := New_Class;
         exception
            when others =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Get_Program (Last_Node).Show_Location
                  & ": unhandled exception while defining "
                  & To_String (Last_Name));
               raise;
         end;
      end if;

      Set_Entity (Class_Name, Parent);

   end Analyse_Class_Name;

   ------------------------
   -- Analyse_Class_Type --
   ------------------------

   procedure Analyse_Class_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
   is
      use type Ack.Types.Type_Entity;
      Name_Node     : constant Node_Id := Class_Name (Type_Node);
      Generics_Node : constant Node_Id := Actual_Generics (Type_Node);
      Class_Entity  : Ack.Classes.Class_Entity := null;
      Type_Entity   : Ack.Types.Type_Entity := null;

   begin
      Analyse_Class_Name (Class, Name_Node, False);

      if not Has_Entity (Name_Node) then
         return;
      end if;

      if Ack.Types.Has_Type_Entity (Name_Node) then
         Type_Entity := Ack.Types.Get_Type_Entity (Name_Node);
      elsif Ack.Classes.Has_Class_Entity (Name_Node) then
         Class_Entity := Ack.Classes.Get_Class_Entity (Name_Node);

         if Generics_Node = No_Node then
            Type_Entity :=
              Ack.Types.New_Class_Type
                (Type_Node, Class_Entity,
                 Detachable => Node_Table.Element (Type_Node).Detachable);
         else
            declare
               Actual_Nodes : constant Array_Of_Nodes :=
                                To_Array
                                  (Actual_Generics_List (Generics_Node));
               Actual_Count : constant Natural :=
                                Actual_Nodes'Length;
               Actual_Types : Ack.Types.Array_Of_Types (Actual_Nodes'Range);
            begin
               if Actual_Count < Class_Entity.Generic_Formal_Count then
                  Error (Type_Node, E_Insufficient_Arguments);
               elsif Actual_Count > Class_Entity.Generic_Formal_Count then
                  Error (Type_Node, E_Too_Many_Arguments);
               else
                  for I in Actual_Nodes'Range loop
                     Analyse_Type (Class, Actual_Nodes (I));
                     Actual_Types (I) :=
                       Ack.Types.Get_Type_Entity (Actual_Nodes (I));
                  end loop;

                  Type_Entity :=
                    Ack.Types.Instantiate_Generic_Class
                      (Node            => Type_Node,
                       Generic_Class   => Class_Entity,
                       Generic_Actuals => Actual_Types,
                       Detachable      =>
                         Node_Table.Element (Type_Node).Detachable);
               end if;
            end;
         end if;
      end if;

      if Type_Entity /= null then
         Set_Entity (Type_Node, Type_Entity);
      else
         Set_Entity (Type_Node, Get_Top_Level_Type ("any"));
      end if;

   end Analyse_Class_Type;

   ----------------------
   -- Analyse_Compound --
   ----------------------

   procedure Analyse_Compound
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Compound  : Node_Id)
   is
      List : constant List_Id := Instructions (Compound);

      procedure Analyse (Node : Node_Id);

      -------------
      -- Analyse --
      -------------

      procedure Analyse (Node : Node_Id) is
      begin
         case N_Instruction (Kind (Node)) is
            when N_Assignment =>
               Analyse_Assignment (Class, Container, Node);
            when N_Creation_Instruction =>
               Analyse_Creation (Class, Container, Node);
            when N_Conditional =>
               Analyse_Conditional (Class, Container, Node);
            when N_Loop =>
               Analyse_Loop (Class, Container, Node);
            when N_Precursor =>
               Analyse_Precursor
                 (Class           => Class,
                  Container       => Container,
                  Expression_Type => null,
                  Precursor       => Node);
            when N_Check =>
               Analyse_Check (Class, Container, Node);
            when N_Retry =>
               Analyse_Retry (Class, Container, Node);
         end case;
      end Analyse;

   begin
      Scan (List, Analyse'Access);
   end Analyse_Compound;

   -------------------------
   -- Analyse_Conditional --
   -------------------------

   procedure Analyse_Conditional
     (Class       : Ack.Classes.Class_Entity;
      Container   : not null access Root_Entity_Type'Class;
      Conditional : Node_Id)
   is
      procedure Analyse_Element (Element : Node_Id);

      ---------------------
      -- Analyse_Element --
      ---------------------

      procedure Analyse_Element (Element : Node_Id) is
         Condition : constant Node_Id := Field_1 (Element);
         Compound  : constant Node_Id := Field_2 (Element);
      begin
         if Condition /= No_Node then
            Analyse_Boolean_Expression
              (Class, Container,
               Condition);
            if Implicit_Entity (Condition) then
               Container.Add_Implicit (Get_Entity (Condition));
            end if;
         end if;
         Analyse_Compound (Class, Container, Compound);
         if Condition /= No_Node and then Implicit_Entity (Condition) then
            Container.Remove_Implicit;
         end if;
      end Analyse_Element;

   begin
      Scan (Node_Table.Element (Conditional).List,
            Analyse_Element'Access);
   end Analyse_Conditional;

   ----------------------
   -- Analyse_Creation --
   ----------------------

   procedure Analyse_Creation
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Creation  : Node_Id)
   is
      Explicit_Type_Node : constant Node_Id :=
                             Explicit_Creation_Type (Creation);
      Call_Node          : constant Node_Id := Creation_Call (Creation);
      Explicit_Call_Node : constant Node_Id :=
                             Explicit_Creation_Call (Call_Node);
      Variable_Node      : constant Node_Id := Variable (Call_Node);
      Name               : constant Name_Id := Get_Name (Variable_Node);
      Created_Entity     : constant Entity_Type :=
                             (if Container.Contains (Name)
                              then Container.Get (Name) else null);
      Created_Type       : Entity_Type;
      Creator_Name       : Name_Id;
   begin

      if Created_Entity = null then
         Error (Variable_Node, E_Undeclared_Name);
         return;
      end if;

      Set_Context (Creation, Container);
      Set_Entity (Creation, Created_Entity);

      Created_Type := Created_Entity.Get_Type;

      if Explicit_Type_Node in Real_Node_Id then
         Analyse_Type (Class, Explicit_Type_Node);
         if Get_Entity (Explicit_Type_Node) /= null then
            Created_Type := Get_Entity (Explicit_Type_Node);
            if Created_Type.Deferred then
               Error (Explicit_Type_Node, E_Create_Deferred_Class);
            end if;
         end if;
      elsif Created_Type.Deferred then
         Error (Creation, E_Create_Deferred_Class);
      end if;

      if Explicit_Call_Node in Real_Node_Id then
         Creator_Name := Get_Name (Explicit_Call_Node);
         if Created_Type.Contains (Creator_Name) then
            declare
               Creator : constant Entity_Type :=
                           Created_Type.Get (Creator_Name);
            begin
               Set_Entity (Explicit_Call_Node, Creator);
               if Creator.all not in Ack.Features.Feature_Entity_Record'Class
                 or else not Ack.Features.Feature_Entity (Creator).Is_Creator
               then
                  Error (Explicit_Call_Node, E_Not_A_Create_Feature,
                         Created_Type);
               else
                  Analyse_Actual_Arguments
                    (Class            => Class,
                     Container        => Container,
                     Entity           => Creator,
                     Actual_List_Node => Actual_List (Explicit_Call_Node));
               end if;
            end;
         else
            Error (Explicit_Call_Node, E_Not_Defined_In,
                   Created_Type);
         end if;
      end if;

   end Analyse_Creation;

   -------------------------------
   -- Analyse_Effective_Routine --
   -------------------------------

   procedure Analyse_Effective_Routine
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Routine   : Node_Id)
   is
   begin
      case N_Effective_Routine (Kind (Routine)) is
         when N_Internal =>
            Analyse_Compound (Class, Container, Compound (Routine));
         when N_External =>
            null;
      end case;
   end Analyse_Effective_Routine;

   ---------------------------------------
   -- Analyse_Entity_Declaration_Groups --
   ---------------------------------------

   procedure Analyse_Entity_Declaration_Groups
     (Class      : Ack.Classes.Class_Entity;
      Feature    : Ack.Features.Feature_Entity;
      Group_List : Node_Id;
      Local      : Boolean)
   is

      procedure Insert_Group (Group_Node : Node_Id);

      ------------------
      -- Insert_Group --
      ------------------

      procedure Insert_Group (Group_Node : Node_Id) is
         Ids         : constant List_Id := Identifiers (Group_Node);
         Type_Node   : constant Node_Id := Group_Type (Group_Node);
         Type_Entity : Ack.Types.Type_Entity := null;

         procedure Insert_Id (Id_Node : Node_Id);

         ---------------
         -- Insert_Id --
         ---------------

         procedure Insert_Id (Id_Node : Node_Id) is
         begin
            if Local then
               Feature.Add_Local
                 (Id_Node, Type_Entity);
            else
               Feature.Add_Argument
                 (Id_Node, Type_Entity);
            end if;
         end Insert_Id;

      begin
         if Type_Node /= No_Node then
            Analyse_Type (Class, Type_Node);
            if Has_Entity (Type_Node) then
               Type_Entity := Ack.Types.Get_Type_Entity (Type_Node);
               Scan (Ids, Insert_Id'Access);
            end if;
         else
            Ada.Text_IO.Put_Line
              ("null type node for "
               & Class.Qualified_Name
               & "." & Feature.Declared_Name
               & " argument");
         end if;

      end Insert_Group;

   begin
      Scan (Node_Table.Element (Group_List).List, Insert_Group'Access);
   end Analyse_Entity_Declaration_Groups;

   ------------------------
   -- Analyse_Expression --
   ------------------------

   procedure Analyse_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Expression      : Node_Id)
   is
      K : constant Node_Kind := Kind (Expression);
   begin
      case N_Expression_Node (K) is
         when N_Operator =>
            Analyse_Operator
              (Class, Container, Expression_Type, Expression);
         when N_Precursor =>
            Analyse_Precursor
              (Class, Container, Expression_Type, Expression);
         when N_Old =>
            Analyse_Expression (Class, Container, Expression_Type,
                                Ack.Expression (Expression));
            Container.Save_Old_Value (Ack.Expression (Expression));
         when N_Tuple =>
            Analyse_Tuple_Expression
              (Class, Container, Expression_Type, Expression);
         when N_Attachment_Test =>
            Analyse_Expression (Class, Container,
                                Get_Top_Level_Type ("any"),
                                Expression => Field_1 (Expression));
            if Get_Name (Expression) /= No_Name then
               declare
                  Implicit : constant Ack.Variables.Variable_Entity :=
                               Ack.Variables.New_Local_Entity
                                 (Name       => Get_Name (Expression),
                                  Node       => Expression,
                                  Local_Type =>
                                    Get_Type (Field_1 (Expression)));
               begin
                  Implicit.Set_Attached;
                  Set_Type (Expression,
                            Ack.Types.Get_Top_Level_Type ("boolean"));
                  Set_Entity (Expression, Implicit);
                  Set_Implicit_Entity (Expression);
               end;
            end if;

         when N_Constant =>
            declare
               use type Ack.Classes.Class_Entity;
               Value     : constant Node_Id := Constant_Value (Expression);
               Type_Name : constant String :=
                              (case N_Constant_Value (Kind (Value)) is
                                  when N_String_Constant  =>
                                     "string",
                                  when N_Integer_Constant =>
                                     "integer",
                                  when N_Boolean_Constant =>
                                     "boolean");
               Value_Type : constant Ack.Types.Type_Entity :=
                              Get_Top_Level_Type (Type_Name);
            begin
               Set_Type (Expression, Value_Type);
               Set_Entity (Expression, Value_Type);
               if Expression_Type = null then
                  Error (Value, E_Ignored_Return_Value);
               elsif not  Value_Type.Conforms_To (Expression_Type) then
                  Error (Expression, E_Type_Error,
                         Entity_Type (Expression_Type));
               end if;
            end;
      end case;

   end Analyse_Expression;

   --------------------------
   -- Analyse_Feature_Body --
   --------------------------

   procedure Analyse_Feature_Body
     (Class   : Ack.Classes.Class_Entity;
      Exports : Node_Id;
      Feature : Node_Id)
   is
      pragma Unreferenced (Exports);
      Names           : constant List_Id := New_Feature_List (Feature);
      Dec_Body        : constant Node_Id := Declaration_Body (Feature);
      Value_Node      : constant Node_Id := Value (Dec_Body);
      Value_Feature   : constant Boolean :=
                          Value_Node /= No_Node
                              and then Kind (Value_Node) = N_Explicit_Value;
      Routine_Feature : constant Boolean :=
                          Value_Node /= No_Node
                              and then Kind (Value_Node) = N_Routine;
      Deferred        : constant Boolean :=
                          Routine_Feature
                              and then Node_Table.Element
                                (Value_Node).Deferred;
      Effective       : constant Boolean :=
                          Routine_Feature and then not Deferred
                                and then Kind (Value_Node) = N_Routine;
      Precondition_Node : constant Node_Id :=
                            (if Routine_Feature
                             then Precondition (Value_Node)
                             else No_Node);
      Postcondition_Node : constant Node_Id :=
                             (if Routine_Feature
                              then Postcondition (Value_Node)
                              else No_Node);
      Rescue_Node        : constant Node_Id :=
                             (if Routine_Feature
                              then Rescue (Value_Node)
                              else No_Node);
      Locals_Node        : constant Node_Id :=
                             (if Value_Node = No_Node then No_Node
                              else Local_Declarations (Value_Node));
      Effective_Node     : constant Node_Id :=
                             (if Effective then Effective_Routine (Value_Node)
                           else No_Node);
      Internal           : constant Boolean :=
                             Effective
                                 and then Kind (Effective_Node) = N_Internal;
      External           : constant Boolean :=
                          Effective
                                 and then Kind (Effective_Node) = N_External;
   begin

      for Node of List_Table.Element (Names).List loop
         declare
            Entity : constant Ack.Features.Feature_Entity :=
                       Ack.Features.Get_Feature_Entity (Node);
         begin

            if Locals_Node /= No_Node then
               Analyse_Entity_Declaration_Groups
                 (Class      => Class,
                  Feature    => Entity,
                  Group_List => Entity_Declaration_Group_List (Locals_Node),
                  Local      => True);
            end if;

            if Entity.Standard_Name = "void" then
               Entity.Set_Explicit_Value (No_Node);
            elsif Value_Feature then
               Entity.Set_Explicit_Value
                 (Constant_Value (Value_Node));
            elsif Deferred then
               Entity.Set_Deferred;
            elsif Internal then
               Entity.Set_Routine (Effective_Node);
            elsif External then
               Entity.Set_External
                 (External_Type  =>
                    To_Standard_String (Get_Name (Effective_Node)),
                  External_Alias =>
                    (if Feature_Alias (Effective_Node) = No_Node
                     then Entity.Standard_Name
                     else To_String
                       (Get_Name (Feature_Alias (Effective_Node)))));
            end if;

            Entity.Bind;

            if Precondition_Node /= No_Node then
               declare
                  procedure Add_Precondition
                    (Tag        : Name_Id;
                     Expression : Node_Id);

                  ----------------------
                  -- Add_Precondition --
                  ----------------------

                  procedure Add_Precondition
                    (Tag        : Name_Id;
                     Expression : Node_Id)
                  is
                  begin
                     Entity.Add_Precondition (Tag, Expression);
                  end Add_Precondition;

               begin
                  Analyse_Assertion (Class, Entity,
                                     Assertion (Precondition_Node),
                                     Add_Precondition'Access);
               end;
            end if;

            if Postcondition_Node /= No_Node then
               declare
                  procedure Add_Postcondition
                    (Tag        : Name_Id;
                     Expression : Node_Id);

                  -----------------------
                  -- Add_Postcondition --
                  -----------------------

                  procedure Add_Postcondition
                    (Tag        : Name_Id;
                     Expression : Node_Id)
                  is
                  begin
                     Entity.Add_Postcondition (Tag, Expression);
                  end Add_Postcondition;

               begin
                  Analyse_Assertion (Class, Entity,
                                     Assertion (Postcondition_Node),
                                     Add_Postcondition'Access);
               end;
            end if;

            if Rescue_Node /= No_Node then
               Analyse_Compound (Class, Entity, Compound (Rescue_Node));
               Entity.Set_Rescue_Node (Rescue_Node);
            end if;

            if Internal then
               Analyse_Effective_Routine (Class, Entity, Effective_Node);
            end if;

         end;
      end loop;
   end Analyse_Feature_Body;

   ----------------------------
   -- Analyse_Feature_Header --
   ----------------------------

   procedure Analyse_Feature_Header
     (Class   : Ack.Classes.Class_Entity;
      Exports : Node_Id;
      Feature : Node_Id)
   is
      pragma Unreferenced (Exports);
      Names        : constant List_Id := New_Feature_List (Feature);
      Dec_Body     : constant Node_Id := Declaration_Body (Feature);
      Arg_Node     : constant Node_Id := Formal_Arguments (Dec_Body);
      Type_Node    : constant Node_Id := Value_Type (Dec_Body);
   begin
      if Type_Node /= No_Node then
         Analyse_Type (Class, Type_Node);
      end if;

      for Node of List_Table.Element (Names).List loop
         if not Ack.Features.Has_Feature_Entity (Node) then
            raise Constraint_Error with
            Get_Program (Node).Show_Location
              & ": expected an entity in "
              & To_String (Get_Name (Feature_Name (Node)));
         end if;

         declare
            Entity    : constant Ack.Features.Feature_Entity :=
                       Ack.Features.Get_Feature_Entity (Node);
         begin
            if Arg_Node /= No_Node then
               Analyse_Entity_Declaration_Groups
                 (Class, Entity,
                  Entity_Declaration_Group_List (Arg_Node),
                  Local => False);
            end if;

            if Type_Node /= No_Node
              and then Has_Entity (Type_Node)
            then
               Entity.Set_Result_Type
                 (Ack.Types.Get_Type_Entity (Type_Node));
            end if;
         end;
      end loop;
   end Analyse_Feature_Header;

   --------------------------
   -- Analyse_Feature_Name --
   --------------------------

   procedure Analyse_Feature_Name
     (Class    : Ack.Classes.Class_Entity;
      Exports  : Node_Id;
      Feature  : Node_Id)
   is
      pragma Unreferenced (Exports);
      Names        : constant List_Id := New_Feature_List (Feature);
   begin
      for Node of List_Table.Element (Names).List loop
         declare
            Extended_Name_Node : constant Node_Id :=
                                   Extended_Feature_Name (Node);
            Name_Node          : constant Node_Id :=
                                   Feature_Name (Extended_Name_Node);
            Alias_Node         : constant Node_Id :=
                                   Feature_Alias (Extended_Name_Node);
            Alias              : constant Name_Id :=
                                   (if Alias_Node = No_Node then No_Name
                                    else Get_Name (Alias_Node));
            Entity             : constant Ack.Features.Feature_Entity :=
                                   Ack.Features.New_Feature
                                     (Name        => Get_Name (Name_Node),
                                      Alias       => Alias,
                                      Declaration => Node,
                                      Property    =>
                                        Property_Feature_Node (Feature),
                                      Class       => Class);
         begin
            if Entity.Standard_Name = "void"
              and then Class.Standard_Name /= "any"
            then
               Error (Name_Node, E_Illegal_Redefinition);
            end if;

            Ack.Features.Set_Feature_Entity (Node, Entity);
            Class.Add_Feature (Entity);
         end;
      end loop;
   end Analyse_Feature_Name;

   ----------------------
   -- Analyse_Features --
   ----------------------

   procedure Analyse_Features
     (Class   : Ack.Classes.Class_Entity;
      Node    : Node_Id;
      Analyse : not null access
        procedure (Class : Ack.Classes.Class_Entity;
                   Exports  : Node_Id;
                   Node  : Node_Id))
   is
      Clause_List : constant List_Id :=
                      Feature_Clauses (Node);
   begin
      for Clause_Node of List_Table.Element (Clause_List).List loop
         declare
            Feature_List : constant List_Id :=
                             Feature_Declarations (Clause_Node);
         begin
            for Feature_Node of List_Table.Element (Feature_List).List loop
               Analyse (Class    => Class,
                        Exports  => No_Node,
                        Node     => Feature_Node);
            end loop;
         end;
      end loop;
   end Analyse_Features;

   -----------------------------
   -- Analyse_Formal_Generics --
   -----------------------------

   procedure Analyse_Formal_Generics
     (Class           : Ack.Classes.Class_Entity;
      Formal_Generics : Node_Id)
   is
      procedure Analyse_Formal_Generic (Node : Node_Id);

      ----------------------------
      -- Analyse_Formal_Generic --
      ----------------------------

      procedure Analyse_Formal_Generic (Node : Node_Id) is
         Name : constant Name_Id :=
                  Get_Name (Formal_Generic_Name (Node));
         Generic_Entity : constant Ack.Types.Type_Entity :=
                            Ack.Types.New_Generic_Formal_Type
                              (Name, Node, Class);
      begin
         Set_Entity (Node, Generic_Entity);
         Class.Add_Generic_Formal (Generic_Entity);
      end Analyse_Formal_Generic;

   begin
      Scan (Formal_Generics_List (Formal_Generics),
            Analyse_Formal_Generic'Access);
   end Analyse_Formal_Generics;

   ---------------------
   -- Analyse_Inherit --
   ---------------------

   procedure Analyse_Inherit
     (Class   : Ack.Classes.Class_Entity;
      Inherit : Node_Id)
   is
      Class_Type      : constant Node_Id := Inherit_Class_Type (Inherit);
      Redefine_List   : constant List_Id := Redefine (Inherit);

      Inherited_Type  : Ack.Types.Type_Entity;
      Inherited_Class : Ack.Classes.Class_Entity;

      Redefined_Features : WL.String_Sets.Set;

      procedure Set_Redefine (Node : Node_Id);

      procedure Check_Redefined
        (Feature : not null access constant
           Ack.Features.Feature_Entity_Record'Class);

      ---------------------
      -- Check_Redefined --
      ---------------------

      procedure Check_Redefined
        (Feature : not null access constant
           Ack.Features.Feature_Entity_Record'Class)
      is
      begin
         if not Redefined_Features.Contains (Feature.Standard_Name) then
            Error (Inherit, E_Missing_Redefine, Feature);
         end if;
      end Check_Redefined;

      ------------------
      -- Set_Redefine --
      ------------------

      procedure Set_Redefine (Node : Node_Id) is
         Name : constant Name_Id := Get_Name (Node);
      begin
         Redefined_Features.Insert (To_Standard_String (Name));
         if Inherited_Class.Has_Feature (Name) then
            if Class.Has_Feature (Name) then
               Class.Feature (Name).Set_Redefined
                 (Class            => Class,
                  Original_Feature =>
                    Inherited_Class.Feature (Name));
            else
               Error (Node, E_Missing_Redefinition,
                      Entity_Type (Inherited_Class));
            end if;
         else
            Error (Node, E_Not_Defined_In, Entity_Type (Inherited_Class));
         end if;
      end Set_Redefine;

   begin

      Analyse_Class_Type (Class, Class_Type);

      if Ack.Types.Has_Type_Entity (Class_Type) then
         Inherited_Type := Ack.Types.Get_Type_Entity (Class_Type);
         Inherited_Class := Inherited_Type.Class;

         Set_Entity (Inherit, Inherited_Type);

         Class.Inherit (Inherited_Type);
         Scan (Redefine_List, Set_Redefine'Access);

         if not Class.Deferred then
            Inherited_Class.Scan_Deferred_Features
              (Check_Redefined'Access);
         end if;
      end if;

   end Analyse_Inherit;

   -------------------------
   -- Analyse_Inheritance --
   -------------------------

   procedure Analyse_Inheritance
     (Class       : Ack.Classes.Class_Entity;
      Inheritance : Node_Id)
   is
      procedure Analyse (Node : Node_Id);

      -------------
      -- Analyse --
      -------------

      procedure Analyse (Node : Node_Id) is
      begin
         Analyse_Inherit (Class, Node);
      end Analyse;

   begin
      Scan (Inherits (Inheritance), Analyse'Access);
   end Analyse_Inheritance;

   ------------------
   -- Analyse_Loop --
   ------------------

   procedure Analyse_Loop
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Loop_Node : Node_Id)
   is
      Iteration_Node      : constant Node_Id := Loop_Iteration (Loop_Node);
      Initialization_Node : constant Node_Id :=
                              Loop_Initialization (Loop_Node);
      Exit_Condition_Node : constant Node_Id :=
                              Loop_Exit_Condition (Loop_Node);
      Loop_Body_Node      : constant Node_Id := Loop_Body (Loop_Node);
   begin
      if Iteration_Node /= No_Node then
         declare
            Expression_Node : constant Node_Id := Expression (Iteration_Node);
            Expression_Type : constant Ack.Types.Type_Entity :=
                                Ack.Types.New_Class_Type
                                  (Iteration_Node, Forward_Iterable, False);
         begin
            Analyse_Expression
              (Class, Container, Expression_Type, Expression_Node);

            declare
               use Ack.Types;
               Iterable_Type  : constant Type_Entity :=
                                  Type_Entity
                                    (Get_Type (Expression_Node));
               Inherited_Type           : constant access constant
                 Type_Entity_Record'Class :=
                   Iterable_Type.Get_Ancestor_Type
                     (Forward_Iterable);
               Implicit_Type      : constant Ack.Types.Type_Entity :=
                                      Inherited_Type.Generic_Binding (1);
               Implicit           : constant Ack.Variables.Variable_Entity :=
                                      Ack.Variables.New_Iterator_Entity
                                        (Name       =>
                                                     Get_Name (Iteration_Node),
                                         Node       => Iteration_Node,
                                         Local_Type => Implicit_Type);
            begin
               Implicit.Set_Attached;
               Set_Entity (Iteration_Node, Implicit);
               Set_Implicit_Entity (Iteration_Node);
               Container.Add_Implicit (Implicit);
            end;
         end;
      else
         if Exit_Condition_Node = No_Node then
            Error (Loop_Node, E_Missing_Exit_Condition);
         end if;
      end if;

      if Initialization_Node /= No_Node then
         Analyse_Compound (Class, Container, Compound (Initialization_Node));
      end if;

      if Exit_Condition_Node /= No_Node then
         Analyse_Boolean_Expression
           (Class, Container,
            Expression (Exit_Condition_Node));
      end if;

      Analyse_Compound (Class, Container, Compound (Loop_Body_Node));

      if Iteration_Node /= No_Node then
         Container.Remove_Implicit;
      end if;

   end Analyse_Loop;

   -------------------
   -- Analyse_Notes --
   -------------------

   procedure Analyse_Notes
     (Class : Ack.Classes.Class_Entity;
      Notes : Node_Id)
   is
      List : constant List_Id := Node_Table.Element (Notes).List;

      function To_String
        (List : List_Id)
         return String;

      ---------------
      -- To_String --
      ---------------

      function To_String
        (List : List_Id)
         return String
      is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;

         procedure Add_Note_Item
           (Note_Item : Node_Id)
           with Pre => Kind (Note_Item) = N_Note_Item;

         procedure Add_Note_Text
           (Text : String);

         -------------------
         -- Add_Note_Item --
         -------------------

         procedure Add_Note_Item
           (Note_Item : Node_Id)
         is
            Item : constant Node_Id := Field_1 (Note_Item);
         begin
            if Kind (Item) = N_Identifier then
               Add_Note_Text (To_String (Get_Name (Item)));
            elsif Kind (Item) = N_Constant then
               declare
                  Value : constant Node_Id := Field_2 (Item);
               begin
                  case N_Constant_Value (Kind (Value)) is
                     when N_String_Constant =>
                        Add_Note_Text (To_String (Get_Name (Value)));
                     when N_Integer_Constant =>
                        Add_Note_Text (To_String (Get_Name (Value)));
                     when N_Boolean_Constant =>
                        Add_Note_Text (if Boolean_Value (Value)
                                       then "True" else "False");
                  end case;
               end;
            else
               raise Constraint_Error with
               Get_Program (Item).Show_Location
                 & ": expected an identifier or a constant, but found "
                 & Node_Kind'Image (Kind (Item));
            end if;
         end Add_Note_Item;

         -------------------
         -- Add_Note_Text --
         -------------------

         procedure Add_Note_Text
           (Text : String)
         is
         begin
            if Result = Null_Unbounded_String then
               Result := To_Unbounded_String (Text);
            else
               Result := Result & Character'Val (10)
                 & Text;
            end if;
         end Add_Note_Text;

      begin
         Scan (List, Add_Note_Item'Access);
         return To_String (Result);
      end To_String;

   begin
      for Note of List_Table.Element (List).List loop
         declare
            Name  : constant Name_Id := Get_Name (Note_Name (Note));
            Value : constant Node_Id := Note_Value (Note);
            Value_List : constant List_Id :=
                           Node_Table.Element (Value).List;
         begin
            Class.Add_Note
              (Name  => To_Standard_String (Name),
               Value => To_String (Value_List));
         end;
      end loop;
   end Analyse_Notes;

   ----------------------
   -- Analyse_Operator --
   ----------------------

   procedure Analyse_Operator
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Operator_Node   : Node_Id)
   is
      use type Ack.Types.Type_Entity;
      Operator  : constant Name_Id := Get_Name (Operator_Node);
      Left      : constant Node_Id := Field_1 (Operator_Node);
      Right     : constant Node_Id := Field_2 (Operator_Node);
      Left_Type : Ack.Types.Type_Entity;
   begin
      Analyse_Expression
        (Class           => Class,
         Container       => Container,
         Expression_Type => Get_Top_Level_Type ("any"),
         Expression      => Left);
      Left_Type := Ack.Types.Type_Entity (Get_Type (Left));

      if Left_Type /= null then
         Left_Type.Check_Bound;
      end if;

      if Left_Type = null then
         null;
      elsif not Left_Type.Has_Aliased_Feature (Operator) then
         Error (Operator_Node, E_Undeclared_Name);
      else
         declare
            Feature : constant Ack.Features.Feature_Entity :=
                        Left_Type.Aliased_Feature (Operator);
         begin
            pragma Assert (Feature.Argument_Count in 0 .. 1);
            if Feature.Argument_Count = 0 then
               if Right /= No_Node then
                  Error (Right, E_Too_Many_Arguments);
               end if;
            else
               if Right = No_Node then
                  Error (Operator_Node, E_Insufficient_Arguments);
               else
                  Analyse_Expression
                    (Class, Container, Feature.Argument (1).Get_Type, Right);
               end if;
            end if;
            Set_Type (Operator_Node, Feature.Get_Type);
            if not Feature.Get_Type.Conforms_To (Expression_Type) then
               Error (Operator_Node, E_Type_Error,
                      Entity_Type (Expression_Type));
            end if;
         end;
      end if;

   end Analyse_Operator;

   -----------------------
   -- Analyse_Precursor --
   -----------------------

   procedure Analyse_Precursor
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Precursor       : Node_Id)
   is
      List   : constant List_Id :=
                 Node_Table (Precursor).List;

      Local_Table  : Entity_Type := Entity_Type (Container);
      Value_Entity : Entity_Type := null;
      Value_Type   : Entity_Type := null;

      Stop         : Boolean := False;

      procedure Process (Precursor_Element : Node_Id);

      -------------
      -- Process --
      -------------

      procedure Process (Precursor_Element : Node_Id) is
         Name : constant Name_Id := Get_Name (Precursor_Element);
      begin
         if Stop then
            return;
         end if;

         if Local_Table = null then
            Error (Precursor_Element, E_No_Component);
            Stop := True;
            return;
         end if;

         Local_Table.Check_Bound;

         if not Local_Table.Contains (Name) then
            Error (Precursor_Element, E_Undeclared_Name, Local_Table);
            Stop := True;
            return;
         end if;

         declare
            Entity           : constant Entity_Type :=
                                 Local_Table.Get (Name);
            Actual_List_Node : constant Node_Id :=
                                 Actual_List (Precursor_Element);
         begin

            if Actual_List_Node /= No_Node then
               declare
                  Actuals : constant Array_Of_Nodes :=
                              To_Array
                                (Node_Table.Element
                                   (Actual_List_Node).List);
               begin
                  if Entity.Argument_Count = 0 then
                     Error (Actual_List_Node, E_Does_Not_Accept_Arguments);
                  elsif Actuals'Length > Entity.Argument_Count then
                     Error (Actual_List_Node, E_Too_Many_Arguments);
                  elsif Actuals'Length < Entity.Argument_Count then
                     Error (Actual_List_Node, E_Insufficient_Arguments);
                  else
                     for I in Actuals'Range loop
                        Analyse_Expression
                          (Class           => Class,
                           Container       => Container,
                           Expression_Type => Entity.Argument (I).Get_Type,
                           Expression      => Actuals (I));
                     end loop;
                  end if;
               end;
            end if;

            Set_Entity (Precursor_Element, Entity);
            Set_Context (Precursor_Element, Local_Table.Class_Context);
            Value_Entity := Entity;
            Value_Type := Value_Entity.Get_Type;
            Local_Table := Value_Type;
         end;
      end Process;

   begin

      Scan (List, Process'Access);

      if not Stop then
         if Value_Entity /= null then
            Set_Entity (Precursor, Value_Entity);

            if Expression_Type = null then
               if Value_Type /= null then
                  Error (Precursor, E_Ignored_Return_Value);
               end if;
            elsif Value_Type /= null
              and then not Value_Type.Conforms_To (Expression_Type)
            then
               Error (Precursor, E_Type_Error,
                      Entity_Type (Expression_Type));
            end if;
         end if;

         if Value_Type /= null then
            Set_Type (Precursor, Value_Type);
         end if;

      end if;

   end Analyse_Precursor;

   -------------------
   -- Analyse_Retry --
   -------------------

   procedure Analyse_Retry
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Retry      : Node_Id)
   is
   begin
      null;
   end Analyse_Retry;

   ------------------------------
   -- Analyse_Tuple_Expression --
   ------------------------------

   procedure Analyse_Tuple_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Expression      : Node_Id)
   is
      use type Ack.Classes.Class_Entity;
      use type Ack.Types.Type_Entity;
      Expr_Nodes : constant Array_Of_Nodes :=
                     To_Array
                       (Tuple_Expression_List (Expression));
      Expr_Count : constant Natural :=
                     Expr_Nodes'Length;
      Expr_Type    : constant Ack.Types.Type_Entity :=
                       Ack.Types.Type_Entity (Expression_Type);
      Tuple_Arity  : constant Tuple_Arity_Range :=
                       Tuple_Arity_Range (Expr_Count);
      Expr_Type_Entity : constant Ack.Types.Type_Entity :=
                           Ack.Types.Type_Entity (Expression_Type);
      Actual_Types : Ack.Types.Array_Of_Types (Expr_Nodes'Range);
      Tuple_Class  : Ack.Classes.Class_Entity renames
                       Tuple_Classes (Tuple_Arity);
      Type_Entity   : Ack.Types.Type_Entity := null;
   begin

      if Tuple_Class = null then
         Tuple_Class := Load_Tuple_Class (Tuple_Arity);
      end if;

      if Expr_Type.Class /= null
        and then not Expr_Type.Class.Conforms_To (Tuple_Class)
      then
         Error (Expression, E_Type_Error, Tuple_Class);
         return;
      end if;

      if Expr_Type_Entity.Generic_Binding_Count = 0 then
         Error (Expression, E_Type_Error, Expr_Type_Entity);
         return;
      end if;

      if Expr_Type_Entity.Generic_Binding_Count /= Expr_Nodes'Length then
         Error (Expression, E_Type_Error, Expr_Type_Entity);
         return;
      end if;

      for I in Expr_Nodes'Range loop
         Analyse_Expression
           (Class, Container,
            Expr_Type_Entity.Generic_Binding (I), Expr_Nodes (I));
         Actual_Types (I) :=
           Ack.Types.Type_Entity (Get_Type (Expr_Nodes (I)));
      end loop;

      Type_Entity :=
        Ack.Types.Instantiate_Generic_Class
          (Node            => Expression,
           Generic_Class   => Tuple_Class,
           Generic_Actuals => Actual_Types,
           Detachable      => False);

      if Type_Entity /= null then
         Set_Type (Expression, Type_Entity);
      end if;

   end Analyse_Tuple_Expression;

   ------------------------
   -- Analyse_Tuple_Type --
   ------------------------

   procedure Analyse_Tuple_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
   is
      use type Ack.Classes.Class_Entity;
      use type Ack.Types.Type_Entity;
      Actual_Nodes : constant Array_Of_Nodes :=
                       To_Array
                         (Tuple_Argument_List (Type_Node));
      Actual_Count : constant Natural :=
                       Actual_Nodes'Length;
      Tuple_Arity  : constant Tuple_Arity_Range :=
                       Tuple_Arity_Range (Actual_Count);
      Actual_Types : Ack.Types.Array_Of_Types (Actual_Nodes'Range);
      Tuple_Class  : Ack.Classes.Class_Entity renames
                       Tuple_Classes (Tuple_Arity);
      Type_Entity   : Ack.Types.Type_Entity := null;
   begin
      if Tuple_Class = null then
         Tuple_Class := Load_Tuple_Class (Tuple_Arity);
      end if;

      for I in Actual_Nodes'Range loop
         Analyse_Type (Class, Actual_Nodes (I));
         Actual_Types (I) := Ack.Types.Get_Type_Entity (Actual_Nodes (I));
      end loop;

      Type_Entity :=
        Ack.Types.Instantiate_Generic_Class
          (Node            => Type_Node,
           Generic_Class   => Tuple_Class,
           Generic_Actuals => Actual_Types,
           Detachable      => False);

      if Type_Entity /= null then
         Set_Entity (Type_Node, Type_Entity);
      else
         Set_Entity (Type_Node, Get_Top_Level_Type ("any"));
      end if;

   end Analyse_Tuple_Type;

   ------------------
   -- Analyse_Type --
   ------------------

   procedure Analyse_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
   is
   begin
      case N_Type (Kind (Type_Node)) is
         when N_Class_Type =>
            Analyse_Class_Type (Class, Type_Node);
         when N_Anchored_Type =>
            Analyse_Anchored_Type (Class, Type_Node);
         when N_Tuple_Type =>
            Analyse_Tuple_Type (Class, Type_Node);
      end case;
   end Analyse_Type;

   ----------------------
   -- Forward_Iterable --
   ----------------------

   function Forward_Iterable return Ack.Classes.Class_Entity is
      Container_Name    : constant Name_Id := Get_Name_Id ("containers");
      Forward_Name      : constant Name_Id := Get_Name_Id ("forward_iterable");
      Aqua_Entity       : constant Ack.Classes.Class_Entity :=
                            Ack.Classes.Get_Top_Level_Class ("aqua");
      Containers_Entity : constant Ack.Classes.Class_Entity :=
                            (if Aqua_Entity.Contains (Container_Name)
                             then Ack.Classes.Class_Entity
                               (Aqua_Entity.Get (Container_Name))
                             else Load_Class
                               (Get_Program (Aqua_Entity.Declaration_Node),
                                Aqua_Entity, Container_Name));
      Forward_Entity    : constant Ack.Classes.Class_Entity :=
                            (if Containers_Entity.Contains (Forward_Name)
                             then Ack.Classes.Class_Entity
                               (Containers_Entity.Get (Forward_Name))
                             else Load_Class
                               (Get_Program
                                  (Containers_Entity.Declaration_Node),
                                Containers_Entity, Forward_Name));
   begin
      return Forward_Entity;
   end Forward_Iterable;

   ------------------------
   -- Get_Top_Level_Type --
   ------------------------

   function Get_Top_Level_Type
     (Name : String)
      return Ack.Types.Type_Entity
   is
      use type Ack.Classes.Class_Entity;
      Class : Ack.Classes.Class_Entity :=
                Ack.Classes.Get_Top_Level_Class (Name);
   begin
      if Class = null then
         Class := Load_Class (null, Ack.Environment.Top_Level,
                              Get_Name_Id (Name));
      end if;

      if Class = null then
         raise Constraint_Error with
           "unknown top-level class: " & Name;
      end if;

      return Ack.Types.Get_Top_Level_Type (Name);
   end Get_Top_Level_Type;

   ----------------
   -- Load_Class --
   ----------------

   function Load_Class
     (Referrer        : Aquarius.Programs.Program_Tree;
      Parent          : not null access Root_Entity_Type'Class;
      Name            : Name_Id)
      return Ack.Classes.Class_Entity
   is
      Entity : Entity_Type :=
                 (if Parent.Contains (Name)
                  then Parent.Get (Name)
                  else null);
   begin
      if Entity = null then
         declare
            Path : constant String :=
                     Ack.Files.Find_Class_File
                       (Referrer, Parent, Name);
         begin
            if Path /= "" then
               declare
                  Program : constant Aquarius.Programs.Program_Tree :=
                              Aquarius.Loader.Load_From_File
                                (Path);
                  Node    : constant Node_Id :=
                              Ack.Parser.Import (Program);
               begin
                  Ack.Semantic.Analyse_Class_Declaration (Node);
                  Ack.Errors.Record_Errors (Node);
                  Ack.Errors.Report_Errors (Node);
                  Entity := Get_Entity (Node);
                  declare
                     Base_Name : constant String := Entity.Base_File_Name;
                  begin
                     Loaded_Classes.Insert
                       (Base_Name, Node);
                  end;

                  declare
                     use Aquarius.Programs.Komnenos_Entities;
                     use Komnenos.Entities;
                  begin
                     Create_Aquarius_Source_Entity
                       (Table            =>
                          Komnenos.Entities.Tables.Table ("aqua"),
                        Name             => Entity.Qualified_Name,
                        Qualified_Name   => Entity.Qualified_Name,
                        Class_Name       => "class",
                        Top_Level        => True,
                        Compilation_Unit => Program,
                        Defining_Name    => Program,
                        Entity_Spec      => Program,
                        Entity_Body      => Program);
                  end;

                  Partial_Class_List.Append (Node);
               end;
            end if;
         end;
      end if;

      if Entity = null then
         return null;
      else
         return Ack.Classes.Class_Entity (Entity);
      end if;

   end Load_Class;

   ----------------------
   -- Load_Tuple_Class --
   ----------------------

   function Load_Tuple_Class
     (Arity : Tuple_Arity_Range)
      return Ack.Classes.Class_Entity
   is
      Aqua_Class   : constant Ack.Classes.Class_Entity :=
                       Load_Class (null,
                                   Ack.Environment.Top_Level,
                                   Get_Name_Id ("Aqua"));
      Arity_Image  : constant String := Tuple_Arity_Range'Image (Arity);
      Tuple_Name   : constant String :=
                       "Tuple" & Arity_Image (2 .. Arity_Image'Last);
   begin
      return Load_Class (null, Aqua_Class,
                         Get_Name_Id (Tuple_Name));
   end Load_Tuple_Class;

   ---------------------------
   -- Property_Feature_Node --
   ---------------------------

   function Property_Feature_Node
     (Node : Node_Id)
      return Boolean
   is
      Dec_Body        : constant Node_Id := Declaration_Body (Node);
      Value_Node      : constant Node_Id := Value (Dec_Body);
      Value_Feature   : constant Boolean :=
                          Value_Node /= No_Node
                              and then Kind (Value_Node) = N_Explicit_Value;
      Routine_Feature : constant Boolean :=
                          Value_Node /= No_Node
                              and then Kind (Value_Node) = N_Routine;
      Deferred        : constant Boolean :=
                          Routine_Feature
                              and then Node_Table.Element
                                (Value_Node).Deferred;
      Arg_Node        : constant Node_Id := Formal_Arguments (Dec_Body);
      Type_Node       : constant Node_Id := Value_Type (Dec_Body);
   begin
      return not Deferred
        and then not Routine_Feature
        and then not Value_Feature
        and then Arg_Node = No_Node and then Type_Node /= No_Node;
   end Property_Feature_Node;

end Ack.Semantic;
