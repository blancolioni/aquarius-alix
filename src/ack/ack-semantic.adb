with Ada.Text_IO;

with Aquarius.Loader;

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

   procedure Analyse_Feature_Name
     (Class   : Ack.Classes.Class_Entity;
      Exports : Node_Id;
      Feature : Node_Id)
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

   ---------------------------
   -- Analyse_Anchored_Type --
   ---------------------------

   procedure Analyse_Anchored_Type
     (Class     : Ack.Classes.Class_Entity;
      Type_Node : Node_Id)
   is
   begin
      null;
   end Analyse_Anchored_Type;

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

   -------------------------------
   -- Analyse_Class_Declaration --
   -------------------------------

   procedure Analyse_Class_Declaration
     (Node : Node_Id)
   is
      Inheritance_Node : constant Node_Id := Inheritance (Node);
      Features_Node    : constant Node_Id := Class_Features (Node);
      Class : constant Ack.Classes.Class_Entity :=
                           Analyse_Class_Header (Node, Class_Header (Node));
   begin

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Analysing: " & Class.Qualified_Name);
      end if;

      if Features_Node in Real_Node_Id then
         if Trace_Class_Analysis then
            Ada.Text_IO.Put_Line
              ("Analysing feature names: " & Class.Qualified_Name);
         end if;

         Analyse_Features (Class, Features_Node,
                           Analyse_Feature_Name'Access);
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
      else
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
                       Generic_Actuals => Actual_Types);
               end if;
            end;
         end if;
      end if;

      if Type_Entity /= null then
         Set_Entity (Type_Node, Type_Entity);
      else
         Set_Entity (Type_Node, Ack.Types.Get_Top_Level_Type ("any"));
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
               null;
            when N_Conditional =>
               Analyse_Conditional (Class, Container, Node);
            when N_Loop =>
               null;
            when N_Precursor =>
               Analyse_Precursor
                 (Class           => Class,
                  Container       => Container,
                  Expression_Type => null,
                  Precursor       => Node);
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
            Analyse_Expression
              (Class, Container,
               Ack.Types.Get_Top_Level_Type ("boolean"),
               Condition);
            if Has_Entity (Condition) then
               Container.Add_Implicit (Get_Entity (Condition));
            end if;
         end if;
         Analyse_Compound (Class, Container, Compound);
         if Condition /= No_Node and then Has_Entity (Condition) then
            Container.Remove_Implicit;
         end if;
      end Analyse_Element;

   begin
      Scan (Node_Table.Element (Conditional).List,
            Analyse_Element'Access);
   end Analyse_Conditional;

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
         end if;

         if Has_Entity (Type_Node) then
            Type_Entity := Ack.Types.Get_Type_Entity (Type_Node);
            Scan (Ids, Insert_Id'Access);
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
   begin
      case N_Expression_Node (Kind (Expression)) is
         when N_Operator =>
            Analyse_Operator
              (Class, Container, Expression_Type, Expression);
         when N_Precursor =>
            Analyse_Precursor
              (Class, Container, Expression_Type, Expression);
         when N_Attachment_Test =>
            Analyse_Expression (Class, Container,
                                Ack.Types.Get_Top_Level_Type ("any"),
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
                                     "integer");
               Value_Type : constant Ack.Types.Type_Entity :=
                              Ack.Types.Get_Top_Level_Type (Type_Name);
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
      Locals_Node     : constant Node_Id :=
                          (if Value_Node = No_Node then No_Node
                           else Local_Declarations (Value_Node));
      Effective_Node  : constant Node_Id :=
                          (if Effective then Effective_Routine (Value_Node)
                           else No_Node);
      Internal        : constant Boolean :=
                          Effective
                              and then Kind (Effective_Node) = N_Internal;
      External        : constant Boolean :=
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
         Analyse_Class_Type (Class, Type_Node);
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
     (Class   : Ack.Classes.Class_Entity;
      Exports : Node_Id;
      Feature : Node_Id)
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
               Analyse (Class   => Class,
                        Exports => No_Node,
                        Node    => Feature_Node);
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

      procedure Set_Redefine (Node : Node_Id);

      ------------------
      -- Set_Redefine --
      ------------------

      procedure Set_Redefine (Node : Node_Id) is
      begin
         if Inherited_Class.Has_Feature (Get_Name (Node)) then
            if Class.Has_Feature (Get_Name (Node)) then
               Class.Feature (Get_Name (Node)).Set_Redefined
                 (Inherited_Class);
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

      Inherited_Type := Ack.Types.Get_Type_Entity (Class_Type);
      Inherited_Class := Inherited_Type.Class;

      Set_Entity (Inherit, Inherited_Type);

      Class.Inherit (Inherited_Type);
      Scan (Redefine_List, Set_Redefine'Access);

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
--        Ada.Text_IO.Put_Line
--          (Get_Program (Left).Show_Location
--               & ": analysing: " & To_String (Operator));
      Analyse_Expression
        (Class           => Class,
         Container       => Container,
         Expression_Type => Ack.Types.Get_Top_Level_Type ("any"),
         Expression      => Left);
      Left_Type := Ack.Types.Type_Entity (Get_Type (Left));

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

         if not Local_Table.Contains (Name) then
            Error (Precursor_Element, E_Undeclared_Name);
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
      end case;
   end Analyse_Type;

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

end Ack.Semantic;
