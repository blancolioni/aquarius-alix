with Aquarius.Loader;

with Ack.Classes;
with Ack.Files;
with Ack.Parser;

with Ack.Primitives;

with Ack.Errors;

package body Ack.Semantic is

   function Load_Entity
     (Referrer        : Aquarius.Programs.Program_Tree;
      Parent          : Entity_Id;
      Name            : Name_Id)
      return Entity_Id;

   procedure Analyse_Class_Header
     (Class  : Node_Id;
      Header : Node_Id);

   procedure Analyse_Formal_Generics
     (Class           : Node_Id;
      Formal_Generics : Node_Id);

   procedure Analyse_Class_Name
     (Class         : Node_Id;
      Class_Name    : Node_Id;
      Defining_Name : Boolean);

   procedure Analyse_Features
     (Class    : Node_Id;
      Features : Node_Id);

   procedure Analyse_Inheritance
     (Class       : Node_Id;
      Inheritance : Node_Id);

   procedure Analyse_Inherit
     (Class   : Node_Id;
      Inherit : Node_Id);

   procedure Analyse_Feature_Clause
     (Class  : Node_Id;
      Clause : Node_Id);

   procedure Analyse_Feature_Declaration
     (Class   : Node_Id;
      Feature : Node_Id);

   procedure Analyse_Entity_Declaration_Groups
     (Class      : Node_Id;
      Table      : Entity_Id;
      Group_List : Node_Id;
      Kind       : Local_Entity_Kind);

   procedure Analyse_Type
     (Class     : Node_Id;
      Table     : Entity_Id;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) in N_Type;

   procedure Analyse_Class_Type
     (Class     : Node_Id;
      Table     : Entity_Id;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) = N_Class_Type;

   procedure Analyse_Anchored_Type
     (Class     : Node_Id;
      Table     : Entity_Id;
      Type_Node : Node_Id)
     with Pre => Kind (Type_Node) = N_Anchored_Type;

   procedure Analyse_Routine
     (Class   : Node_Id;
      Table   : Entity_Id;
      Routine : Node_Id);

   procedure Analyse_Effective_Routine
     (Class   : Node_Id;
      Table   : Entity_Id;
      Routine : Node_Id);

   procedure Analyse_Compound
     (Class    : Node_Id;
      Table    : Entity_Id;
      Compound : Node_Id);

   procedure Analyse_Assignment
     (Class      : Node_Id;
      Table      : Entity_Id;
      Assignment : Node_Id);

   procedure Analyse_Expression
     (Class           : Node_Id;
      Table           : Entity_Id;
      Expression_Type : Entity_Id;
      Expression      : Node_Id);

   procedure Analyse_Precursor
     (Class           : Node_Id;
      Table           : Entity_Id;
      Expression_Type : Entity_Id;
      Precursor       : Node_Id);

   ---------------------------
   -- Analyse_Anchored_Type --
   ---------------------------

   procedure Analyse_Anchored_Type
     (Class     : Node_Id;
      Table     : Entity_Id;
      Type_Node : Node_Id)
   is
   begin
      null;
   end Analyse_Anchored_Type;

   ------------------------
   -- Analyse_Assignment --
   ------------------------

   procedure Analyse_Assignment
     (Class      : Node_Id;
      Table      : Entity_Id;
      Assignment : Node_Id)
   is
      Target : constant Name_Id := Get_Name (Variable (Assignment));
      Entity : constant Entity_Id := Find_Entity (Table, Target);
   begin
      if Entity = Undeclared_Entity then
         Error (Variable (Assignment), E_Undeclared_Name);
      else
         Analyse_Expression (Class, Table, Get_Type (Entity),
                             Expression (Assignment));
      end if;

      Set_Entity (Variable (Assignment), Entity);
   end Analyse_Assignment;

   -------------------------------
   -- Analyse_Class_Declaration --
   -------------------------------

   procedure Analyse_Class_Declaration
     (Node : Node_Id)
   is
      Inheritance_Node : constant Node_Id := Inheritance (Node);
      Features_Node : constant Node_Id := Features (Node);
   begin
      Analyse_Class_Header (Node, Class_Header (Node));
      if Inheritance_Node /= No_Node then
         Analyse_Inheritance (Node, Inheritance_Node);
      end if;
      if Features_Node in Real_Node_Id then
         Analyse_Features (Node, Features_Node);
      end if;
   end Analyse_Class_Declaration;

   --------------------------
   -- Analyse_Class_Header --
   --------------------------

   procedure Analyse_Class_Header
     (Class  : Node_Id;
      Header : Node_Id)
   is
      Formal_Generics_Node : constant Node_Id := Formal_Generics (Header);
   begin
      Analyse_Class_Name (Class, Class_Name (Header),
                          Defining_Name => True);
      if Formal_Generics_Node /= No_Node then
         Analyse_Formal_Generics (Class, Formal_Generics_Node);
      end if;
   end Analyse_Class_Header;

   ------------------------
   -- Analyse_Class_Name --
   ------------------------

   procedure Analyse_Class_Name
     (Class         : Node_Id;
      Class_Name    : Node_Id;
      Defining_Name : Boolean)
   is
      use type List_Of_Nodes.Cursor;
      List : constant List_Of_Nodes.List :=
               List_Table.Element (Identifiers (Class_Name)).List;
      Position : List_Of_Nodes.Cursor := List.First;
      Last : constant List_Of_Nodes.Cursor :=
               (if Defining_Name
                then List.Last
                else List_Of_Nodes.No_Element);

      Class_Context : Entity_Id :=
                        (if Defining_Name then No_Entity
                         else Get_Entity (Class));
      Parent        : Entity_Id;
      Class_Program : constant Aquarius.Programs.Program_Tree :=
                        Node_Table.Element (Class).From;

   begin
      if Defining_Name then
         Parent := No_Entity;
      else
         Parent := Undeclared_Entity;
         declare
            Element_Node : constant Node_Id :=
                             List_Of_Nodes.Element (Position);
            Element_Name : constant Name_Id :=
                             Node_Table.Element (Element_Node).Name;
         begin
            while Class_Context /= No_Entity loop
               Parent :=
                 Load_Entity (Class_Program, Class_Context, Element_Name);
               exit when Parent /= Undeclared_Entity;
               Class_Context := Get_Context (Class_Context);
            end loop;
            if Parent = Undeclared_Entity then
               Parent := Load_Entity (Class_Program, No_Entity, Element_Name);
            end if;
         end;

         if Parent = Undeclared_Entity then
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
            New_Parent   : constant Entity_Id :=
                             Load_Entity
                               (Referrer        => Class_Program,
                                Parent          => Parent,
                                Name            => Element_Name);
         begin
            if New_Parent = Undeclared_Entity then
               Error (Element_Node,
                      (if Parent = No_Entity
                       then E_Undeclared_Name
                       else E_No_Child));
               Parent :=
                 New_Entity
                   (Name        => Element_Name,
                    Kind        => Class_Entity,
                    Context     => Parent,
                    Declaration => Element_Node,
                    Entity_Type => No_Entity);
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
         begin
            Ack.Classes.Add_Class
              (Parent, Last_Name, Class);
         end;
      else
         Set_Entity (Class_Name, Parent);
      end if;

   end Analyse_Class_Name;

   ------------------------
   -- Analyse_Class_Type --
   ------------------------

   procedure Analyse_Class_Type
     (Class     : Node_Id;
      Table     : Entity_Id;
      Type_Node : Node_Id)
   is
      Name_Node     : constant Node_Id := Class_Name (Type_Node);
      Generics_Node : constant Node_Id := Actual_Generics (Type_Node);
      Type_Entity   : Entity_Id := No_Entity;

      procedure Analyse_Generic_Type (Generic_Actual_Node : Node_Id);

      --------------------------
      -- Analyse_Generic_Type --
      --------------------------

      procedure Analyse_Generic_Type (Generic_Actual_Node : Node_Id) is
      begin
         Analyse_Type (Class, Table, Generic_Actual_Node);
      end Analyse_Generic_Type;

   begin
      Analyse_Class_Name (Class, Name_Node, False);
      Type_Entity := Get_Entity (Name_Node);

      if Generics_Node /= No_Node then
         Scan (Actual_Generics_List (Generics_Node),
               Analyse_Generic_Type'Access);
         Type_Entity :=
           Ack.Classes.Instantiate_Class
             (Type_Entity, Get_Entity (Class), Generics_Node);
      end if;

      Set_Entity (Type_Node, Type_Entity);

   end Analyse_Class_Type;

   ----------------------
   -- Analyse_Compound --
   ----------------------

   procedure Analyse_Compound
     (Class    : Node_Id;
      Table    : Entity_Id;
      Compound : Node_Id)
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
               Analyse_Assignment (Class, Table, Node);
            when N_Creation_Instruction =>
               null;
            when N_Conditional =>
               null;
            when N_Loop =>
               null;
            when N_Precursor =>
               Analyse_Precursor
                 (Class           => Class,
                  Table           => Table,
                  Expression_Type => No_Entity,
                  Precursor       => Node);
         end case;
      end Analyse;

   begin
      Scan (List, Analyse'Access);
   end Analyse_Compound;

   -------------------------------
   -- Analyse_Effective_Routine --
   -------------------------------

   procedure Analyse_Effective_Routine
     (Class   : Node_Id;
      Table   : Entity_Id;
      Routine : Node_Id)
   is
   begin
      case N_Effective_Routine (Kind (Routine)) is
         when N_Internal =>
            Analyse_Compound (Class, Table, Compound (Routine));
         when N_External =>
            null;
      end case;
   end Analyse_Effective_Routine;

   ---------------------------------------
   -- Analyse_Entity_Declaration_Groups --
   ---------------------------------------

   procedure Analyse_Entity_Declaration_Groups
     (Class      : Node_Id;
      Table      : Entity_Id;
      Group_List : Node_Id;
      Kind       : Local_Entity_Kind)
   is

      Count : Natural := 0;

      procedure Insert_Group (Group_Node : Node_Id);

      ------------------
      -- Insert_Group --
      ------------------

      procedure Insert_Group (Group_Node : Node_Id) is
         Ids         : constant List_Id := Identifiers (Group_Node);
         Type_Node   : constant Node_Id := Entity_Type (Group_Node);
         Type_Entity : Entity_Id := No_Entity;

         procedure Insert_Id (Id_Node : Node_Id);

         ---------------
         -- Insert_Id --
         ---------------

         procedure Insert_Id (Id_Node : Node_Id) is
            Entity      : constant Entity_Id :=
                            New_Entity
                              (Name        => Get_Name (Id_Node),
                               Kind        => Kind,
                               Context     => Table,
                               Declaration => Group_Node,
                               Entity_Type => Type_Entity);
         begin
            Set_Entity (Id_Node, Entity);
            Count := Count + 1;
         end Insert_Id;

      begin
         if Type_Node /= No_Node then
            Analyse_Type (Class, Table, Type_Node);
            Type_Entity := Get_Entity (Type_Node);
         end if;

         Scan (Ids, Insert_Id'Access);
      end Insert_Group;

   begin
      Scan (Node_Table.Element (Group_List).List, Insert_Group'Access);
      Node_Table (Group_List).Integer_Value := Count;
   end Analyse_Entity_Declaration_Groups;

   ------------------------
   -- Analyse_Expression --
   ------------------------

   procedure Analyse_Expression
     (Class           : Node_Id;
      Table           : Entity_Id;
      Expression_Type : Entity_Id;
      Expression      : Node_Id)
   is
   begin
      case N_Expression_Node (Kind (Expression)) is
         when N_Operator =>
            null;
         when N_Precursor =>
            Analyse_Precursor
              (Class, Table, Expression_Type, Expression);
         when N_Constant =>
            declare
               use Ack.Primitives;
               Value      : constant Node_Id := Constant_Value (Expression);
               Value_Type : constant Entity_Id :=
                              (case N_Constant_Value (Kind (Value)) is
                                  when N_String_Constant  =>
                                     String_Class,
                                  when N_Integer_Constant =>
                                     Integer_Class);
            begin
               Set_Entity (Expression, Value_Type);
               if Expression_Type = No_Entity then
                  Error (Value, E_Ignored_Return_Value);
               elsif not Ack.Classes.Is_Derived_From
                 (Context    => Get_Entity (Class),
                  Ancestor   => Value_Type,
                  Descendent => Expression_Type)
               then
                  Error (Expression, E_Type_Error, Expression_Type);
               end if;
            end;
      end case;
   end Analyse_Expression;

   ----------------------------
   -- Analyse_Feature_Clause --
   ----------------------------

   procedure Analyse_Feature_Clause
     (Class  : Node_Id;
      Clause : Node_Id)
   is
      Feature_List : constant List_Id :=
                       Feature_Declarations (Clause);
   begin
      if Feature_List in Real_List_Id then
         for Feature_Node of List_Table.Element (Feature_List).List loop
            Analyse_Feature_Declaration (Class, Feature_Node);
         end loop;
      end if;
   end Analyse_Feature_Clause;

   ---------------------------------
   -- Analyse_Feature_Declaration --
   ---------------------------------

   procedure Analyse_Feature_Declaration
     (Class   : Node_Id;
      Feature : Node_Id)
   is
      Names        : constant List_Id := New_Feature_List (Feature);
      Dec_Body     : constant Node_Id := Declaration_Body (Feature);
      Arg_Node     : constant Node_Id := Formal_Arguments (Dec_Body);
      Type_Node    : constant Node_Id := Value_Type (Dec_Body);
      Value_Node   : constant Node_Id := Value (Dec_Body);
      Type_Entity  : Entity_Id := No_Entity;
      Single       : constant Boolean :=
                       Natural (List_Table.Element (Names).List.Length) = 1;
      Feature_Kind : Entity_Kind := Property_Feature_Entity;
   begin

      if Single and then
        (Arg_Node /= No_Node or else Value_Node /= No_Node)
      then
         Feature_Kind := Routine_Feature_Entity;
      end if;

      if not Single then
         if Arg_Node /= No_Node then
            Error (Feature, E_Id_List_With_Arguments);
         elsif Type_Node = No_Node then
            Error (Feature, E_Id_List_With_No_Type);
         elsif Value_Node /= No_Node
           and then Kind (Value_Node) = N_Routine
         then
            Error (Feature, E_Id_List_With_Routine);
         end if;
      end if;

      if Type_Node /= No_Node then
         Analyse_Class_Type
           (Class, Get_Entity (Class), Type_Node);
         Type_Entity := Get_Entity (Type_Node);
      end if;

      for Node of List_Table.Element (Names).List loop
         declare
            Entity : constant Entity_Id :=
                       New_Entity
                         (Name        => Get_Name (Feature_Name (Node)),
                          Kind        => Feature_Kind,
                          Context     => Get_Entity (Class),
                          Declaration => Feature,
                          Entity_Type => Type_Entity);
         begin
            Set_Entity (Node, Entity);

            if Single and then Arg_Node /= No_Node then
               Analyse_Entity_Declaration_Groups
                 (Class, Entity,
                  Entity_Declaration_Group_List (Arg_Node),
                  Kind => Argument_Entity);
               Set_Entity (Feature, Entity);
            end if;

            if Value_Node /= No_Node then
               if Kind (Value_Node) = N_Routine then
                  Create_Current_Entity
                    (Get_Entity (Class), Feature, Entity);
                  Set_Entity
                    (Value_Node,
                     New_Entity
                       (Name        => Get_Name_Id ("Result"),
                        Kind        => Result_Entity,
                        Context     => Entity,
                        Declaration => Feature,
                        Entity_Type => Type_Entity));
                  Analyse_Routine (Class, Entity, Value_Node);
               end if;
            end if;

         end;
      end loop;

   end Analyse_Feature_Declaration;

   ----------------------
   -- Analyse_Features --
   ----------------------

   procedure Analyse_Features
     (Class    : Node_Id;
      Features : Node_Id)
   is
      Clause_List : constant List_Id :=
                      Feature_Clauses (Features);
   begin
      for Clause_Node of List_Table.Element (Clause_List).List loop
         Analyse_Feature_Clause (Class, Clause_Node);
      end loop;
   end Analyse_Features;

   -----------------------------
   -- Analyse_Formal_Generics --
   -----------------------------

   procedure Analyse_Formal_Generics
     (Class           : Node_Id;
      Formal_Generics : Node_Id)
   is
      procedure Analyse_Formal_Generic (Node : Node_Id);

      ----------------------------
      -- Analyse_Formal_Generic --
      ----------------------------

      procedure Analyse_Formal_Generic (Node : Node_Id) is
         Name : constant Name_Id :=
                  Get_Name (Formal_Generic_Name (Node));
         Generic_Entity : constant Entity_Id :=
                            New_Entity
                              (Name        => Name,
                               Kind        => Generic_Argument_Entity,
                               Context     => Get_Entity (Class),
                               Declaration => Node,
                               Entity_Type => Primitives.Any_Class);
      begin
         Set_Entity (Node, Generic_Entity);
      end Analyse_Formal_Generic;

   begin
      Scan (Formal_Generics_List (Formal_Generics),
            Analyse_Formal_Generic'Access);
   end Analyse_Formal_Generics;

   ---------------------
   -- Analyse_Inherit --
   ---------------------

   procedure Analyse_Inherit
     (Class   : Node_Id;
      Inherit : Node_Id)
   is
      Class_Type    : constant Node_Id := Inherit_Class_Type (Inherit);
      Class_Node    : constant Node_Id := Class_Name (Class_Type);
      Redefine_List : constant List_Id := Redefine (Inherit);

      procedure Inherit_Entity (Entity : Entity_Id);

      --------------------
      -- Inherit_Entity --
      --------------------

      procedure Inherit_Entity (Entity : Entity_Id) is
      begin
         if Get_Kind (Entity) in Feature_Entity_Kind then
            Inherit_Entity
              (Entity        => Entity,
               Derived_Class => Get_Entity (Class),
               Declaration   => Inherit,
               Redefine      =>
                 Redefine_List /= No_List
               and then Contains_Name (Redefine_List, Get_Name (Entity)),
               Rename        => No_Name);
         end if;
      end Inherit_Entity;

   begin

      Analyse_Class_Name (Class, Class_Node,
                          Defining_Name => False);
      Set_Entity (Inherit, Get_Entity (Class_Node));

      Scan_Children (Get_Entity (Inherit), Inherit_Entity'Access);

   end Analyse_Inherit;

   -------------------------
   -- Analyse_Inheritance --
   -------------------------

   procedure Analyse_Inheritance
     (Class       : Node_Id;
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

   -----------------------
   -- Analyse_Precursor --
   -----------------------

   procedure Analyse_Precursor
     (Class           : Node_Id;
      Table           : Entity_Id;
      Expression_Type : Entity_Id;
      Precursor       : Node_Id)
   is
      List   : constant List_Id :=
                 Node_Table (Precursor).List;

      Local_Table  : Entity_Id := Table;
      Value_Entity : Entity_Id := No_Entity;
      Value_Type   : Entity_Id := No_Entity;

      procedure Process (Precursor_Element : Node_Id);

      -------------
      -- Process --
      -------------

      procedure Process (Precursor_Element : Node_Id) is
         Entity           : constant Entity_Id :=
                              Find_Entity
                                (Local_Table, Get_Name (Precursor_Element));
         Actual_List_Node : constant Node_Id :=
                              Actual_List (Precursor_Element);
      begin
         if Entity = Undeclared_Entity then
            Error (Precursor_Element, E_Undeclared_Name);
         else

            if Get_Kind (Entity) in Feature_Entity_Kind then
               declare
                  Formals : constant Node_Id :=
                                  Get_Formal_Arguments_Node (Entity);
               begin
                  if Actual_List_Node = No_Node then
                     if Formals /= No_Node then
                        Error (Precursor_Element, E_Insufficient_Arguments);
                     end if;
                  else

                     declare
                        use List_Of_Nodes;
                        Actuals_List : constant List_Id :=
                                         Node_Table.Element
                                           (Actual_List_Node).List;
                        Actuals_Node_List : constant List_Of_Nodes.List :=
                                              List_Table.Element
                                                (Actuals_List).List;
                        Current_Actual    : Cursor :=
                                              (if Actuals_Node_List.Is_Empty
                                               then No_Element
                                               else Actuals_Node_List.First);
                        Stop              : Boolean := False;
                        Entity_Group_List : constant Node_Id :=
                                              Entity_Declaration_Group_List
                                                (Formals);
                        procedure Check_Argument
                          (Declaration_Node : Node_Id);

                        --------------------
                        -- Check_Argument --
                        --------------------

                        procedure Check_Argument
                          (Declaration_Node : Node_Id)
                        is
                           Name        : constant Name_Id :=
                                           Get_Name (Declaration_Node)
                                           with Unreferenced;
                           Entity      : constant Entity_Id :=
                                           Get_Entity (Declaration_Node);
                           Entity_Type : constant Entity_Id :=
                                           Get_Type (Entity);
                        begin

                           if Stop then
                              return;
                           end if;

                           if not Has_Element (Current_Actual) then
                              Error (Precursor_Element,
                                     E_Insufficient_Arguments);
                              Stop := True;
                           else
                              Analyse_Expression
                                (Class, Table, Entity_Type,
                                 List_Of_Nodes.Element (Current_Actual));
                              Next (Current_Actual);
                           end if;
                        end Check_Argument;

                     begin
                        Scan_Entity_Declarations
                          (Entity_Group_List, Check_Argument'Access);
                        if Has_Element (Current_Actual) then
                           Error (Precursor_Element, E_Too_Many_Arguments);
                        end if;
                     end;

                  end if;
               end;
            end if;

            Set_Entity (Precursor_Element, Entity);
            Value_Entity := Entity;
            Value_Type := Get_Type (Value_Entity);
            Local_Table := Value_Type;
         end if;
      end Process;

   begin

      Scan (List, Process'Access);

      if Value_Entity /= No_Entity then
         Set_Entity (Precursor, Value_Entity);

         if Expression_Type = No_Entity then
            if Value_Type /= No_Entity then
               Error (Precursor, E_Ignored_Return_Value);
            end if;
         elsif Value_Type /= No_Entity
           and then not Ack.Classes.Is_Derived_From
             (Get_Entity (Class), Expression_Type, Value_Type)
         then
            Error (Precursor, E_Type_Error, Expression_Type);
         end if;
      end if;

   end Analyse_Precursor;

   ---------------------
   -- Analyse_Routine --
   ---------------------

   procedure Analyse_Routine
     (Class   : Node_Id;
      Table   : Entity_Id;
      Routine : Node_Id)
   is
   begin
      Analyse_Effective_Routine (Class, Table, Effective_Routine (Routine));
   end Analyse_Routine;

   ------------------
   -- Analyse_Type --
   ------------------

   procedure Analyse_Type
     (Class     : Node_Id;
      Table     : Entity_Id;
      Type_Node : Node_Id)
   is
   begin
      case N_Type (Kind (Type_Node)) is
         when N_Class_Type =>
            Analyse_Class_Type (Class, Table, Type_Node);
         when N_Anchored_Type =>
            Analyse_Anchored_Type (Class, Table, Type_Node);
      end case;
   end Analyse_Type;

   -----------------
   -- Load_Entity --
   -----------------

   function Load_Entity
     (Referrer        : Aquarius.Programs.Program_Tree;
      Parent          : Entity_Id;
      Name            : Name_Id)
            return Entity_Id
   is
      Entity : Entity_Id := Find_Entity (Parent, Name);
   begin
      if Entity = Undeclared_Entity then
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
                  Loaded_Classes.Insert
                    (Get_File_Name (Entity), Node);
                  Partial_Class_List.Append (Node);
               end;
            end if;
         end;
      end if;
      return Entity;
   end Load_Entity;

end Ack.Semantic;