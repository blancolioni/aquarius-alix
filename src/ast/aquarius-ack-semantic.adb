with Aquarius.Loader;

with Aquarius.Ack.Classes;
with Aquarius.Ack.Files;
with Aquarius.Ack.Parser;

with Aquarius.Ack.IO;

package body Aquarius.Ack.Semantic is

   function Load_Entity
     (Referrer : Aquarius.Programs.Program_Tree;
      Parent   : Entity_Id;
      Name     : Name_Id)
      return Entity_Id;

   procedure Analyse_Class_Header
     (Class  : Node_Id;
      Header : Node_Id);

   procedure Analyse_Class_Name
     (Class         : Node_Id;
      Class_Name    : Node_Id;
      Defining_Name : Boolean);

   procedure Analyse_Features
     (Class    : Node_Id;
      Features : Node_Id);

   procedure Analyse_Feature_Clause
     (Class  : Node_Id;
      Clause : Node_Id);

   procedure Analyse_Feature_Declaration
     (Class   : Node_Id;
      Feature : Node_Id);

   procedure Analyse_Entity_Declaration_Groups
     (Class  : Node_Id;
      Table  : Entity_Id;
      Groups : List_Id;
      Kind   : Local_Entity_Kind);

   procedure Analyse_Routine
     (Class  : Node_Id;
      Table  : Entity_Id;
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
      Features_Node : constant Node_Id := Features (Node);
   begin
      Analyse_Class_Header (Node, Class_Header (Node));
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
   begin
      Analyse_Class_Name (Class, Class_Name (Header),
                          Defining_Name => True);
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
      Parent : Entity_Id := No_Entity;
      Class_Program : constant Aquarius.Programs.Program_Tree :=
                        Node_Table.Element (Class).From;
   begin
      while Position /= Last loop
         declare
            Element_Node : constant Node_Id :=
                             List_Of_Nodes.Element (Position);
            Element_Name : constant Name_Id :=
                             Node_Table.Element (Element_Node).Name;
            New_Parent   : constant Entity_Id :=
                             Load_Entity (Class_Program, Parent, Element_Name);
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
            Aquarius.Ack.Classes.Add_Class
              (Parent, Last_Name, Class);
         end;
      else
         Set_Entity (Class_Name, Parent);
      end if;

   end Analyse_Class_Name;

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
      Analyse_Compound (Class, Table, Compound (Routine));
   end Analyse_Effective_Routine;

   ---------------------------------------
   -- Analyse_Entity_Declaration_Groups --
   ---------------------------------------

   procedure Analyse_Entity_Declaration_Groups
     (Class  : Node_Id;
      Table  : Entity_Id;
      Groups : List_Id;
      Kind   : Local_Entity_Kind)
   is
      procedure Insert_Group (Group_Node : Node_Id);

      ------------------
      -- Insert_Group --
      ------------------

      procedure Insert_Group (Group_Node : Node_Id) is
         Ids        : constant List_Id := Identifiers (Group_Node);
         Group_Type : constant Node_Id := Entity_Type (Group_Node);
         Class_Node  : constant Node_Id :=
                         (if Group_Type /= No_Node
                          then Class_Name (Group_Type)
                          else No_Node);
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
         end Insert_Id;

      begin
         Analyse_Class_Name (Class, Class_Node, False);

         Type_Entity :=
           (if Class_Node /= No_Node
            then Get_Entity (Class_Node)
            else No_Entity);

         Scan (Ids, Insert_Id'Access);
      end Insert_Group;

   begin
      Scan (Groups, Insert_Group'Access);
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
            null;
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
      Names       : constant List_Id := New_Feature_List (Feature);
      Dec_Body    : constant Node_Id := Declaration_Body (Feature);
      Arg_Node    : constant Node_Id := Formal_Arguments (Dec_Body);
      Type_Node   : constant Node_Id := Value_Type (Dec_Body);
      Value_Node  : constant Node_Id := Value (Dec_Body);
      Class_Node  : constant Node_Id :=
                      (if Type_Node /= No_Node
                       then Class_Name (Type_Node)
                       else No_Node);
      Type_Entity : Entity_Id := No_Entity;
      Local_Table : Entity_Id := No_Entity;
      Single      : constant Boolean :=
                      Natural (List_Table.Element (Names).List.Length) = 1;
      Single_Name : constant Name_Id :=
                      (if Single
                       then Get_Name
                         (Feature_Name
                            (List_Table.Element (Names).List.First_Element))
                       else No_Name);
   begin

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

      if Single and then
        (Arg_Node /= No_Node or else Value_Node /= No_Node)
      then
         Local_Table :=
           New_Entity
             (Name        => Single_Name,
              Kind        => Table_Entity,
              Context     => Get_Entity (Class),
              Declaration => Feature,
              Entity_Type => No_Entity);
      end if;

      if Single and then Arg_Node /= No_Node then
         Analyse_Entity_Declaration_Groups
           (Class, Local_Table, Entity_Declaration_Groups (Arg_Node),
            Kind => Argument_Entity);
         Set_Entity (Feature, Local_Table);
      end if;

      if Class_Node /= No_Node then
         Analyse_Class_Name (Class, Class_Node,
                             Defining_Name => False);
         Type_Entity := Get_Entity (Class_Node);
      end if;

      if Value_Node /= No_Node then
         if Kind (Value_Node) = N_Routine then
            Analyse_Routine (Class, Local_Table, Value_Node);
         end if;
      end if;

      for Node of List_Table.Element (Names).List loop
         declare
            Entity : constant Entity_Id :=
                       New_Entity
                         (Name        => Get_Name (Feature_Name (Node)),
                          Kind        => Feature_Entity,
                          Context     => Get_Entity (Class),
                          Declaration => Node,
                          Entity_Type => Type_Entity);
         begin
            Set_Entity (Node, Entity);
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

   -----------------------
   -- Analyse_Precursor --
   -----------------------

   procedure Analyse_Precursor
     (Class           : Node_Id;
      Table           : Entity_Id;
      Expression_Type : Entity_Id;
      Precursor       : Node_Id)
   is
      pragma Unreferenced (Class);
      List   : constant List_Id :=
                 Node_Table (Precursor).List;

      Local_Table  : Entity_Id := Table;
      Value_Entity : Entity_Id := No_Entity;
      Value_Type   : Entity_Id := No_Entity;

      procedure Process (Element : Node_Id);

      -------------
      -- Process --
      -------------

      procedure Process (Element : Node_Id) is
         Entity : constant Entity_Id :=
                    Find_Entity (Local_Table, Get_Name (Element));
      begin
         if Entity = Undeclared_Entity then
            Error (Element, E_Undeclared_Name);
         else
            Set_Entity (Element, Entity);
            Value_Entity := Entity;
            Value_Type := Get_Type (Value_Entity);
            Local_Table := Value_Type;
         end if;
      end Process;

   begin

      Aquarius.Ack.IO.Put_Line (Precursor);

      Scan (List, Process'Access);

      if Value_Entity /= No_Entity then
         Set_Entity (Precursor, Value_Entity);

         if not Aquarius.Ack.Classes.Is_Derived_From
           (Expression_Type, Value_Type)
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

   -----------------
   -- Load_Entity --
   -----------------

   function Load_Entity
     (Referrer : Aquarius.Programs.Program_Tree;
      Parent   : Entity_Id;
      Name     : Name_Id)
      return Entity_Id
   is
      Entity : Entity_Id := Find_Entity (Parent, Name);
   begin
      if Entity = Undeclared_Entity then
         declare
            Path : constant String :=
                     Aquarius.Ack.Files.Find_Class_File
                       (Referrer, Parent, Name);
         begin
            if Path /= "" then
               declare
                  Program : constant Aquarius.Programs.Program_Tree :=
                              Aquarius.Loader.Load_From_File
                                (Path);
                  Node    : constant Node_Id :=
                              Aquarius.Ack.Parser.Import (Program);
               begin
                  Aquarius.Ack.Semantic.Analyse_Class_Declaration (Node);
                  Entity := Get_Entity (Node);
               end;
            end if;
         end;
      end if;
      return Entity;
   end Load_Entity;

end Aquarius.Ack.Semantic;