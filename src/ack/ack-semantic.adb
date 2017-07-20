with Aquarius.Loader;

with Ack.Files;
with Ack.Parser;

with Ack.Classes;
with Ack.Features;

with Ack.Environment;

with Ack.Errors;

package body Ack.Semantic is

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

   procedure Analyse_Features
     (Class    : Ack.Classes.Class_Entity;
      Features : Node_Id)
     with Pre => Kind (Features) = N_Features;

   procedure Analyse_Feature_Bodies
     (Class    : Ack.Classes.Class_Entity;
      Features : Node_Id)
     with Pre => Kind (Features) = N_Features;

   procedure Analyse_Inheritance
     (Class       : Ack.Classes.Class_Entity;
      Inheritance : Node_Id);

   procedure Analyse_Inherit
     (Class   : Ack.Classes.Class_Entity;
      Inherit : Node_Id);

   procedure Analyse_Feature_Clause
     (Class   : Ack.Classes.Class_Entity;
      Clause  : Node_Id;
      Headers : Boolean;
      Bodies  : Boolean);

   procedure Analyse_Feature_Declaration
     (Class      : Ack.Classes.Class_Entity;
      Feature    : Node_Id;
      Headers    : Boolean;
      Bodies     : Boolean);

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

   procedure Analyse_Routine
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Routine   : Node_Id);

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

   procedure Analyse_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : Ack.Entity_Type;
      Expression      : Node_Id);

   procedure Analyse_Precursor
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : Ack.Entity_Type;
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
      if Inheritance_Node /= No_Node then
         Analyse_Inheritance (Class, Inheritance_Node);
      end if;

      if Features_Node in Real_Node_Id then
         Analyse_Features (Class, Features_Node);
      end if;

      Class.Bind;

      if Features_Node in Real_Node_Id then
         Analyse_Feature_Bodies (Class, Features_Node);
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
      use type Ack.Classes.Class_Entity;
      Name_Node     : constant Node_Id := Class_Name (Type_Node);
      Generics_Node : constant Node_Id := Actual_Generics (Type_Node);
      Type_Entity   : Ack.Classes.Class_Entity := null;

   begin
      Analyse_Class_Name (Class, Name_Node, False);
      Type_Entity := Ack.Classes.Get_Class_Entity (Name_Node);

      if Type_Entity = null then
         return;
      end if;

      if Generics_Node /= No_Node then
         declare
            Actual_Nodes : constant Array_Of_Nodes :=
                             To_Array (Actual_Generics_List (Generics_Node));
            Actual_Types : Ack.Classes.Array_Of_Classes
              (Actual_Nodes'Range);
         begin
            for I in Actual_Nodes'Range loop
               Analyse_Type (Class, Actual_Nodes (I));
               Actual_Types (I) :=
                 Ack.Classes.Get_Class_Entity (Actual_Nodes (I));
            end loop;

            Type_Entity :=
              Ack.Classes.New_Instantiated_Class
                (Type_Entity, Actual_Types, Type_Node);
         end;
      end if;

      Set_Entity (Type_Node, Type_Entity);

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
               null;
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
         Type_Entity : Entity_Type := null;

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
            Type_Entity := Get_Entity (Type_Node);
         end if;

         if Type_Entity /= null then
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
      Expression_Type : Ack.Entity_Type;
      Expression      : Node_Id)
   is
   begin
      case N_Expression_Node (Kind (Expression)) is
         when N_Operator =>
            null;
         when N_Precursor =>
            Analyse_Precursor
              (Class, Container, Expression_Type, Expression);
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
               Value_Type : constant Ack.Classes.Class_Entity :=
                              Ack.Classes.Get_Top_Level_Class (Type_Name);
            begin
               Set_Entity (Expression, Value_Type);
               if Expression_Type = null then
                  Error (Value, E_Ignored_Return_Value);
               elsif not  Value_Type.Conforms_To (Expression_Type) then
                  Error (Expression, E_Type_Error, Expression_Type);
               end if;
            end;
      end case;
   end Analyse_Expression;

   ----------------------------
   -- Analyse_Feature_Bodies --
   ----------------------------

   procedure Analyse_Feature_Bodies
     (Class    : Ack.Classes.Class_Entity;
      Features : Node_Id)
   is
      Clause_List : constant List_Id :=
                      Feature_Clauses (Features);
   begin
      for Clause_Node of List_Table.Element (Clause_List).List loop
         Analyse_Feature_Clause (Class, Clause_Node,
                                 Headers => False, Bodies => True);
      end loop;
   end Analyse_Feature_Bodies;

   ----------------------------
   -- Analyse_Feature_Clause --
   ----------------------------

   procedure Analyse_Feature_Clause
     (Class   : Ack.Classes.Class_Entity;
      Clause  : Node_Id;
      Headers : Boolean;
      Bodies  : Boolean)
   is
      Feature_List : constant List_Id :=
                       Feature_Declarations (Clause);
   begin
      if Feature_List in Real_List_Id then
         for Feature_Node of List_Table.Element (Feature_List).List loop
            Analyse_Feature_Declaration
              (Class, Feature_Node,
               Headers => Headers, Bodies => Bodies);
         end loop;
      end if;
   end Analyse_Feature_Clause;

   ---------------------------------
   -- Analyse_Feature_Declaration --
   ---------------------------------

   procedure Analyse_Feature_Declaration
     (Class      : Ack.Classes.Class_Entity;
      Feature    : Node_Id;
      Headers    : Boolean;
      Bodies     : Boolean)
   is
      Names        : constant List_Id := New_Feature_List (Feature);
      Dec_Body     : constant Node_Id := Declaration_Body (Feature);
      Arg_Node     : constant Node_Id := Formal_Arguments (Dec_Body);
      Type_Node    : constant Node_Id := Value_Type (Dec_Body);
      Value_Node   : constant Node_Id := Value (Dec_Body);
      Type_Entity  : Ack.Classes.Class_Entity := null;
      Single       : constant Boolean :=
                       Natural (List_Table.Element (Names).List.Length) = 1;
      Routine      : constant Boolean :=
                       Single and then
                           (Arg_Node /= No_Node or else Value_Node /= No_Node);
   begin

      if Headers then
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
            Analyse_Class_Type (Class, Type_Node);
            Type_Entity := Ack.Classes.Get_Class_Entity (Type_Node);
         end if;
      end if;

      for Node of List_Table.Element (Names).List loop
         declare
            use type Ack.Classes.Class_Entity;
            use type Ack.Features.Feature_Entity;
            Entity : Ack.Features.Feature_Entity;
         begin
            if Headers then
               if Routine then
                  Entity :=
                    Ack.Features.New_Routine_Feature
                      (Name          => Get_Name (Feature_Name (Node)),
                       Class         => Class,
                       Result_Type   => Type_Entity,
                       Declaration   => Node,
                       Routine       => Value_Node);
               elsif Type_Entity /= null then
                  Entity :=
                    Ack.Features.New_Property_Feature
                      (Name          => Get_Name (Feature_Name (Node)),
                       Class         => Class,
                       Property_Type => Type_Entity,
                       Declaration   => Node);
               end if;

               if Entity /= null then
                  Set_Entity (Node, Entity);
               end if;

            else
               Entity := Ack.Features.Feature_Entity (Get_Entity (Node));
            end if;

            if Headers and then Single and then Arg_Node /= No_Node then
               Analyse_Entity_Declaration_Groups
                 (Class, Entity,
                  Entity_Declaration_Group_List (Arg_Node),
                  Local => False);
            end if;

            if Bodies and then Entity /= null then
               Entity.Bind;

               if Bodies and then Value_Node /= No_Node then
                  if Kind (Value_Node) = N_Routine then
                     Analyse_Routine (Class, Entity, Value_Node);
                  end if;
               end if;
            end if;

         end;
      end loop;

   end Analyse_Feature_Declaration;

   ----------------------
   -- Analyse_Features --
   ----------------------

   procedure Analyse_Features
     (Class    : Ack.Classes.Class_Entity;
      Features : Node_Id)
   is
      Clause_List : constant List_Id :=
                      Feature_Clauses (Features);
   begin
      for Clause_Node of List_Table.Element (Clause_List).List loop
         Analyse_Feature_Clause
           (Class, Clause_Node,
            Headers => True, Bodies => False);
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
         Generic_Entity : constant Ack.Classes.Class_Entity :=
                            Ack.Classes.New_Generic_Formal (Name, Node);
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
      Class_Node      : constant Node_Id := Class_Name (Class_Type);
      Redefine_List   : constant List_Id := Redefine (Inherit);

      Inherited_Class : Ack.Classes.Class_Entity;

      procedure Set_Redefine (Node : Node_Id);

      ------------------
      -- Set_Redefine --
      ------------------

      procedure Set_Redefine (Node : Node_Id) is
      begin
         Class.Redefine (Inherited_Class, Get_Name (Node));
      end Set_Redefine;

   begin

      Analyse_Class_Name (Class, Class_Node,
                          Defining_Name => False);
      Inherited_Class := Ack.Classes.Get_Class_Entity (Class_Node);

      Set_Entity (Inherit, Inherited_Class);

      Class.Inherit (Ack.Classes.Get_Class_Entity (Class_Node));
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

   -----------------------
   -- Analyse_Precursor --
   -----------------------

   procedure Analyse_Precursor
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Expression_Type : Ack.Entity_Type;
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
               Error (Precursor, E_Type_Error, Expression_Type);
            end if;
         end if;
      end if;

   end Analyse_Precursor;

   ---------------------
   -- Analyse_Routine --
   ---------------------

   procedure Analyse_Routine
     (Class     : Ack.Classes.Class_Entity;
      Container : not null access Root_Entity_Type'Class;
      Routine   : Node_Id)
   is
   begin
      Analyse_Effective_Routine
        (Class     => Class,
         Container => Container,
         Routine   => Effective_Routine (Routine));
   end Analyse_Routine;

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
      return Ack.Classes.Class_Entity (Entity);
   end Load_Class;

end Ack.Semantic;
