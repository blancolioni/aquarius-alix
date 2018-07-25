with Ada.Text_IO;

with Komnenos.Entities.Tables;

with Aquarius.Loader;
with Aquarius.Programs.Komnenos_Entities;

with Ack.Files;
with Ack.Parser;

with Ack.Features;
with Ack.Types;
with Ack.Variables;

with Ack.Attachment;
with Ack.Semantic.Work;

with Ack.Generate.Primitives;

with Ack.Environment;

package body Ack.Semantic is

   Local_Integral_Type         : Ack.Types.Type_Entity := null;
   Local_Iterable_Type         : Ack.Types.Type_Entity := null;
   Local_Iterable_Class        : Ack.Classes.Class_Entity := null;

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

   function Type_Any return Ack.Types.Type_Entity
   is (Get_Top_Level_Type ("any"))
   with Unreferenced;

   function Type_Integral (Node : Node_Id) return Ack.Types.Type_Entity;
   function Type_String return Ack.Types.Type_Entity
   is (Get_Top_Level_Type ("string"));

   function Type_Character return Ack.Types.Type_Entity
   is (Get_Top_Level_Type ("character"));

   function Type_Boolean return Ack.Types.Type_Entity
   is (Get_Top_Level_Type ("boolean"));

   function Class_Iterable
     return Ack.Classes.Class_Entity;

   function Type_Iterable
     return Ack.Types.Type_Entity;

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
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Assertion  : Node_Id;
      Process    : not null access
        procedure (Tag : Name_Id;
                   Condition : Node_Id));

   procedure Analyse_Effective_Routine
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Routine    : Node_Id);

   procedure Analyse_Compound
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Compound   : Node_Id);

   procedure Analyse_Assignment
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Assignment : Node_Id);

   procedure Analyse_Conditional
     (Class       : Ack.Classes.Class_Entity;
      Container   : not null access Root_Entity_Type'Class;
      Attachment  : in out Ack.Attachment.Attachment_Context'Class;
      Conditional : Node_Id);

   procedure Analyse_Creation
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Creation   : Node_Id);

   procedure Analyse_Loop
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Loop_Node  : Node_Id);

   procedure Analyse_Check
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Check      : Node_Id)
   is null;

   procedure Analyse_Retry
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Retry      : Node_Id);

   procedure Analyse_Boolean_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression      : Node_Id);

   procedure Analyse_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : not null access Root_Entity_Type'Class;
      Expression      : Node_Id);

   procedure Analyse_Operator
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Operator_Node   : Node_Id);

   procedure Analyse_Precursor
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Precursor       : Node_Id);

   procedure Analyse_Tuple_Expression
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : access Root_Entity_Type'Class;
      Expression      : Node_Id);

   procedure Analyse_Actual_Arguments
     (Class            : Ack.Classes.Class_Entity;
      Container        : not null access Root_Entity_Type'Class;
      Attachment       : in out Ack.Attachment.Attachment_Context'Class;
      Entity           : Entity_Type;
      Actual_List_Node : Node_Id)
     with Unreferenced;

   ------------------------------
   -- Analyse_Actual_Arguments --
   ------------------------------

   procedure Analyse_Actual_Arguments
     (Class            : Ack.Classes.Class_Entity;
      Container        : not null access Root_Entity_Type'Class;
      Attachment       : in out Ack.Attachment.Attachment_Context'Class;
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
               Error (Actual_List_Node, E_Does_Not_Accept_Arguments, Entity);
            elsif Actuals'Length > Entity.Argument_Count then
               Error (Actual_List_Node, E_Too_Many_Arguments);
            elsif Actuals'Length < Entity.Argument_Count then
               Error (Actual_List_Node, E_Insufficient_Arguments);
            else
               for I in Actuals'Range loop
                  Analyse_Expression
                    (Class           => Class,
                     Container       => Container,
                     Attachment      => Attachment,
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
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Assertion  : Node_Id;
      Process    : not null access
        procedure (Tag : Name_Id;
                   Condition : Node_Id))
   is

      procedure Analyse_Clause (Clause : Node_Id);

      --------------------
      -- Analyse_Clause --
      --------------------

      procedure Analyse_Clause (Clause : Node_Id) is
      begin
         Analyse_Boolean_Expression
           (Class, Container, Attachment, Expression (Clause));
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
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
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
            if Ack.Features.Is_Feature (Entity) then
               Ack.Semantic.Work.Check_Work_Item
                 (Class        =>
                    Ack.Features.Feature_Entity (Entity).Definition_Class,
                  Feature_Name => Get_Name (Variable (Assignment)),
                  Category     => Ack.Semantic.Work.Feature_Header);
            end if;

            Analyse_Expression (Class, Container, Attachment,
                                Entity.Get_Type,
                                Expression (Assignment));
            Set_Entity (Variable (Assignment), Entity);
            Set_Context (Variable (Assignment), Container);
            Attachment.Transfer_Current_Context_Attachment (Entity);
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
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression      : Node_Id)
   is
   begin
      Analyse_Expression
        (Class, Container, Attachment,
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
      Features_Node    : constant Node_Id := Class_Features (Node);
      Class            : constant Ack.Classes.Class_Entity :=
                           Analyse_Class_Header (Node, Class_Header (Node));

      procedure Add_Feature_Work_Items
        (Features_Node : Node_Id;
         Category      : Ack.Semantic.Work.Work_Item_Category);

      procedure Add_Feature_Work_Items
        (Features_Node : Node_Id;
         Category      : Ack.Semantic.Work.Work_Item_Category)
      is
         Clause_List : constant List_Id :=
                         Feature_Clauses (Features_Node);
      begin
         for Clause_Node of List_Table.Element (Clause_List).List loop
            declare
               Feature_List : constant List_Id :=
                                Feature_Declarations (Clause_Node);
            begin
               for Feature_Node of List_Table.Element (Feature_List).List loop
                  Ack.Semantic.Work.Add_Work_Item
                    (Category => Category,
                     Class    => Class,
                     Feature  => Feature_Node);
               end loop;
            end;
         end loop;
      end Add_Feature_Work_Items;

   begin

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Analysing: " & Class.Qualified_Name);
      end if;

      if Notes_Node in Real_Node_Id then
         Analyse_Notes (Class, Notes_Node);
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
              ("Adding feature work items: " & Class.Qualified_Name);
         end if;

         Add_Feature_Work_Items
           (Features_Node, Ack.Semantic.Work.Feature_Header);

      end if;

      Ack.Semantic.Work.Add_Work_Item
        (Category  => Ack.Semantic.Work.Class_Binding,
         Class     => Class,
         Feature   => No_Node);

      if Features_Node in Real_Node_Id then

         Add_Feature_Work_Items
           (Features_Node, Ack.Semantic.Work.Feature_Body);

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

      Ack.Semantic.Work.Add_Work_Item
        (Category  => Ack.Semantic.Work.Class_Layout,
         Class     => Class,
         Feature   => No_Node);

      Ack.Semantic.Work.Add_Work_Item
        (Category  => Ack.Semantic.Work.Error_Report,
         Class     => Class,
         Feature   => No_Node);

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
      Creators_Node        : constant Node_Id := Class_Creators (Class);
      Result               : Ack.Classes.Class_Entity;
   begin
      Analyse_Class_Name (null, Class_Name (Header),
                          Defining_Name => True);
      Result := Ack.Classes.Get_Class_Entity (Class_Name (Header));

      Result.Set_Top_Class_Node (Class);

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
                  Result.Add_Creator
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

      if Has_Entity (Class_Name) then
         Ada.Text_IO.Put_Line
           (Get_Program (Class_Name).Show_Location
            & ": cannot set entity to "
            & Parent.Identity & " " & Parent.Qualified_Name);
         Ada.Text_IO.Put_Line
           (Get_Program (Class_Name).Show_Location
            & ": already contains entity "
            & Get_Entity (Class_Name).Identity
            & " " & Get_Entity (Class_Name).Description);
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
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Compound   : Node_Id)
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
               Analyse_Assignment (Class, Container, Attachment, Node);
            when N_Creation_Instruction =>
               Analyse_Creation (Class, Container, Attachment, Node);
            when N_Conditional =>
               Analyse_Conditional (Class, Container, Attachment, Node);
            when N_Loop =>
               Analyse_Loop (Class, Container, Attachment, Node);
            when N_Precursor =>
               Analyse_Precursor
                 (Class           => Class,
                  Container       => Container,
                  Attachment      => Attachment,
                  Expression_Type => null,
                  Precursor       => Node);
            when N_Check =>
               Analyse_Check (Class, Container, Attachment, Node);
            when N_Retry =>
               Analyse_Retry (Class, Container, Attachment, Node);
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
      Attachment  : in out Ack.Attachment.Attachment_Context'Class;
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
              (Class, Container, Attachment,
               Condition);
            if Implicit_Entity (Condition) then
               Container.Add_Implicit (Get_Entity (Condition));
            end if;
         end if;
         Analyse_Compound (Class, Container, Attachment, Compound);
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
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Creation   : Node_Id)
   is
      Explicit_Type_Node : constant Node_Id :=
                             Explicit_Creation_Type (Creation);
      Call_Node          : constant Node_Id := Creation_Call (Creation);
      Explicit_Call_Node : constant Node_Id :=
                             Explicit_Creation_Call (Call_Node);
      Actual_List_Node   : constant Node_Id :=
                             (if Explicit_Call_Node = No_Node then No_Node
                              else Actual_List (Explicit_Call_Node));
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

      if Ack.Features.Is_Feature (Created_Entity) then
         Ack.Semantic.Work.Check_Work_Item
           (Ack.Features.Feature_Entity (Created_Entity).Active_Class,
            Created_Entity.Entity_Name_Id,
            Ack.Semantic.Work.Feature_Header);
      end if;

      Set_Context (Creation, Container);
      Set_Entity (Creation, Created_Entity);

      Created_Type := Created_Entity.Get_Type;

      if Created_Type = null then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Get_Program (Variable_Node).Show_Location
            & ": no type for entity '"
            & Created_Entity.Declared_Name
            & "'");
      end if;

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

      if not Created_Type.Conforms_To (Created_Entity.Get_Type) then
         Error (Creation, E_Creation_Type_Error, Created_Type);
      end if;

      if Explicit_Call_Node not in Real_Node_Id then
         Ack.Semantic.Work.Check_Work_Item
           (Ack.Types.Type_Entity (Created_Type).Class,
            No_Name, Ack.Semantic.Work.Class_Binding);

         if not Created_Type.Has_Default_Creation_Routine then
            Error (Creation, E_No_Default_Create_Routine);
         end if;
      end if;

      if Explicit_Call_Node in Real_Node_Id then

         Creator_Name := Get_Name (Explicit_Call_Node);

         Ack.Semantic.Work.Check_Work_Item
           (Ack.Types.Type_Entity (Created_Type).Class,
            Creator_Name, Ack.Semantic.Work.Feature_Header);

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
                  if Actual_List_Node /= No_Node then
                     declare
                        Actuals : constant Array_Of_Nodes :=
                                    To_Array
                                      (Node_Table.Element
                                         (Actual_List_Node).List);
                     begin
                        if Creator.Argument_Count = 0 then
                           Error (Actual_List_Node,
                                  E_Does_Not_Accept_Arguments,
                                  Creator);
                        elsif Actuals'Length > Creator.Argument_Count then
                           Error (Actual_List_Node, E_Too_Many_Arguments);
                        elsif Actuals'Length < Creator.Argument_Count then
                           Error (Actual_List_Node, E_Insufficient_Arguments);
                        else
                           for I in Actuals'Range loop
                              Attachment.Save_State;
                              Analyse_Expression
                                (Class           => Class,
                                 Container       => Container,
                                 Attachment      => Attachment,
                                 Expression_Type =>
                                   Creator.Argument (I).Get_Type,
                                 Expression      => Actuals (I));
                              Set_Destination_Type
                                (Actuals (I), Creator.Argument (I).Get_Type);
                              Attachment.Restore_State;
                           end loop;
                        end if;

                        Attachment.Detach_Current_Context;

                     end;
                  end if;
               end if;
            end;
         else
            Error (Explicit_Call_Node, E_Not_Defined_In,
                   Created_Type);
         end if;
      end if;

      Attachment.Attach (Created_Entity);

   end Analyse_Creation;

   -------------------------------
   -- Analyse_Effective_Routine --
   -------------------------------

   procedure Analyse_Effective_Routine
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Routine    : Node_Id)
   is
   begin
      case N_Effective_Routine (Kind (Routine)) is
         when N_Internal =>
            Analyse_Compound (Class, Container, Attachment,
                              Compound (Routine));
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
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
      Expression_Type : not null access Root_Entity_Type'Class;
      Expression      : Node_Id)
   is
      K : constant Node_Kind := Kind (Expression);
   begin
      case N_Expression_Node (K) is
         when N_Operator =>
            Analyse_Operator
              (Class, Container, Attachment, Expression_Type, Expression);
         when N_Precursor =>
            Analyse_Precursor
              (Class, Container, Attachment, Expression_Type, Expression);
         when N_Old =>
            Analyse_Expression (Class, Container, Attachment,
                                Expression_Type,
                                Ack.Expression (Expression));
            Container.Save_Old_Value (Ack.Expression (Expression));
         when N_Tuple =>
            Analyse_Tuple_Expression
              (Class, Container, Attachment, Expression_Type, Expression);
         when N_Attachment_Test =>
            Analyse_Expression (Class, Container, Attachment,
                                Get_Top_Level_Type ("any"),
                                Expression => Field_1 (Expression));
            if Get_Name (Expression) /= No_Name
              and then Get_Type (Field_1 (Expression)) /= null
            then
               declare
                  Implicit : constant Ack.Variables.Variable_Entity :=
                               Ack.Variables.New_Local_Entity
                                 (Name       => Get_Name (Expression),
                                  Node       => Expression,
                                  Local_Type =>
                                    Get_Type (Field_1 (Expression)));
               begin
                  Implicit.Set_Attached;
                  Attachment.Attach (Implicit);
                  Set_Type (Expression,
                            Ack.Types.Get_Top_Level_Type ("boolean"));
                  Set_Entity (Expression, Implicit);
                  Set_Implicit_Entity (Expression);
               end;
            end if;

         when N_Constant =>
            declare
               Value     : constant Node_Id := Constant_Value (Expression);
               Value_Type : constant Ack.Types.Type_Entity :=
                              (case N_Constant_Value (Kind (Value)) is
                                  when N_String_Constant  =>
                                     Type_String,
                                  when N_Character_Constant =>
                                     Type_Character,
                                  when N_Integer_Constant =>
                                     Type_Integral (Value),
                                  when N_Boolean_Constant =>
                                     Type_Boolean);
            begin
               if Kind (Value) = N_Integer_Constant then
                  Set_Type (Expression, Expression_Type);
                  Set_Entity (Expression, Expression_Type);
                  if not Expression_Type.Conforms_To (Value_Type) then
                     Error (Expression, E_Type_Error, Value_Type);
                  end if;
               else
                  Set_Type (Expression, Value_Type);
                  Set_Entity (Expression, Value_Type);
                  if not  Value_Type.Conforms_To (Expression_Type) then
                     Error (Expression, E_Type_Error,
                            Entity_Type (Expression_Type));
                  end if;
               end if;
            end;
      end case;

   end Analyse_Expression;

   --------------------------
   -- Analyse_Feature_Body --
   --------------------------

   procedure Analyse_Feature_Body
     (Class   : Ack.Classes.Class_Entity;
      Feature : Node_Id)
   is
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
                             (if Routine_Feature
                              then Local_Declarations (Value_Node)
                              else No_Node);
      Effective_Node     : constant Node_Id :=
                             (if Effective then Effective_Routine (Value_Node)
                           else No_Node);
      Internal           : constant Boolean :=
                             Effective
                                 and then Kind (Effective_Node) = N_Internal;
      External           : constant Boolean :=
                          Effective
                                 and then Kind (Effective_Node) = N_External;

      Attachment         : Ack.Attachment.Attachment_Context;

   begin

      if not Property_Feature_Node (Feature)
        and then not Value_Feature
        and then not Routine_Feature
      then
         Error (Feature, E_Requires_Body);
      end if;

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
               null;  --  already analysed by feature header
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
                  Analyse_Assertion (Class, Entity, Attachment,
                                     Assertion (Precondition_Node),
                                     Add_Precondition'Access);
               end;
            end if;

            if Rescue_Node /= No_Node then
               declare
                  Rescue_Attachment : Ack.Attachment.Attachment_Context;
               begin
                  Analyse_Compound (Class, Entity, Rescue_Attachment,
                                    Compound (Rescue_Node));
                  Entity.Set_Rescue_Node (Rescue_Node);
               end;
            end if;

            if Internal then
               for I in 1 .. Entity.Argument_Count loop
                  Attachment.Attach (Entity.Argument (I));
               end loop;

               Analyse_Effective_Routine
                 (Class, Entity, Attachment, Effective_Node);
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
                  Analyse_Assertion (Class, Entity, Attachment,
                                     Assertion (Postcondition_Node),
                                     Add_Postcondition'Access);
               end;
            end if;

         end;
      end loop;
   end Analyse_Feature_Body;

   ----------------------------
   -- Analyse_Feature_Header --
   ----------------------------

   procedure Analyse_Feature_Header
     (Class   : Ack.Classes.Class_Entity;
      Feature : Node_Id)
   is
      Names        : constant List_Id := New_Feature_List (Feature);
      Dec_Body     : constant Node_Id := Declaration_Body (Feature);
      Arg_Node     : constant Node_Id := Formal_Arguments (Dec_Body);
      Type_Node    : constant Node_Id := Value_Type (Dec_Body);

      procedure Analyse_Ancestor_Feature
        (Ancestor_Class    : Ack.Classes.Class_Entity;
         Redefinition_Node : Node_Id;
         Ancestor_Feature  : Name_Id);

      ------------------------------
      -- Analyse_Ancestor_Feature --
      ------------------------------

      procedure Analyse_Ancestor_Feature
        (Ancestor_Class    : Ack.Classes.Class_Entity;
         Redefinition_Node : Node_Id;
         Ancestor_Feature  : Name_Id)
      is
         pragma Unreferenced (Redefinition_Node);
      begin
         Ack.Semantic.Work.Check_Work_Item
           (Class        => Ancestor_Class,
            Feature_Name => Ancestor_Feature,
            Category     => Ack.Semantic.Work.Feature_Header);
      end Analyse_Ancestor_Feature;

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

         Class.Scan_Redefinitions
           (Get_Entity (Node).Entity_Name_Id,
            Analyse_Ancestor_Feature'Access);

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
      Dec_Body        : constant Node_Id := Declaration_Body (Feature);
      Value_Node      : constant Node_Id := Value (Dec_Body);
      Routine_Feature : constant Boolean :=
                          Value_Node /= No_Node
                              and then Kind (Value_Node) = N_Routine;
      Deferred        : constant Boolean :=
                          Routine_Feature
                              and then Node_Table.Element
                                (Value_Node).Deferred;
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
            if Deferred then
               Entity.Set_Deferred;
            end if;
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

--        Redefined_Features : WL.String_Sets.Set;

      procedure Set_Redefine (Node : Node_Id);

--        procedure Check_Redefined
--          (Feature : not null access constant
--             Ack.Features.Feature_Entity_Record'Class);
--
--        ---------------------
--        -- Check_Redefined --
--        ---------------------
--
--        procedure Check_Redefined
--          (Feature : not null access constant
--             Ack.Features.Feature_Entity_Record'Class)
--        is
--        begin
--           if Inherited_Class.Feature
--             (Get_Name_Id (Feature.Standard_Name)).Deferred
--             and then not Redefined_Features.Contains (Feature.Standard_Name)
--           then
--              Error (Inherit, E_Missing_Redefine, Feature);
--           end if;
--        end Check_Redefined;

      ------------------
      -- Set_Redefine --
      ------------------

      procedure Set_Redefine (Node : Node_Id) is
         Name : constant Name_Id := Get_Name (Node);
      begin
         Class.Redefine (Node, Inherited_Class, Name);

--           Redefined_Features.Insert (To_Standard_String (Name));
--           if Inherited_Class.Has_Feature (Name) then
--              if Class.Has_Feature (Name) then
--                 Class.Feature (Name).Set_Redefined
--                   (Class            => Class,
--                    Original_Feature =>
--                      Inherited_Class.Feature (Name));
--              else
--                 Error (Node, E_Missing_Redefinition,
--                        Entity_Type (Inherited_Class));
--              end if;
--           else
--              Error (Node, E_Not_Defined_In, Entity_Type (Inherited_Class));
--           end if;
      end Set_Redefine;

   begin

      Analyse_Class_Type (Class, Class_Type);

      if Ack.Types.Has_Type_Entity (Class_Type) then
         Inherited_Type := Ack.Types.Get_Type_Entity (Class_Type);
         Inherited_Class := Inherited_Type.Class;

         Set_Entity (Inherit, Inherited_Type);

         Class.Inherit (Inherited_Type);
         Scan (Redefine_List, Set_Redefine'Access);

--           if not Class.Deferred then
--              Inherited_Class.Scan_Features
--                (Check_Redefined'Access);
--           end if;
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
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
      Loop_Node  : Node_Id)
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
            use type Ack.Types.Type_Entity;
            Expression_Node : constant Node_Id := Expression (Iteration_Node);
            Expression_Type : constant Ack.Types.Type_Entity :=
                                Type_Iterable;
            Iterable_Type   : Ack.Types.Type_Entity;
         begin

            Analyse_Expression
              (Class, Container, Attachment,
               Expression_Type, Expression_Node);

            Iterable_Type :=
              Ack.Types.Type_Entity
                (Get_Type (Expression_Node));

            if Iterable_Type = null then
               return;
            end if;

            Ack.Semantic.Work.Check_Work_Item
              (Class        => Iterable_Type.Class,
               Feature_Name => Get_Name_Id ("new_cursor"),
               Category     => Ack.Semantic.Work.Feature_Header);

            declare
               use Ack.Types;
               Inherited_Type     : constant access constant
                 Type_Entity_Record'Class
                   := Iterable_Type.Get_Ancestor_Type (Class_Iterable);

               New_Cursor_Feature : constant Ack.Features.Feature_Entity :=
                                      Iterable_Type.Feature
                                        (Get_Name_Id ("new_cursor"));
               Iterator_Type      : constant Ack.Types.Type_Entity :=
                                      Ack.Types.Type_Entity
                                        (New_Cursor_Feature.Get_Type);
               Generic_Bindings   : constant Natural :=
                                      Inherited_Type.Generic_Binding_Count;
               pragma Assert (Generic_Bindings = 1);
               Implicit_Type      : constant Ack.Types.Type_Entity :=
                                      Inherited_Type.Generic_Binding (1);
               Implicit           : constant Ack.Variables.Variable_Entity :=
                                      Ack.Variables.New_Iterator_Entity
                                        (Name       =>
                                           Get_Name (Iteration_Node),
                                         Node       => Iteration_Node,
                                         Iteration_Type =>
                                           Iterator_Type,
                                         Local_Type     => Implicit_Type);
            begin
               Implicit.Set_Attached;
               Set_Entity (Iteration_Node, Implicit);
               Set_Context (Iteration_Node, Container);
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
         Analyse_Compound (Class, Container, Attachment,
                           Compound (Initialization_Node));
      end if;

      if Exit_Condition_Node /= No_Node then
         Analyse_Boolean_Expression
           (Class, Container, Attachment,
            Expression (Exit_Condition_Node));
      end if;

      Analyse_Compound (Class, Container, Attachment,
                        Compound (Loop_Body_Node));

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

      Have_Single_Item    : Boolean;
      Have_Single_Integer : Boolean := False;
      Single_Integer      : Integer;

      procedure Add_Note_Items
        (Note_Name : String;
         List      : List_Id);

      --------------------
      -- Add_Note_Items --
      --------------------

      procedure Add_Note_Items
        (Note_Name : String;
         List      : List_Id)
      is

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
               Add_Note_Text (To_Standard_String (Get_Name (Item)));
            elsif Kind (Item) = N_Constant then
               declare
                  Value : constant Node_Id := Field_2 (Item);
               begin
                  case N_Constant_Value (Kind (Value)) is
                     when N_String_Constant =>
                        Add_Note_Text (To_String (Get_Text (Value)));
                     when N_Character_Constant =>
                        Add_Note_Text (To_String (Get_Name (Value)));
                     when N_Integer_Constant =>
                        Add_Note_Text (To_String (Get_Name (Value)));
                        if Have_Single_Item then
                           Have_Single_Integer := True;
                           Single_Integer :=
                             Integer'Value (To_String (Get_Name (Value)));
                        end if;

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
            Class.Add_Note (Note_Name, Text);
         end Add_Note_Text;

      begin
         Scan (List, Add_Note_Item'Access);
      end Add_Note_Items;

   begin
      for Note of List_Table.Element (List).List loop
         declare
            use type Ada.Containers.Count_Type;
            Name  : constant Name_Id := Get_Name (Note_Name (Note));
            Value : constant Node_Id := Note_Value (Note);
            Value_List : constant List_Id :=
                           Node_Table.Element (Value).List;
         begin

            Have_Single_Item := List_Table (Value_List).List.Length = 1;
            Have_Single_Integer := False;

            Add_Note_Items (To_Standard_String (Name), Value_List);

            if To_Standard_String (Name) = "aqua_modular_type" then
               if Have_Single_Integer then
                  declare
                     Modulus : Positive;
                  begin
                     Modulus := Single_Integer;
                     Class.Set_Modulus (Modulus);
                     Ack.Generate.Primitives
                       .Create_Integral_Primitives (Class);
                  end;
               else
                  raise Constraint_Error with
                  Get_Program (Value).Show_Location
                    & ": positive integer required for modulus bits";
               end if;
            end if;
         end;
      end loop;
   end Analyse_Notes;

   ----------------------
   -- Analyse_Operator --
   ----------------------

   procedure Analyse_Operator
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
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
         Attachment      => Attachment,
         Expression_Type => Get_Top_Level_Type ("any"),
         Expression      => Left);
      Left_Type := Ack.Types.Type_Entity (Get_Type (Left));

      if Left_Type /= null
        and then not Left_Type.Is_Generic_Formal_Type
      then
         Ack.Semantic.Work.Check_Work_Item
           (Left_Type.Class, No_Name, Ack.Semantic.Work.Class_Binding);
      end if;

      if Left_Type = null then
         null;
      elsif not Left_Type.Has_Aliased_Feature (Operator, Right /= No_Node) then
         Error (Operator_Node, E_Undeclared_Name);
      else
         declare
            Feature : constant Ack.Features.Feature_Entity :=
                        Left_Type.Aliased_Feature
                          (Alias => Operator,
                           Infix => Right /= No_Node);
         begin

            Ack.Semantic.Work.Check_Work_Item
              (Feature.Definition_Class, Feature.Entity_Name_Id,
               Ack.Semantic.Work.Feature_Header);

            pragma Assert (Feature.Argument_Count in 0 .. 1);
            if not Feature.Has_Type then
               raise Constraint_Error with
                 "attempted to treat non-value feature "
                 & Feature.Qualified_Name
                 & " as an operator";
            end if;

            if Feature.Argument_Count = 0 then
               if Right /= No_Node then
                  Error (Right, E_Too_Many_Arguments);
               end if;
            else
               if Right = No_Node then
                  Error (Operator_Node, E_Insufficient_Arguments);
               else
                  declare
                     Argument_Type : constant Ack.Types.Type_Entity :=
                                       Ack.Types.Type_Entity
                                         (Feature.Argument (1).Get_Type);
                     Expected_Type : constant Ack.Types.Type_Entity :=
                                       Ack.Types.Get_Concrete_Type
                                         (Of_Type => Argument_Type,
                                          Current => Left_Type,
                                          Feature => Feature);
                  begin
                     Analyse_Expression
                       (Class, Container, Attachment, Expected_Type, Right);
                  end;
               end if;
            end if;

            declare
               Result_Type : constant Ack.Types.Type_Entity :=
                               Ack.Types.Get_Concrete_Type
                                 (Of_Type => Ack.Types.Type_Entity
                                    (Feature.Get_Type),
                                  Current => Left_Type,
                                  Feature => Feature);
            begin
               Set_Type (Operator_Node, Result_Type);
               Set_Entity (Operator_Node, Feature);
               if not Result_Type.Conforms_To (Expression_Type) then
                  Error (Operator_Node, E_Type_Error,
                         Entity_Type (Expression_Type));
               end if;
            end;
         end;
      end if;

   end Analyse_Operator;

   -----------------------
   -- Analyse_Precursor --
   -----------------------

   procedure Analyse_Precursor
     (Class           : Ack.Classes.Class_Entity;
      Container       : not null access Root_Entity_Type'Class;
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
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
            Entity : constant Entity_Type := Local_Table.Get (Name);
         begin
            if Ack.Features.Is_Feature (Entity) then
               Ack.Semantic.Work.Check_Work_Item
                 (Ack.Classes.Constant_Class_Entity
                    (Local_Table.Class_Context),
                  Name,
                  Ack.Semantic.Work.Feature_Header);
            end if;
         end;

         declare
            use type Ack.Types.Constant_Type_Entity;
            Entity           : constant Entity_Type :=
                                 Local_Table.Get (Name);
            E_Type           : constant Ack.Types.Constant_Type_Entity :=
                                 Ack.Types.Constant_Type_Entity
                                   (Entity.Get_Type);
            Actual_List_Node : constant Node_Id :=
                                 Actual_List (Precursor_Element);
         begin

            Node_Table (Precursor_Element).Attached :=
              Attachment.Is_Attached (Entity);

            if Local_Warn_No_Default_Create
              and then E_Type /= null
              and then Entity.Standard_Name /= "void"
              and then not E_Type.Expanded
              and then Entity.Can_Update
              and then not Entity.Attached
              and then not E_Type.Detachable
              and then not E_Type.Deferred
              and then not E_Type.Is_Generic_Formal_Type
            then
               if not Attachment.Is_Attached (Entity) then
                  if not E_Type.Has_Default_Creation_Routine then
                     Warning (Precursor_Element,
                              E_Value_Might_Be_Void,
                              Entity);
                  end if;
               end if;
            end if;

            if Actual_List_Node /= No_Node then
               declare
                  Actuals : constant Array_Of_Nodes :=
                              To_Array
                                (Node_Table.Element
                                   (Actual_List_Node).List);
               begin
                  if Entity.Argument_Count = 0 then
                     Error (Actual_List_Node, E_Does_Not_Accept_Arguments,
                            Entity);
                  elsif Actuals'Length > Entity.Argument_Count then
                     Error (Actual_List_Node, E_Too_Many_Arguments);
                  elsif Actuals'Length < Entity.Argument_Count then
                     Error (Actual_List_Node, E_Insufficient_Arguments);
                  else
                     for I in Actuals'Range loop
                        Attachment.Save_State;
                        Analyse_Expression
                          (Class           => Class,
                           Container       => Container,
                           Attachment      => Attachment,
                           Expression_Type => Entity.Argument (I).Get_Type,
                           Expression      => Actuals (I));
                        Set_Destination_Type
                          (Actuals (I), Entity.Argument (I).Get_Type);
                        Attachment.Restore_State;
                     end loop;
                  end if;

                  Attachment.Detach_Current_Context;

               end;

            else
               Attachment.Set_Current_Context_Attachment (Entity);
            end if;

            Set_Entity (Precursor_Element, Entity);
            Set_Context (Precursor_Element, Local_Table.Class_Context);

            Value_Entity := Entity;
            Value_Type := Value_Entity.Get_Type;
            Local_Table := Value_Type;
         end;

      exception
         when others =>
            raise Constraint_Error with
            Get_Program (Precursor_Element).Show_Location
              & ": exception while processing precursor";

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

      Set_Context (Precursor, Container);

   end Analyse_Precursor;

   -------------------
   -- Analyse_Retry --
   -------------------

   procedure Analyse_Retry
     (Class      : Ack.Classes.Class_Entity;
      Container  : not null access Root_Entity_Type'Class;
      Attachment : in out Ack.Attachment.Attachment_Context'Class;
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
      Attachment      : in out Ack.Attachment.Attachment_Context'Class;
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

      Container.Add_Shelf ("tuple-expression");

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
           (Class, Container, Attachment,
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

      Attachment.Attach_Current_Context;

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

   --------------------
   -- Class_Iterable --
   --------------------

   function Class_Iterable
      return Ack.Classes.Class_Entity
   is
      use type Ack.Classes.Class_Entity;
   begin
      if Local_Iterable_Class = null then
         declare
            Aqua_Entity     : constant Ack.Classes.Class_Entity :=
                                Ack.Classes.Get_Top_Level_Class ("aqua");
            Iterable_Name   : constant String := "iterable";
         begin
            if Aqua_Entity.Contains (Iterable_Name) then
               Local_Iterable_Class :=
                 Ack.Classes.Class_Entity
                   (Aqua_Entity.Get (Iterable_Name));
            else
               Local_Iterable_Class :=
                 Load_Class (null, Aqua_Entity,
                             Get_Name_Id (Iterable_Name));
            end if;
         end;
      end if;

      return Local_Iterable_Class;
   end Class_Iterable;

   function Get_Class
     (Qualified_Name : String)
      return Ack.Classes.Constant_Class_Entity
   is
      Q_Name : constant String := Qualified_Name & ".";
      Start  : Positive := Q_Name'First;
      Class  : Ack.Classes.Class_Entity;
   begin
      for I in Q_Name'Range loop
         if Q_Name (I) = '.' then
            declare
               Name : constant String :=
                        Qualified_Name (Start .. I - 1);
            begin
               if Start = Q_Name'First then
                  Class :=
                    Ack.Classes.Get_Top_Level_Class (Name);
               elsif Class.Contains (Name) then
                  Class :=
                    Ack.Classes.Class_Entity
                      (Class.Get (Name));
               else
                  Class :=
                    Load_Class (null, Class, Get_Name_Id (Name));
               end if;
               Start := I + 1;
            end;
         end if;
      end loop;
      return Ack.Classes.Constant_Class_Entity (Class);
   end Get_Class;

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
                  Entity := Get_Entity (Node);
                  declare
                     Base_Name : constant String := Entity.Base_File_Name;
                  begin
                     Loaded_Classes.Insert
                       (Base_Name, Node);
                  end;

                  declare
                     use Aquarius.Programs.Komnenos_Entities;
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

   -------------------
   -- Type_Integral --
   -------------------

   function Type_Integral
     (Node : Node_Id)
      return Ack.Types.Type_Entity
   is
      use type Ack.Types.Type_Entity;
   begin
      if Local_Integral_Type = null then
         declare
            Aqua_Entity     : constant Ack.Classes.Class_Entity :=
                                Ack.Classes.Get_Top_Level_Class ("aqua");
            Integral_Name   : constant String := "integral";
            Integral_Entity : constant Ack.Classes.Class_Entity :=
                                (if Aqua_Entity.Contains (Integral_Name)
                                 then Ack.Classes.Class_Entity
                                   (Aqua_Entity.Get (Integral_Name))
                                   else Load_Class
                                   (Get_Program (Aqua_Entity.Declaration_Node),
                                    Aqua_Entity, Get_Name_Id (Integral_Name)));
         begin
            Local_Integral_Type :=
              Ack.Types.New_Class_Type (Node, Integral_Entity, False);
         end;
      end if;

      return Local_Integral_Type;
   end Type_Integral;

   -------------------
   -- Type_Iterable --
   -------------------

   function Type_Iterable
      return Ack.Types.Type_Entity
   is
      use type Ack.Types.Type_Entity;
   begin
      if Local_Iterable_Type = null then
         Local_Iterable_Type :=
           Ack.Types.New_Class_Type (No_Node, Class_Iterable, False);
      end if;

      return Local_Iterable_Type;
   end Type_Iterable;

end Ack.Semantic;
