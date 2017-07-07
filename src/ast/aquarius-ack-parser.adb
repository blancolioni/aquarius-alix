with WL.String_Maps;

with Aquarius.Trees;

with Aquarius.Ack.Parser.Expressions;

package body Aquarius.Ack.Parser is

   type Import_Function is access
     function (From : Aquarius.Programs.Program_Tree) return Node_Id;

   package Import_Function_Maps is
     new WL.String_Maps (Import_Function);

   Instruction_Imports : Import_Function_Maps.Map;

   function Import_List
     (From         : Aquarius.Programs.Program_Tree;
      Child_Name   : String;
      Import_Child : not null access
        function (Child : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return List_Id;

   function Import_Optional_Child
     (Parent     : Aquarius.Programs.Program_Tree;
      Child_Name : String;
      Import     : not null access
        function (Child : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return Node_Id;

   function Import_Choice
     (Parent : Aquarius.Programs.Program_Tree;
      Left_Name, Right_Name : String;
      Left_Importer, Right_Importer : not null access
        function (From : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return Node_Id;

   function Import_Class_Declaration
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "class_declaration";

   function Import_Class_Header
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "class_header";

   function Import_Inherited
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "inherited";

   function Import_Inheritance
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Inheritance, From,
                 List =>
                    Import_List (From.Program_Child ("inherit_list"),
                                 "inherited",
                                 Import_Inherited'Access)))
   with Pre => From.Name = "inheritance";

   function Import_Features
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "features";

   function Import_Feature_Clause
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "feature_clause";

   function Import_Feature_Declaration_List
     (From : Aquarius.Programs.Program_Tree)
      return List_Id
     with Pre => From.Name = "feature_declaration_list";

   function Import_Feature_Declaration
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "feature_declaration";

   function Import_New_Feature
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "new_feature";

   function Import_Feature_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "feature_name";

   function Import_Declaration_Body
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "declaration_body";

--     function Import_Event_Clause
--       (From : Aquarius.Programs.Program_Tree)
--        return Node_Id
--       with Pre => From.Name = "event_clause";

   function Import_Formal_Arguments
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "formal_arguments";

   function Import_Entity_Declaration_List
     (From : Aquarius.Programs.Program_Tree)
      return List_Id
     with Pre => From.Name = "entity_declaration_list";

   function Import_Entity_Declaration_Group
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "entity_declaration_group";

   function Import_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "type";

   function Import_Type_Mark
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (Import_Type (From.Program_Child ("type")))
   with Pre => From.Name = "type_mark";

   function Import_Class_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "class_type";

   function Import_Anchored
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id is (No_Node)
   with Pre => From.Name = "anchored";

   function Import_Explicit_Value
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id is (No_Node)
   with Pre => From.Name = "explicit_value";

   function Import_Routine
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "routine";

   function Import_Deferred
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id is (No_Node)
   with Pre => From.Name = "deferred";

   function Import_Effective_Routine
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   with Pre => From.Name = "effective_routine";

   function Import_Instruction
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "instruction";

   function Import_Compound
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (New_Node (N_Compound, From,
                 List =>
                    Import_List
                      (From, "instruction", Import_Instruction'Access)))
     with Pre => From.Name = "compound";

   function Import_Assignment
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "assignment";

   function Import_Creation_Instruction
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id is (New_Node (N_Creation_Instruction, From))
     with Pre => From.Name = "creation_instruction";

   function Import_Conditional
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id is (New_Node (N_Conditional, From))
     with Pre => From.Name = "conditional";

   function Import_Variable
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "variable";

   function Import_Feature_Value
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (Import_Choice
       (From, "explicit_value", "routine",
        Import_Explicit_Value'Access, Import_Routine'Access))
   with Pre => From.Name = "feature_value";

   function Import_Feature_Body
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is (Import_Choice
       (From, "deferred", "effective_routine",
        Import_Deferred'Access, Import_Effective_Routine'Access))
   with Pre => From.Name = "feature_body";

   ------------
   -- Import --
   ------------

   function Import
     (Program : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return Import_Class_Declaration
        (Program.Program_Child ("class_declaration"));
   end Import;

   -----------------------
   -- Import_Assignment --
   -----------------------

   function Import_Assignment
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node
        (N_Assignment, From,
         Field_1 => Import_Variable (From.Program_Child ("variable")),
         Field_2 =>
           Aquarius.Ack.Parser.Expressions.Import_Expression
             (From.Program_Child ("expression")));
   end Import_Assignment;

   -------------------
   -- Import_Choice --
   -------------------

   function Import_Choice
     (Parent                        : Aquarius.Programs.Program_Tree;
      Left_Name, Right_Name         : String;
      Left_Importer, Right_Importer : not null access
        function (From : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return Node_Id
   is
      Choice : constant Aquarius.Programs.Program_Tree :=
                 Parent.Chosen_Tree;
   begin
      if Choice.Name = Left_Name then
         return Left_Importer (Choice);
      elsif Choice.Name = Right_Name then
         return Right_Importer (Choice);
      else
         raise Constraint_Error with
         Parent.Name & ": expected choice of "
           & Left_Name & " or " & Right_Name
           & " but found " & Choice.Name;
      end if;
   end Import_Choice;

   ------------------------------
   -- Import_Class_Declaration --
   ------------------------------

   function Import_Class_Declaration
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Header_Node : constant Node_Id :=
                      Import_Class_Header
                        (From.Program_Child ("class_header"));
      Notes_Node  : constant Node_Id := No_Node;
      Inheritance_Node : constant Node_Id :=
                           Import_Optional_Child
                             (From, "inheritance", Import_Inheritance'Access);
      Creators_Node : constant Node_Id := No_Node;
      Features_Node : constant Node_Id :=
                        Import_Optional_Child
                             (From, "features", Import_Features'Access);
      Invariant_Node : constant Node_Id := No_Node;
   begin
      return New_Node
        (N_Class_Declaration, From,
         Field_1 => Notes_Node,
         Field_2 => Header_Node,
         Field_3 => Inheritance_Node,
         Field_4 => Creators_Node,
         Field_5 => Features_Node,
         Field_6 => Invariant_Node);
   end Import_Class_Declaration;

   -------------------------
   -- Import_Class_Header --
   -------------------------

   function Import_Class_Header
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Class_Name_Node : constant Node_Id :=
                          Import_Class_Name
                            (From.Program_Child ("class_name"));
   begin
      Node_Table (Class_Name_Node).Defining := True;
      return New_Node
        (N_Class_Header, From,
         Field_2 => Class_Name_Node);
   end Import_Class_Header;

   -----------------------
   -- Import_Class_Name --
   -----------------------

   function Import_Class_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      List : constant List_Id := New_List;
      Ids  : constant Array_Of_Program_Trees :=
               From.Direct_Children (Skip_Separators => True);
   begin
      for Id of Ids loop
         declare
            Name : constant Name_Id :=
                     Get_Name_Id (Id.Text);
            Node : constant Node_Id :=
                     New_Node (N_Identifier, Id,
                               Name => Name);
         begin
            Append (List, Node);
         end;
      end loop;

      return New_Node
        (N_Class_Name, From,
         List => List);
   end Import_Class_Name;

   -----------------------
   -- Import_Class_Type --
   -----------------------

   function Import_Class_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node (N_Class_Type, From,
                       Field_2 =>
                         Import_Class_Name
                           (From.Program_Child ("class_name")));
   end Import_Class_Type;

   -----------------------------
   -- Import_Declaration_Body --
   -----------------------------

   function Import_Declaration_Body
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Formal_Arguments_Node : constant Node_Id :=
                                Import_Optional_Child
                                  (From, "formal_arguments",
                                   Import_Formal_Arguments'Access);
      Type_Mark_Node        : constant Node_Id :=
                                Import_Optional_Child
                                  (From, "type_mark",
                                   Import_Type_Mark'Access);
      Feature_Value_Node    : constant Node_Id :=
                                Import_Optional_Child
                                  (From, "feature_value",
                                   Import_Feature_Value'Access);
   begin
      return New_Node (N_Declaration_Body, From,
                       Field_1 => Formal_Arguments_Node,
                       Field_2 => Type_Mark_Node,
                       Field_3 => Feature_Value_Node);
   end Import_Declaration_Body;

   ------------------------------
   -- Import_Effective_Routine --
   ------------------------------

   function Import_Effective_Routine
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Once : constant Boolean :=
               From.Program_Child ("routine_mark").Chosen_Tree.Name = "once";
   begin
      return New_Node (N_Effective_Routine, From,
                       Once => Once,
                       Field_1 =>
                         Import_Compound
                           (From.Program_Child ("compound")));
   end Import_Effective_Routine;

   -------------------------------------
   -- Import_Entity_Declaration_Group --
   -------------------------------------

   function Import_Entity_Declaration_Group
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      List : constant List_Id := New_List;
      Ids  : constant Array_Of_Program_Trees :=
               From.Program_Child
                 ("identifier_list").Direct_Children ("identifier");
      Group_Type : constant Node_Id :=
                     Import_Type_Mark (From.Program_Child ("type_mark"));
   begin
      for Id of Ids loop
         declare
            Name : constant Name_Id :=
                     Get_Name_Id (Id.Text);
            Node : constant Node_Id :=
                     New_Node (N_Identifier, Id,
                               Name => Name);
         begin
            Append (List, Node);
         end;
      end loop;
      return New_Node (N_Entity_Declaration_Group, From,
                       List => List, Field_1 => Group_Type);
   end Import_Entity_Declaration_Group;

   ------------------------------------
   -- Import_Entity_Declaration_List --
   ------------------------------------

   function Import_Entity_Declaration_List
     (From : Aquarius.Programs.Program_Tree)
      return List_Id
   is
      use Aquarius.Programs;
      List : constant List_Id := New_List;
      Fs   : constant Array_Of_Program_Trees :=
               From.Direct_Children;
   begin
      for F of Fs loop
         declare
            Node : constant Node_Id :=
                     Import_Entity_Declaration_Group (F);
         begin
            if Node /= No_Node then
               Append (List, Node);
            end if;
         end;
      end loop;

      return List;
   end Import_Entity_Declaration_List;

   -------------------------
   -- Import_Event_Clause --
   -------------------------

--     function Import_Event_Clause
--       (From : Aquarius.Programs.Program_Tree)
--        return Node_Id
--     is
--     begin
--        return New_Node (N_Event_Clause, From);
--     end Import_Event_Clause;

   ---------------------------
   -- Import_Feature_Clause --
   ---------------------------

   function Import_Feature_Clause
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Declaration_List : constant List_Id :=
                           Import_Feature_Declaration_List
                             (From.Program_Child
                                ("feature_declaration_list"));
   begin
      return New_Node (N_Feature_Clause, From,
                       List => Declaration_List);
   end Import_Feature_Clause;

   --------------------------------
   -- Import_Feature_Declaration --
   --------------------------------

   function Import_Feature_Declaration
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      New_Feature_List : constant List_Id :=
                           Import_List
                             (From.Program_Child ("new_feature_list"),
                              "new_feature",
                              Import_New_Feature'Access);
   begin
      return New_Node (N_Feature_Declaration, From,
                       List => New_Feature_List,
                       Field_1 =>
                         Import_Declaration_Body
                           (From.Program_Child
                              ("declaration_body")));
   end Import_Feature_Declaration;

   -------------------------------------
   -- Import_Feature_Declaration_List --
   -------------------------------------

   function Import_Feature_Declaration_List
     (From : Aquarius.Programs.Program_Tree)
      return List_Id
   is
      use Aquarius.Programs;
      Decs : constant Array_Of_Program_Trees :=
               From.Direct_Children
                 ("feature_declaration");
      List : constant List_Id := New_List;
   begin
      for Dec of Decs loop
         declare
            Node : constant Node_Id :=
                     Import_Feature_Declaration (Dec);
         begin
            if Node in Real_Node_Id then
               Append (List, Node);
            end if;
         end;
      end loop;
      return List;
   end Import_Feature_Declaration_List;

   -------------------------
   -- Import_Feature_Name --
   -------------------------

   function Import_Feature_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node
        (Kind     => N_Feature_Name,
         From     => From,
         Name     =>
           Get_Name_Id
             (From.Program_Child ("identifier").Text));
   end Import_Feature_Name;

   ---------------------
   -- Import_Features --
   ---------------------

   function Import_Features
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      List : constant List_Id := New_List;
      Fs   : constant Array_Of_Program_Trees :=
             From.Direct_Children;
   begin
      for F of Fs loop
         Append (List, Import_Feature_Clause (F));
      end loop;

      return New_Node (N_Features, From,
                       List => List);
   end Import_Features;

   -----------------------------
   -- Import_Formal_Arguments --
   -----------------------------

   function Import_Formal_Arguments
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      List : constant List_Id :=
               Import_Entity_Declaration_List
                 (From.Program_Child ("entity_declaration_list"));
   begin
      return New_Node (N_Formal_Arguments, From, List => List);
   end Import_Formal_Arguments;

   ----------------------
   -- Import_Inherited --
   ----------------------

   function Import_Inherited
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node (N_Inherited, From);
   end Import_Inherited;

   ------------------------
   -- Import_Instruction --
   ------------------------

   function Import_Instruction
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      Choice : constant Aquarius.Programs.Program_Tree :=
                 From.Chosen_Tree;
      Choice_Name : constant String := Choice.Name;
      Choice_Node : Node_Id;
   begin
      if Instruction_Imports.Is_Empty then
         declare
            procedure Insert
              (Name : String;
               F    : Import_Function);

            ------------
            -- Insert --
            ------------

            procedure Insert
              (Name : String;
               F    : Import_Function)
            is
            begin
               Instruction_Imports.Insert (Name, F);
            end Insert;
         begin
            Insert ("assignment", Import_Assignment'Access);
            Insert ("creation_instruction",
                    Import_Creation_Instruction'Access);
            Insert ("conditional", Import_Conditional'Access);
         end;
      end if;

      if Instruction_Imports.Contains (Choice_Name) then
         Choice_Node := Instruction_Imports.Element (Choice_Name) (Choice);
      else
         raise Constraint_Error with
         From.Show_Location & ": " & From.Name
           & ": expected an instruction but found " & Choice_Name;
      end if;

      return Choice_Node;

   end Import_Instruction;

   -----------------
   -- Import_List --
   -----------------

   function Import_List
     (From         : Aquarius.Programs.Program_Tree;
      Child_Name   : String;
      Import_Child : not null access
        function (Child : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return List_Id
   is
      use Aquarius.Programs;
      List     : constant List_Id := New_List;
      Children : constant Array_Of_Program_Trees :=
                   From.Direct_Children (Child_Name);
   begin
      for Child of Children loop
         declare
            Node : constant Node_Id :=
                     Import_Child (Child);
         begin
            Append (List, Node);
         end;
      end loop;
      return List;
   end Import_List;

   ------------------------
   -- Import_New_Feature --
   ------------------------

   function Import_New_Feature
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Name_Node : constant Node_Id :=
                    Import_Feature_Name
                      (From.Program_Child ("feature_name"));
      Frozen_Tree : constant Program_Tree :=
                      From.Program_Child ("frozen");
   begin
      return New_Node
        (Kind     => N_New_Feature,
         From     => From,
         Frozen   => Frozen_Tree /= null and then Frozen_Tree.Is_Filled,
         Field_1  => Name_Node);
   end Import_New_Feature;

   ---------------------------
   -- Import_Optional_Child --
   ---------------------------

   function Import_Optional_Child
     (Parent     : Aquarius.Programs.Program_Tree;
      Child_Name : String;
      Import     : not null access
        function (Child : Aquarius.Programs.Program_Tree)
      return Node_Id)
      return Node_Id
   is
      use Aquarius.Programs, Aquarius.Trees;
      Children : constant Array_Of_Trees := Parent.Get_Named_Children;
   begin
      for Child of Children loop
         if Child.Name = Child_Name then
            return Import (Program_Tree (Child));
         end if;
      end loop;

      return No_Node;
   end Import_Optional_Child;

   --------------------
   -- Import_Routine --
   --------------------

   function Import_Routine
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node (N_Routine, From,
                       Field_3 =>
                         Import_Feature_Body
                           (From.Program_Child ("feature_body")));
   end Import_Routine;

   -----------------
   -- Import_Type --
   -----------------

   function Import_Type
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
      use Aquarius.Programs;
      Choice_Tree : constant Program_Tree := From.Chosen_Tree;
      Choice_Node : Node_Id;
   begin
      if Choice_Tree.Name = "class_type" then
         Choice_Node := Import_Class_Type (Choice_Tree);
      elsif Choice_Tree.Name = "anchored" then
         Choice_Node := Import_Anchored (Choice_Tree);
      else
         raise Constraint_Error with
           "invalid type choice: " & Choice_Tree.Name;
      end if;

      return Choice_Node;

   end Import_Type;

   ---------------------
   -- Import_Variable --
   ---------------------

   function Import_Variable
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
   is
   begin
      return New_Node (N_Variable, From,
                       Name =>
                         Get_Name_Id
                           (From.Program_Child ("identifier").Text));
   end Import_Variable;

end Aquarius.Ack.Parser;
