with Tagatha.Operands;
with Tagatha.Units;
with Tagatha.Units.Listing;

with Aquarius.Command_Line;
with Aquarius.Config_Paths;

with Ack.Classes;
with Ack.Features;
with Ack.Types;

with Ack.Generate.Primitives;

with Ada.Text_IO;
with Ack.IO;

package body Ack.Generate is

   Report_Allocation : constant Boolean := False;

   procedure Generate_Allocator
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Class : not null access Ack.Classes.Class_Entity_Record'Class);

   procedure Generate_Default_Create
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Class : not null access Ack.Classes.Class_Entity_Record'Class);

   procedure Generate_Feature
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Class   : not null access constant
        Ack.Classes.Class_Entity_Record'Class;
      Feature : not null access constant
        Ack.Features.Feature_Entity_Record'Class);

   procedure Generate_Creation
     (Unit     : in out Tagatha.Units.Tagatha_Unit;
      Creation : Node_Id);

   procedure Generate_Conditional
     (Unit        : in out Tagatha.Units.Tagatha_Unit;
      Conditional : Node_Id);

   procedure Generate_Loop
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Loop_Node : Node_Id);

   procedure Generate_Operator_Expression
     (Unit          : in out Tagatha.Units.Tagatha_Unit;
      Operator_Node : Node_Id);

   procedure Generate_Tuple_Expression
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Expression : Node_Id);

   procedure Generate_Precursor
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Precursor : Node_Id);

   procedure Generate_Check
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Check : Node_Id)
   is null;

   procedure Generate_Retry
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Retry : Node_Id);

   procedure Generate_Set_Value
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Node    : Node_Id);

   procedure Generate_Get_Property
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Node    : Node_Id)
     with Pre => Kind (Node) = N_Get_Property;

   ------------------------
   -- Generate_Allocator --
   ------------------------

   procedure Generate_Allocator
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Class : not null access Ack.Classes.Class_Entity_Record'Class)
   is

      use type Ack.Classes.Class_Entity;

      Current_Property : Name_Id := No_Name;

      procedure Set_Value
        (Feature : not null access constant
           Ack.Features.Feature_Entity_Record'Class);

      procedure Generate_Local_Allocator
        (Ancestor_Class : not null access
           constant Ack.Classes.Class_Entity_Record'Class);

      ------------------------------
      -- Generate_Local_Allocator --
      ------------------------------

      procedure Generate_Local_Allocator
        (Ancestor_Class : not null access
           constant Ack.Classes.Class_Entity_Record'Class)
      is
      begin
         Unit.Push_Operand
           (Tagatha.Operands.External_Operand ("map", True),
            Tagatha.Default_Size);
         Unit.Pop_Register ("op");
         Unit.Native_Operation
           ("allocate",
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "op");
         Unit.Push_Register ("op");
         Unit.Pop_Register ("pv");
         Unit.Push_Register ("agg");
         Unit.Pop_Register ("op");
         Unit.Native_Operation
           ("set_property " & Ancestor_Class.Link_Name);
      end Generate_Local_Allocator;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value
        (Feature : not null access constant
           Ack.Features.Feature_Entity_Record'Class)
      is
      begin
         Feature.Set_Default_Value (Current_Property, Unit);
      end Set_Value;

   begin
      Unit.Begin_Routine
        (Class.Link_Name & "$allocate",
         Argument_Words => 0,
         Frame_Words    => 0,
         Result_Words   => 1,
         Global         => True);

      if Report_Allocation then
         Unit.Push_Text ("allocating: " & Class.Full_Name);
         Unit.Push_Operand
           (Tagatha.Operands.External_Operand ("io", True),
            Tagatha.Default_Size);
         Unit.Pop_Register ("op");
         Unit.Native_Operation
           ("get_property put_line,1",
            Input_Stack_Words  => 1,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");
      end if;

      Unit.Push_Operand
        (Tagatha.Operands.External_Operand ("map", True),
         Tagatha.Default_Size);
      Unit.Pop_Register ("op");
      Unit.Native_Operation
        ("allocate",
         Input_Stack_Words  => 0,
         Output_Stack_Words => 0,
         Changed_Registers  => "op");
      Unit.Push_Register ("op");
      Unit.Push_Register ("op");
      Unit.Pop_Register ("agg");

      Class.Scan_Ancestors (Proper_Ancestors => False,
                            Process          =>
                              Generate_Local_Allocator'Access);
      Class.Scan_Features (Set_Value'Access);

      if Report_Allocation then
         Unit.Push_Text ("finished allocating: " & Class.Full_Name);
         Unit.Push_Operand
           (Tagatha.Operands.External_Operand ("io", True),
            Tagatha.Default_Size);
         Unit.Pop_Register ("op");
         Unit.Native_Operation
           ("get_property put_line,1",
            Input_Stack_Words  => 1,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");
      end if;

      Unit.Pop_Result;
      Unit.End_Routine;

   end Generate_Allocator;

   --------------------------------
   -- Generate_Class_Declaration --
   --------------------------------

   procedure Generate_Class_Declaration
     (Node : Node_Id;
      Root : Boolean)
   is
      Unit : Tagatha.Units.Tagatha_Unit;
      Entity : constant Ack.Classes.Class_Entity :=
                 Ack.Classes.Get_Class_Entity (Node);
   begin
      Unit.Create_Unit
        (Entity.Base_File_Name,
         Get_Program (Node).Source_File_Name);

      Unit.Directive
        ("map ="
         & Natural'Image (16#3000_0001#));

      Unit.Directive
        ("array ="
         & Natural'Image (16#3000_0002#));

      Unit.Directive
        ("aqua ="
         & Natural'Image (16#3000_0003#));

      Unit.Directive
        ("io ="
         & Natural'Image (16#3000_0004#));

      if Root then
         Unit.Directive (".start " & Entity.Link_Name & "$main");
         Unit.Begin_Routine
           (Name           => Entity.Link_Name & "$main",
            Argument_Words => 0,
            Frame_Words    => 0,
            Result_Words   => 0,
            Global         => True);
         Unit.Call (Entity.Link_Name & "$create");
         Unit.Push_Register ("r0");
         Unit.Pop_Register ("op");
         Unit.Push_Register ("op");
         Unit.Push_Register ("op");
         Unit.Dereference;
         Push_Offset
           (Unit,
            Entity.Feature (Get_Name_Id ("make")).Virtual_Table_Offset);
         Unit.Operate (Tagatha.Op_Add);
         Unit.Dereference;
         Unit.Indirect_Call;
         Unit.Drop;
         Unit.End_Routine;
      end if;

      Unit.Begin_Routine
        (Entity.Link_Name & "$init",
         Argument_Words => 0,
         Frame_Words    => 0,
         Result_Words   => 1,
         Global         => True);

      Unit.End_Routine;

      Entity.Generate_Virtual_Table (Unit);

      if not Entity.Deferred then
         Entity.Generate_Object_Allocator (Unit);
      end if;

      if False then
         if not Entity.Deferred then
            Generate_Allocator (Unit, Entity);
            Generate_Default_Create (Unit, Entity);
         end if;
      end if;

      declare

         function Class_Defined_Feature
           (Feature : not null access constant
              Ack.Features.Feature_Entity_Record'Class)
            return Boolean
         is (not Feature.Deferred
             and then Feature.Effective_Class = Entity);

         procedure Generate_Feature
           (Feature : not null access constant
              Ack.Features.Feature_Entity_Record'Class);

         ----------------------
         -- Generate_Feature --
         ----------------------

         procedure Generate_Feature
           (Feature : not null access constant
              Ack.Features.Feature_Entity_Record'Class)
         is
         begin
            Generate_Feature (Unit, Entity, Feature);
         end Generate_Feature;

      begin

         Entity.Scan_Features
           (Class_Defined_Feature'Access,
            Generate_Feature'Access);
      end;

      Unit.Finish_Unit;

      if Aquarius.Command_Line.Ack_Write_Listing then
         Tagatha.Units.Listing.Write_Command_Listing (Unit);
         Tagatha.Units.Listing.Write_Transfer_Listing (Unit);
      end if;

      Unit.Write
        (Target_Name    => "pdp32",
         Directory_Path => Aquarius.Config_Paths.Config_File ("scratch"));

   end Generate_Class_Declaration;

   -----------------------
   -- Generate_Compound --
   -----------------------

   procedure Generate_Compound
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Node    : Node_Id)
   is
      procedure Generate_Instruction
        (Instruction : Node_Id);

      --------------------------
      -- Generate_Instruction --
      --------------------------

      procedure Generate_Instruction
        (Instruction : Node_Id)
      is
      begin
         Unit.Source_Position
           (Line   => Positive (Get_Program (Instruction).Location_Line),
            Column => Positive (Get_Program (Instruction).Location_Column));
         case N_Instruction (Kind (Instruction)) is
            when N_Assignment =>
               Generate_Expression (Unit, Expression (Instruction));
               Generate_Set_Value (Unit, Variable (Instruction));
            when N_Creation_Instruction =>
               Generate_Creation (Unit, Instruction);
            when N_Conditional =>
               Generate_Conditional (Unit, Instruction);
            when N_Loop =>
               Generate_Loop (Unit, Instruction);
            when N_Precursor =>
               Generate_Precursor (Unit, Instruction);
               Unit.Drop;
            when N_Check =>
               Generate_Check (Unit, Instruction);
            when N_Retry =>
               Generate_Retry (Unit, Instruction);
         end case;
      end Generate_Instruction;

   begin
      Scan (Instructions (Node), Generate_Instruction'Access);
   end Generate_Compound;

   --------------------------
   -- Generate_Conditional --
   --------------------------

   procedure Generate_Conditional
     (Unit        : in out Tagatha.Units.Tagatha_Unit;
      Conditional : Node_Id)
   is

      Out_Label  : constant Positive := Unit.Next_Label;
      Else_Label : Natural := 0;

      procedure Generate_Element (Element : Node_Id);

      ----------------------
      -- Generate_Element --
      ----------------------

      procedure Generate_Element (Element : Node_Id) is
         Condition : constant Node_Id := Field_1 (Element);
         Compound  : constant Node_Id := Field_2 (Element);
      begin
         if Else_Label /= 0 then
            Unit.Label (Else_Label);
            Else_Label := 0;
         end if;

         if Condition /= No_Node then
            Generate_Expression (Unit, Condition);
            if Implicit_Entity (Condition) then
               Unit.Pop_Register ("r0");
               Unit.Push_Register ("r0");
               Unit.Push_Register ("r0");
            end if;

            Else_Label := Unit.Next_Label;
            Unit.Operate (Tagatha.Op_Test, Tagatha.Default_Size);
            Unit.Jump (Else_Label, Tagatha.C_Equal);
         end if;
         Generate_Compound (Unit, Compound);
         if Condition /= No_Node
           and then Implicit_Entity (Condition)
         then
            Unit.Drop;
         end if;

         Unit.Jump (Out_Label);
      end Generate_Element;

   begin
      Scan (Node_Table.Element (Conditional).List,
            Generate_Element'Access);
      if Else_Label /= 0 then
         Unit.Label (Else_Label);
      end if;
      Unit.Label (Out_Label);

   end Generate_Conditional;

   -----------------------
   -- Generate_Creation --
   -----------------------

   procedure Generate_Creation
     (Unit     : in out Tagatha.Units.Tagatha_Unit;
      Creation : Node_Id)
   is
      Call_Node : constant Node_Id := Creation_Call (Creation);
      Explicit_Type_Node : constant Node_Id :=
                             Explicit_Creation_Type (Creation);
      Explicit_Call_Node : constant Node_Id :=
                             Explicit_Creation_Call (Call_Node);
      Creation_Type      : constant Entity_Type :=
                             (if Explicit_Type_Node in Real_Node_Id
                              then Get_Entity (Explicit_Type_Node)
                              else Get_Entity (Creation).Get_Type);
   begin

      Unit.Call
        (Creation_Type.Link_Name & "$create");
      Unit.Push_Register ("r0");
      Get_Entity (Creation).Pop_Entity (Unit);

      if Explicit_Call_Node in Real_Node_Id then
         declare
            Actual_List_Node : constant Node_Id :=
                                 Actual_List (Explicit_Call_Node);
            Actual_List      : constant List_Id :=
                                 (if Actual_List_Node /= No_Node
                                  then Node_Table.Element
                                    (Actual_List_Node).List
                                  else No_List);
            Actuals_Node_List : constant List_Of_Nodes.List :=
                                  (if Actual_List = No_List
                                   then List_Of_Nodes.Empty_List
                                   else List_Table.Element (Actual_List)
                                   .List);
         begin
            for Item of reverse Actuals_Node_List loop
               Generate_Expression (Unit, Item);
            end loop;
         end;

         Get_Entity (Creation).Push_Entity
           (Have_Current => False,
            Context      => Get_Context (Creation),
            Unit         => Unit);

         Get_Entity (Explicit_Call_Node).Push_Entity
           (Have_Current => True,
            Context      => Get_Context (Creation),
            Unit         => Unit);

         Unit.Drop;
      end if;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("no entity in this tree:");
         Ack.IO.Put_Line (Creation);
         raise;

   end Generate_Creation;

   -----------------------------
   -- Generate_Default_Create --
   -----------------------------

   procedure Generate_Default_Create
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Class : not null access Ack.Classes.Class_Entity_Record'Class)
   is
      procedure Clear_Feature_Value
        (Feature : not null access constant
           Ack.Features.Feature_Entity_Record'Class);

      -------------------------
      -- Clear_Feature_Value --
      -------------------------

      procedure Clear_Feature_Value
        (Feature : not null access constant
           Ack.Features.Feature_Entity_Record'Class)
      is

         procedure Clear_Value_In_Class
           (Class : not null access constant
              Ack.Classes.Class_Entity_Record'Class);

         --------------------------
         -- Clear_Value_In_Class --
         --------------------------

         procedure Clear_Value_In_Class
           (Class : not null access constant
              Ack.Classes.Class_Entity_Record'Class)
         is
         begin
            Unit.Push_Register ("r0");
            Unit.Pop_Register ("op");
            Unit.Native_Operation
              ("get_property " & Class.Link_Name & ",0",
               Input_Stack_Words  => 0,
               Output_Stack_Words => 0,
               Changed_Registers  => "pv");

            Unit.Push_Register ("pv");
            Unit.Pop_Register ("op");

            Unit.Push (0);
            Unit.Pop_Register ("pv");
            Unit.Native_Operation
              ("set_property " & Feature.Standard_Name);
         end Clear_Value_In_Class;

      begin
         Feature.Scan_Original_Classes (Clear_Value_In_Class'Access);
      end Clear_Feature_Value;

   begin
      Unit.Begin_Routine
        (Class.Link_Name & "$default_create",
         Argument_Words => 1,
         Frame_Words    => 0,
         Result_Words   => 0,
         Global         => True);

      Unit.Push_Argument (1);
      Unit.Pop_Register ("r0");

      Class.Scan_Features (Clear_Feature_Value'Access);

      Unit.End_Routine;

   end Generate_Default_Create;

   -------------------------
   -- Generate_Expression --
   -------------------------

   procedure Generate_Expression
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Expression : Node_Id)
   is
   begin
      Unit.Source_Position
        (Line   => Positive (Get_Program (Expression).Location_Line),
         Column => Positive (Get_Program (Expression).Location_Column));
      case N_Expression_Node (Kind (Expression)) is
         when N_Operator =>
            Generate_Operator_Expression (Unit, Expression);
         when N_Precursor =>
            Generate_Precursor (Unit, Expression);
         when N_Attachment_Test =>
            Generate_Expression (Unit, Field_1 (Expression));
         when N_Old =>
            Generate_Expression (Unit, Field_1 (Expression));
         when N_Tuple =>
            Generate_Tuple_Expression (Unit, Expression);
         when N_Constant =>
            declare
               Value : constant Node_Id := Constant_Value (Expression);
            begin
               case N_Constant_Value (Kind (Value)) is
                  when N_String_Constant =>
                     Unit.Push_Text
                       (To_String (Get_Name (Value)));
                  when N_Integer_Constant =>
                     Unit.Push
                       (Tagatha.Tagatha_Integer'Value
                          (To_String
                               (Get_Name (Value))));
                  when N_Boolean_Constant =>
                     Unit.Push
                       (if Boolean_Value (Value) then 1 else 0);
               end case;
            end;
      end case;

      if Get_Property (Expression) /= No_Node then
         Generate_Get_Property
           (Unit, Get_Property (Expression));
      end if;

   end Generate_Expression;

   ----------------------
   -- Generate_Feature --
   ----------------------

   procedure Generate_Feature
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Class   : not null access constant
        Ack.Classes.Class_Entity_Record'Class;
      Feature : not null access constant
        Ack.Features.Feature_Entity_Record'Class)
   is
   begin
      Feature.Generate_Routine (Class, Unit);
   end Generate_Feature;

   ---------------------------
   -- Generate_Get_Property --
   ---------------------------

   procedure Generate_Get_Property
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Node    : Node_Id)
   is
   begin
      Unit.Pop_Register ("op");
      Unit.Native_Operation
        ("get_property " & To_Standard_String (Get_Name (Node)) & ", 0",
         Input_Stack_Words  => 0,
         Output_Stack_Words => 0,
         Changed_Registers  => "pv");
      Unit.Push_Register ("pv");
   end Generate_Get_Property;

   -------------------
   -- Generate_Loop --
   -------------------

   procedure Generate_Loop
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Loop_Node : Node_Id)
   is
      Iteration_Node      : constant Node_Id := Loop_Iteration (Loop_Node);
      Initialization_Node : constant Node_Id :=
                              Loop_Initialization (Loop_Node);
      Exit_Condition_Node : constant Node_Id :=
                              Loop_Exit_Condition (Loop_Node);
      Loop_Body_Node      : constant Node_Id := Loop_Body (Loop_Node);
      Top_Label           : constant Positive := Unit.Next_Label;
      Out_Label           : constant Positive := Unit.Next_Label;
   begin

      if Initialization_Node /= No_Node then
         Generate_Compound (Unit, Compound (Initialization_Node));
      end if;

      if Iteration_Node /= No_Node then
         declare
            Expression_Node : constant Node_Id := Expression (Iteration_Node);
         begin
            Generate_Expression (Unit, Expression_Node);
            Unit.Pop_Register ("r0");
            Unit.Push_Register ("r0");  --  'current' argument to New_Cursor
            Unit.Push_Register ("r0");  --  sent to op
            Unit.Pop_Register ("op");
            Unit.Native_Operation
              ("get_property aqua__iterable, 0",
               Input_Stack_Words  => 0,
               Output_Stack_Words => 0,
               Changed_Registers  => "pv");
            Unit.Push_Register ("pv");
            Unit.Pop_Register ("op");
            Unit.Native_Operation
              ("get_property new_cursor, 0",
               Input_Stack_Words  => 0,
               Output_Stack_Words => 0,
               Changed_Registers  => "pv");
            Unit.Push_Register ("pv");
            Unit.Indirect_Call;
            Unit.Drop;
         end;
      end if;

      if Iteration_Node /= No_Node then
         Unit.Push_Register ("r0");
         Unit.Pop_Register ("op");
         Unit.Push_Register ("op");
         Unit.Push_Register ("op");
         Unit.Native_Operation
           ("get_property aqua__iteration_cursor, 0",
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");
         Unit.Push_Register ("pv");
         Unit.Pop_Register ("op");
         Unit.Native_Operation
           ("get_property after, 0",
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");
         Unit.Push_Register ("pv");
         Unit.Indirect_Call;
         Unit.Drop;
         Unit.Push_Register ("r0");
         Unit.Operate (Tagatha.Op_Test);
         Unit.Jump (Out_Label, Tagatha.C_Not_Equal);
      end if;

      Unit.Label (Top_Label);

      if Exit_Condition_Node /= No_Node then
         Generate_Expression (Unit, Expression (Exit_Condition_Node));
         Unit.Operate (Tagatha.Op_Test);
         Unit.Jump (Out_Label, Tagatha.C_Not_Equal);
      end if;

      Generate_Compound (Unit, Compound (Loop_Body_Node));

      if Iteration_Node = No_Node then
         Unit.Jump (Top_Label, Tagatha.C_Always);
      else
         Unit.Pop_Register ("op");
         Unit.Push_Register ("op");
         Unit.Push_Register ("op");
         Unit.Native_Operation
           ("get_property aqua__iteration_cursor, 0",
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");
         Unit.Push_Register ("pv");
         Unit.Pop_Register ("op");
         Unit.Native_Operation
           ("get_property next, 0",
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");
         Unit.Push_Register ("pv");
         Unit.Indirect_Call;
         Unit.Drop;
         Unit.Pop_Register ("op");
         Unit.Push_Register ("op");
         Unit.Push_Register ("op");
         Unit.Native_Operation
           ("get_property aqua__iteration_cursor, 0",
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");
         Unit.Push_Register ("pv");
         Unit.Pop_Register ("op");
         Unit.Native_Operation
           ("get_property after, 0",
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");
         Unit.Push_Register ("pv");
         Unit.Indirect_Call;
         Unit.Drop;
         Unit.Push_Register ("r0");
         Unit.Operate (Tagatha.Op_Test);
         Unit.Jump (Top_Label, Tagatha.C_Equal);
      end if;

      Unit.Label (Out_Label);

      if Iteration_Node /= No_Node then
         Unit.Drop;
      end if;

   end Generate_Loop;

   ----------------------------------
   -- Generate_Operator_Expression --
   ----------------------------------

   procedure Generate_Operator_Expression
     (Unit          : in out Tagatha.Units.Tagatha_Unit;
      Operator_Node : Node_Id)
   is
      use type Ack.Types.Type_Entity;
      Operator  : constant Name_Id := Get_Name (Operator_Node);
      Left      : constant Node_Id := Field_1 (Operator_Node);
      Right     : constant Node_Id := Field_2 (Operator_Node);
   begin

      if Operator = Get_Name_Id ("andthen") then
         declare
            Maybe : constant Positive := Unit.Next_Label;
            Leave : constant Positive := Unit.Next_Label;
         begin
            Generate_Expression (Unit, Left);
            Unit.Operate (Tagatha.Op_Test);
            Unit.Jump (Maybe, Tagatha.C_Not_Equal);
            Unit.Push (0);
            Unit.Jump (Leave);
            Unit.Label (Maybe);
            Generate_Expression (Unit, Right);
            Unit.Label (Leave);
         end;
      elsif Operator = Get_Name_Id ("orelse") then
         declare
            Maybe : constant Positive := Unit.Next_Label;
            Leave : constant Positive := Unit.Next_Label;
         begin
            Generate_Expression (Unit, Left);
            Unit.Operate (Tagatha.Op_Test);
            Unit.Jump (Maybe, Tagatha.C_Equal);
            Unit.Push (1);
            Unit.Jump (Leave);
            Unit.Label (Maybe);
            Generate_Expression (Unit, Right);
            Unit.Label (Leave);
         end;
      elsif Operator = Get_Name_Id ("implies") then
         Generate_Expression (Unit, Left);
         Unit.Operate (Tagatha.Op_Not);
         Generate_Expression (Unit, Right);
         Unit.Operate (Tagatha.Op_Or);
      else
         Generate_Expression (Unit, Left);
         if Right /= No_Node then
            Generate_Expression (Unit, Right);
         end if;

         if not Ack.Generate.Primitives.Generate_Operator
           (Unit      => Unit,
            Operator  => Operator,
            Left_Type => Ack.Types.Type_Entity (Get_Type (Left)))
         then
            raise Constraint_Error with
            Get_Program (Operator_Node).Show_Location
              & "custom operators ("
              & To_String (Operator)
              & ") not implemented yet";
         end if;
      end if;
   end Generate_Operator_Expression;

   ------------------------
   -- Generate_Precursor --
   ------------------------

   procedure Generate_Precursor
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Precursor : Node_Id)
   is
      List   : constant List_Id :=
                 Node_Table (Precursor).List;

      Pending : List_Of_Nodes.List;

      First_Element : constant Node_Id :=
                        List_Table (List).List.First_Element;
      Last_Element  : constant Node_Id :=
                        List_Table (List).List.Last_Element;

      procedure Apply_Arguments
        (Actuals_List   : List_Id);

      procedure Process
        (Element      : Node_Id);

      ---------------------
      -- Apply_Arguments --
      ---------------------

      procedure Apply_Arguments
        (Actuals_List   : List_Id)
      is
         Actuals_Node_List : constant List_Of_Nodes.List :=
                               List_Table.Element
                                 (Actuals_List).List;
      begin
         for Item of reverse Actuals_Node_List loop
            Generate_Expression (Unit, Item);
         end loop;
      end Apply_Arguments;

      -------------
      -- Process --
      -------------

      procedure Process
        (Element      : Node_Id)
      is
         Entity : constant Entity_Type := Get_Entity (Element);
      begin
         Entity.Push_Entity
           (Have_Current => Element /= First_Element,
            Context      => Get_Context (Element),
            Unit         => Unit);
      end Process;

   begin

      for Element of List_Table (List).List loop
         Pending.Append (Element);

         declare
            Actual_List_Node : constant Node_Id :=
                                 Actual_List (Element);
            Actual_List      : constant List_Id :=
                                 (if Actual_List_Node /= No_Node
                                  then Node_Table.Element
                                    (Actual_List_Node).List
                                  else No_List);
         begin
            if Element = Last_Element
              or else Actual_List /= No_List
            then
               if Actual_List /= No_List then
                  Apply_Arguments (Actual_List);
               end if;

               for Item of Pending loop
                  Process (Item);
               end loop;

               Pending.Clear;
            end if;
         end;
      end loop;

   end Generate_Precursor;

   --------------------
   -- Generate_Retry --
   --------------------

   procedure Generate_Retry
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Retry : Node_Id)
   is
      Target : constant String := Unit.Get_Property ("retry_label");
   begin
      if Target /= "" then
         Unit.Jump (Target);
      else
         raise Constraint_Error with
         Get_Program (Retry).Show_Location
           & ": expected to be in a rescue context";
      end if;
   end Generate_Retry;

   ------------------------
   -- Generate_Set_Value --
   ------------------------

   procedure Generate_Set_Value
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Node    : Node_Id)
   is
      Entity : constant Entity_Type := Get_Entity (Node);
   begin
      Entity.Pop_Entity (Unit);
   end Generate_Set_Value;

   -------------------------------
   -- Generate_Tuple_Expression --
   -------------------------------

   procedure Generate_Tuple_Expression
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Expression : Node_Id)
   is
      Tuple_Type   : constant Entity_Type := Get_Type (Expression);
      Actual_Nodes : constant Array_Of_Nodes :=
                       To_Array
                         (Tuple_Expression_List (Expression));
      Arity_Image : constant String := Natural'Image (Actual_Nodes'Length);
      Make_Name    : constant String :=
                       "make_tuple" & Arity_Image (2 .. Arity_Image'Last);
   begin
      Unit.Push_Register ("agg");
      Unit.Call
        (Tuple_Type.Link_Name & "$allocate");
      Unit.Push_Register ("r0");
      Unit.Pop_Register ("agg");

      for Arg of reverse Actual_Nodes loop
         Generate_Expression (Unit, Arg);
      end loop;

      Unit.Push_Register ("agg");
      Unit.Pop_Register ("op");
      Unit.Push_Register ("op");
      Unit.Native_Operation
        ("get_property " & Tuple_Type.Link_Name & ",0",
         Input_Stack_Words  => 0,
         Output_Stack_Words => 0,
         Changed_Registers  => "pv");
      Unit.Push_Register ("pv");
      Unit.Pop_Register ("op");
      Unit.Native_Operation
        ("get_property " & Make_Name & ",0",
         Input_Stack_Words  => 0,
         Output_Stack_Words => 0,
         Changed_Registers  => "pv");
      Unit.Push_Register ("pv");
      Unit.Indirect_Call;
      for I in 1 .. Actual_Nodes'Length + 1 loop
         Unit.Drop;
      end loop;
      Unit.Push_Register ("agg");
   end Generate_Tuple_Expression;

end Ack.Generate;
