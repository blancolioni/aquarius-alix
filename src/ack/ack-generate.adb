with Tagatha.Units;
with Tagatha.Units.Listing;

with Aquarius.Command_Line;
with Aquarius.Config_Paths;

with Ack.Classes;
with Ack.Features;
with Ack.Types;

with Ada.Text_IO;
with Ack.IO;

package body Ack.Generate is

   String_Label_Index : Natural := 0;

   function Next_String_Label
     (Base_Name : String)
      return String;

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
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Value_Type : not null access constant Root_Entity_Type'Class;
      Node       : Node_Id);

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
         Unit.Push (0);
         Unit.Native_Operation ("trap 15");
         Unit.End_Routine;
      end if;

      Unit.Begin_Routine
        (Entity.Link_Name & "$init",
         Argument_Words => 0,
         Frame_Words    => 0,
         Result_Words   => 1,
         Global         => True);

      Unit.End_Routine;

      if not Entity.Expanded then
         Entity.Generate_Virtual_Table (Unit);
      end if;

      if not Entity.Deferred and then not Entity.Expanded then
         Entity.Generate_Object_Allocator (Unit);
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
               Generate_Set_Value
                 (Unit, Get_Type (Expression (Instruction)),
                  Variable (Instruction));
            when N_Creation_Instruction =>
               Generate_Creation (Unit, Instruction);
            when N_Conditional =>
               Generate_Conditional (Unit, Instruction);
            when N_Loop =>
               Generate_Loop (Unit, Instruction);
            when N_Precursor =>
               Generate_Precursor (Unit, Instruction);
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

      Unit.Push_Register ("agg");

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
      end if;

      Unit.Call
        (Creation_Type.Link_Name & "$create");
      Unit.Push_Register ("r0");
      Unit.Pop_Register ("agg");

      if Explicit_Call_Node in Real_Node_Id then
         Unit.Push_Register ("r0");
         Get_Entity (Explicit_Call_Node).Push_Entity
           (Have_Current => True,
            Context      => Creation_Type.Class_Context,
            Unit         => Unit);
      end if;

      Unit.Push_Register ("agg");
      Get_Entity (Creation).Pop_Entity
        (Get_Context (Creation).Class_Context, Creation_Type, Unit);

      Unit.Pop_Register ("agg");

   exception
      when others =>
         Ada.Text_IO.Put_Line ("no entity in this tree:");
         Ack.IO.Put_Line (Creation);
         raise;

   end Generate_Creation;

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
                     declare
                        Label : constant String :=
                                  Next_String_Label (Unit.Unit_Name);
                        Text  : constant String :=
                                  To_String (Get_Name (Value));
                     begin
                        Unit.Segment (Tagatha.Read_Only);
                        Unit.Label (Label);
                        Unit.Data (Text'Length);

                        for Ch of Text loop
                           Unit.Data (Character'Pos (Ch));
                        end loop;

                        Unit.Segment (Tagatha.Executable);
                        Unit.Push_Label (Label);
                        Unit.Call ("string$create");
                        Unit.Push_Register ("r0");
                        Unit.Call ("string__create_from_string_literal");
                        Unit.Pop_Register ("r0");
                        Unit.Drop;
                        Unit.Push_Register ("r0");
                     end;
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

      Has_Iterator        : constant Boolean := Iteration_Node /= No_Node;
      It_Expression       : Node_Id := No_Node;
      Iterable_Type       : Ack.Types.Type_Entity;
      Iterator_Type       : Ack.Types.Type_Entity;
      New_Cursor_Feature  : Ack.Features.Feature_Entity;
      After_Feature       : Ack.Features.Feature_Entity;
      Next_Feature        : Ack.Features.Feature_Entity;
      --  Iterator_Entity     : Entity_Type;
   begin

      if Has_Iterator then
         It_Expression := Expression (Iteration_Node);
         Iterable_Type := Ack.Types.Type_Entity (Get_Type (It_Expression));
         New_Cursor_Feature :=
           Iterable_Type.Feature (Get_Name_Id ("new_cursor"));
         Iterator_Type :=
           Ack.Types.Type_Entity
             (New_Cursor_Feature.Get_Type);
         After_Feature :=
           Iterator_Type.Feature (Get_Name_Id ("after"));
         Next_Feature :=
           Iterator_Type.Feature (Get_Name_Id ("next"));
         --  Iterator_Entity := Get_Entity (Iteration_Node);

      end if;

      if Initialization_Node /= No_Node then
         Generate_Compound (Unit, Compound (Initialization_Node));
      end if;

      if Has_Iterator then
         Generate_Expression (Unit, It_Expression);
         New_Cursor_Feature.Push_Entity
           (Have_Current => True,
            Context      => Iterable_Type.Class_Context,
            Unit         => Unit);
      end if;

      Unit.Label (Top_Label);

      if Has_Iterator then
         Unit.Pop_Register ("op");
         Unit.Push_Register ("op");
         Unit.Push_Register ("op");
         After_Feature.Push_Entity
           (Have_Current => True,
            Context      => Iterator_Type.Class_Context,
            Unit         => Unit);
         Unit.Jump (Out_Label, Tagatha.C_Not_Equal);
      end if;

      if Exit_Condition_Node /= No_Node then
         Generate_Expression (Unit, Expression (Exit_Condition_Node));
         Unit.Operate (Tagatha.Op_Test);
         Unit.Jump (Out_Label, Tagatha.C_Not_Equal);
      end if;

      Generate_Compound (Unit, Compound (Loop_Body_Node));

      if Has_Iterator then
         Unit.Pop_Register ("r0");
         Unit.Push_Register ("r0");
         Unit.Push_Register ("r0");
         Next_Feature.Push_Entity
           (Have_Current => True,
            Context      => Iterator_Type.Class_Context,
            Unit         => Unit);
      end if;

      Unit.Jump (Top_Label, Tagatha.C_Always);

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
         if Right /= No_Node then
            Generate_Expression (Unit, Right);
         end if;
         Generate_Expression (Unit, Left);

         Get_Entity (Operator_Node).Push_Entity
           (Have_Current => True,
            Context      => Get_Type (Left).Class_Context,
            Unit         => Unit);

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

      Previous_Entity : Ack.Constant_Entity_Type := null;
      Previous_Context : Ack.Classes.Constant_Class_Entity := null;

      procedure Apply_Arguments
        (Actuals_List   : List_Id);

      procedure Process
        (Element : Node_Id;
         Last    : Boolean);

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
        (Element : Node_Id;
         Last    : Boolean)
      is
         Entity : constant Entity_Type := Get_Entity (Element);
      begin
         Entity.Push_Entity
           (Have_Current => Element /= First_Element,
            Context      => Get_Context (Element),
            Unit         => Unit);

         if not Last then
            Previous_Entity := Constant_Entity_Type (Entity);
            Previous_Context :=
              Ack.Classes.Constant_Class_Entity
                (Get_Context (Element).Class_Context);
         end if;

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
            Check_Stack      : constant Boolean :=
                                 Stack_Check_Enabled
                                     and then Actual_List /= No_List;
         begin
            if Element = Last_Element
              or else Actual_List /= No_List
            then

               if Check_Stack then
                  Unit.Push_Register ("sp");
               end if;

               if Actual_List /= No_List then
                  Apply_Arguments (Actual_List);
               end if;

               for Item of Pending loop
                  Process (Item, Item = Last_Element);
               end loop;

               if Check_Stack then
                  declare
                     use Ack.Features;
                     Label : constant Positive := Unit.Next_Label;
                     Last  : constant Constant_Entity_Type :=
                               Constant_Entity_Type
                                 (Get_Entity
                                    (Pending.Last_Element));
                     Has_Result      : constant Boolean :=
                                         Ack.Features.Is_Feature (Last)
                                           and then
                                               (Constant_Feature_Entity
                                                  (Last).Has_Result
                                                or else Constant_Feature_Entity
                                                  (Last)
                                                .Is_Property);
                  begin
                     if Has_Result then
                        Unit.Pop_Register ("r0");
                     end if;
                     Unit.Pop_Register ("r1");
                     Unit.Native_Operation ("cmp r1, sp");
                     Unit.Jump (Label, Tagatha.C_Equal);
                     Unit.Push (0);
                     Unit.Indirect_Call;
                     Unit.Label (Label);
                     if Has_Result then
                        Unit.Push_Register ("r0");
                     else
                        Unit.Native_Operation ("nop");
                     end if;
                  end;
               end if;

               Pending.Clear;
            end if;
         end;
      end loop;

      declare
         use Ack.Features, Ack.Classes;
         Last_Entity : constant Constant_Entity_Type :=
                         Constant_Entity_Type
                           (Get_Entity (Last_Element));
         Current_Context : constant Constant_Class_Entity :=
                             Constant_Class_Entity
                               (Get_Context (Last_Element).Class_Context);
         Expanded        : constant Boolean :=
                             Previous_Context /= null
                                 and then Current_Context /= null
                                     and then Current_Context.Expanded;
         Has_Result      : constant Boolean :=
                             Ack.Features.Is_Feature (Last_Entity)
                               and then
                                 (Constant_Feature_Entity
                                    (Last_Entity).Has_Result
                                  or else Constant_Feature_Entity (Last_Entity)
                                  .Is_Property);
      begin
         if Expanded and then not Has_Result then
            Previous_Entity.Pop_Entity
              (Get_Context (Precursor).Class_Context, Current_Context, Unit);
         end if;
      end;

   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Get_Program (Precursor).Show_Location
            & ": generate precursor failed");
         raise;

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
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Value_Type : not null access constant Root_Entity_Type'Class;
      Node       : Node_Id)
   is
      Entity : constant Entity_Type := Get_Entity (Node);
   begin
      if Value_Type.Standard_Name = "none" then
         Entity.Pop_Entity
           (Get_Context (Node).Class_Context,
            Entity.Get_Type.Class_Context, Unit);
      else
         Entity.Pop_Entity
           (Get_Context (Node).Class_Context,
            Value_Type, Unit);
      end if;

   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Get_Program (Node).Show_Location
            & ": pop entity " & Entity.Qualified_Name & " failed");
         raise;
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
                       Tuple_Type.Link_Name
                       & "__make_tuple"
                       & Arity_Image (2 .. Arity_Image'Last);
   begin
      Unit.Push_Register ("agg");
      Unit.Call
        (Tuple_Type.Link_Name & "$create");
      Unit.Push_Register ("r0");
      Unit.Push_Register ("r0");
      Unit.Pop_Register ("agg");

      for Arg of reverse Actual_Nodes loop
         Generate_Expression (Unit, Arg);
      end loop;

      Unit.Push_Register ("agg");
      Unit.Pop_Register ("op");
      Unit.Push_Register ("op");
      Unit.Call (Make_Name);
      for I in 1 .. Actual_Nodes'Length + 1 loop
         Unit.Drop;
      end loop;
   end Generate_Tuple_Expression;

   -----------------------
   -- Next_String_Label --
   -----------------------

   function Next_String_Label
     (Base_Name : String)
      return String
   is
      S : constant String := Natural'Image (String_Label_Index);
   begin
      String_Label_Index := String_Label_Index + 1;
      return Base_Name & "$str_" & S (2 .. S'Last);
   end Next_String_Label;

end Ack.Generate;
