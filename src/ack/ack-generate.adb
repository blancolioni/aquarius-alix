with Tagatha.Units;
with Tagatha.Units.Listing;

with Aquarius.Command_Line;
with Aquarius.Config_Paths;

with Ack.Classes;
with Ack.Features;
with Ack.Types;

with Ack.Generate.Intrinsics;

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
      Context  : not null access constant Root_Entity_Type'Class;
      Creation : Node_Id);

   procedure Generate_Conditional
     (Unit        : in out Tagatha.Units.Tagatha_Unit;
      Context     : not null access constant Root_Entity_Type'Class;
      Conditional : Node_Id);

   procedure Generate_Loop
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Context   : not null access constant Root_Entity_Type'Class;
      Loop_Node : Node_Id);

   procedure Generate_Operator_Expression
     (Unit          : in out Tagatha.Units.Tagatha_Unit;
      Context       : not null access constant Root_Entity_Type'Class;
      Operator_Node : Node_Id);

   procedure Generate_Tuple_Expression
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Context    : not null access constant Root_Entity_Type'Class;
      Expression : Node_Id);

   procedure Generate_Precursor
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Context   : not null access constant Root_Entity_Type'Class;
      Precursor : Node_Id);

   procedure Generate_Check
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Context : not null access constant Root_Entity_Type'Class;
      Check   : Node_Id)
   is null;

   procedure Generate_Retry
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Context : not null access constant Root_Entity_Type'Class;
      Retry   : Node_Id);

   procedure Generate_Set_Value
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Context    : not null access constant Root_Entity_Type'Class;
      Value_Type : not null access constant Root_Entity_Type'Class;
      Node       : Node_Id);

   procedure Generate_Assignment_Conversion
     (Unit               : in out Tagatha.Units.Tagatha_Unit;
      From_Type, To_Type : not null access constant
        Root_Entity_Type'Class);

   ------------------------------------
   -- Generate_Assignment_Conversion --
   ------------------------------------

   procedure Generate_Assignment_Conversion
     (Unit               : in out Tagatha.Units.Tagatha_Unit;
      From_Type, To_Type : not null access constant
        Root_Entity_Type'Class)
   is
      use type Ack.Classes.Class_Entity;
      From : constant Ack.Types.Constant_Type_Entity :=
               Ack.Types.Constant_Type_Entity (From_Type);
      To   : constant Ack.Types.Constant_Type_Entity :=
               Ack.Types.Constant_Type_Entity (To_Type);
   begin
      if From.Class /= To.Class
        and then not From.Class.Expanded
        and then From.Class.Qualified_Name /= "None"
      then
         declare
            Offset : constant Word_Offset :=
                       From.Class.Ancestor_Table_Offset (To.Class);
         begin
            if Offset > 0 then
               Unit.Duplicate;
               Unit.Dereference;
               Push_Offset (Unit, Offset);
               Unit.Operate (Tagatha.Op_Add);
               Unit.Dereference;
               Unit.Operate (Tagatha.Op_Add);
            end if;
         end;
      end if;
   end Generate_Assignment_Conversion;

   --------------------------------
   -- Generate_Class_Declaration --
   --------------------------------

   procedure Generate_Class_Declaration
     (Node : Node_Id;
      Root : Boolean)
   is
      Unit : Tagatha.Units.Tagatha_Unit;
      Class : constant Ack.Classes.Class_Entity :=
                Ack.Classes.Get_Class_Entity (Node);

   begin

      Unit.Create_Unit
        (Class.Base_File_Name,
         Get_Program (Node).Source_File_Name);

      if Root then
         Unit.Directive (".start " & Class.Link_Name & "$main");
         Unit.Source_Location
           (Line   => Positive (Get_Program (Node).Location_Line),
            Column => Positive (Get_Program (Node).Location_Column));

         Unit.Begin_Routine
           (Name           => Class.Link_Name & "$main",
            Argument_Words => 0,
            Result_Words   => 0,
            Global         => True);
         Unit.Call (Class.Link_Name & "$create", 0);

         Unit.Push_Return;
         Unit.Duplicate;
         Unit.Dereference;

         Push_Offset
           (Unit,
            Class.Feature (Get_Name_Id ("make")).Virtual_Table_Offset);

         Unit.Operate (Tagatha.Op_Add);
         Unit.Dereference;
         Unit.Indirect_Call (1);
         Unit.Drop;
         Unit.Push (0);
         Unit.Native_Operation ("trap 15");
         Unit.End_Routine;
      end if;

      declare

         function Class_Defined_Feature
           (Feature : not null access constant
              Ack.Features.Feature_Entity_Record'Class)
            return Boolean
         is (not Feature.Deferred
             and then Feature.Effective_Class = Class);

         procedure Check_Feature_Bindings
           (Feature : not null access constant
              Ack.Features.Feature_Entity_Record'Class);

         ----------------------------
         -- Check_Feature_Bindings --
         ----------------------------

         procedure Check_Feature_Bindings
           (Feature : not null access constant
              Ack.Features.Feature_Entity_Record'Class)
         is
            Note_Name : constant String :=
                          "aqua_action_binding_"
                          & Feature.Standard_Name;
         begin

            if Class.Has_Note (Note_Name) then
               declare
                  Parent_Name   : constant String :=
                                    Ada.Characters.Handling.To_Lower
                                      (Class.Get_Note_Item (Note_Name, 1));
                  Position_Name : constant String :=
                                    Ada.Characters.Handling.To_Lower
                                      (Class.Get_Note_Item (Note_Name, 2));
                  Child_Name    : constant String :=
                                    Ada.Characters.Handling.To_Lower
                                      (Class.Get_Note_Item (Note_Name, 3));
               begin
                  Unit.Segment (Tagatha.Executable);
                  Unit.Directive
                    (".bind_action "
                     & Class.Declaration_Context.Standard_Name
                     & " " & Feature.Link_Name
                     & " " & Position_Name
                     & " " & Parent_Name
                     & " " & Child_Name);
               end;
            end if;

         end Check_Feature_Bindings;

      begin

         Class.Scan_Features
           (Class_Defined_Feature'Access,
            Check_Feature_Bindings'Access);
      end;

      Unit.Begin_Routine
        (Class.Link_Name & "$init",
         Argument_Words => 0,
         Result_Words   => 1,
         Global         => True);

      Unit.End_Routine;

      if not Class.Expanded and then not Class.Deferred then
         Class.Generate_Virtual_Table (Unit);
      end if;

      if not Class.Deferred and then not Class.Expanded then
         Class.Generate_Object_Allocator (Unit);
      end if;

      declare

         function Class_Defined_Feature
           (Feature : not null access constant
              Ack.Features.Feature_Entity_Record'Class)
            return Boolean
         is (not Feature.Deferred
             and then Feature.Effective_Class = Class);

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
            Generate_Feature (Unit, Class, Feature);
         end Generate_Feature;

      begin

         Class.Scan_Features
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
      Context : not null access constant Root_Entity_Type'Class;
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
         Unit.Source_Location
           (Line   => Positive (Get_Program (Instruction).Location_Line),
            Column => Positive (Get_Program (Instruction).Location_Column));

         case N_Instruction (Kind (Instruction)) is
            when N_Assignment =>
               Generate_Expression (Unit, Context, Expression (Instruction));
               Generate_Set_Value
                 (Unit, Context, Get_Type (Expression (Instruction)),
                  Variable (Instruction));
            when N_Creation_Instruction =>
               Generate_Creation (Unit, Context, Instruction);
            when N_Conditional =>
               Generate_Conditional (Unit, Context, Instruction);
            when N_Loop =>
               Generate_Loop (Unit, Context, Instruction);
            when N_Precursor =>
               Generate_Precursor (Unit, Context, Instruction);
            when N_Check =>
               Generate_Check (Unit, Context, Instruction);
            when N_Retry =>
               Generate_Retry (Unit, Context, Instruction);
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
      Context     : not null access constant Root_Entity_Type'Class;
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
            Generate_Expression (Unit, Context, Condition);
            if Implicit_Entity (Condition) then
               Unit.Duplicate;
               Unit.Allocate_Local;
            end if;

            Else_Label := Unit.Next_Label;
            Unit.Operate (Tagatha.Op_Test);
            Unit.Jump (Else_Label, Tagatha.C_Equal);
         end if;
         Generate_Compound (Unit, Context, Compound);
         if Condition /= No_Node
           and then Implicit_Entity (Condition)
         then
            Unit.Drop;
            Unit.Deallocate_Local;
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
      Context  : not null access constant Root_Entity_Type'Class;
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
      Created_Context    : constant Constant_Entity_Type :=
                             Get_Context (Creation).Class_Context;
      Created_Entity     : constant Entity_Type :=
                             Get_Entity (Creation);
      Creation_Routine   : Entity_Type;

      Created_Offset : constant Tagatha.Local_Offset :=
                         Unit.Allocate_Local;
   begin

      Unit.Call
        (Creation_Type.Link_Name & "$create", 0);
      Unit.Push_Return;

      if Explicit_Call_Node not in Real_Node_Id then
         pragma Assert (Creation_Type.Has_Default_Creation_Routine);
         Creation_Routine :=
           Creation_Type.Default_Creation_Routine;
      else
         Creation_Routine :=
           Get_Entity (Explicit_Call_Node);

         declare
            Actual_List_Node  : constant Node_Id :=
                                  Actual_List (Explicit_Call_Node);
            Actual_List       : constant List_Id :=
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
               Generate_Expression (Unit, Context, Item);
            end loop;
         end;
      end if;

      if Creation_Routine /= null then

         Unit.Push_Local (Created_Offset);

         Creation_Routine.Push_Entity
           (Have_Current => True,
            Context      => Creation_Type.Class_Context,
            Unit         => Unit);

      end if;

      Created_Entity.Pop_Entity (Created_Context, Creation_Type, Unit);

      Unit.Deallocate_Local;

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
      Context    : not null access constant Root_Entity_Type'Class;
      Expression : Node_Id)
   is
   begin
      case N_Expression_Node (Kind (Expression)) is
         when N_Operator =>
            Generate_Operator_Expression (Unit, Context, Expression);
         when N_Precursor =>
            Generate_Precursor (Unit, Context, Expression);
         when N_Attachment_Test =>
            Generate_Expression (Unit, Context, Field_1 (Expression));
         when N_Old =>
            Generate_Expression (Unit, Context, Field_1 (Expression));
         when N_Tuple =>
            Generate_Tuple_Expression (Unit, Context, Expression);
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
                                  To_String (Get_Text (Value));
                     begin
                        Unit.Segment (Tagatha.Read_Only);
                        Unit.Label (Label);
                        Unit.Data (Text'Length);

                        for Ch of Text loop
                           Unit.Data (Character'Pos (Ch));
                        end loop;

                        Unit.Segment (Tagatha.Executable);
                        Unit.Call ("string$create", 0);
                        Unit.Push_Return;
                        Unit.Duplicate;
                        Unit.Push_Label (Label);
                        Unit.Swap;
                        Unit.Call ("string__create_from_string_literal", 2);
                        Unit.Drop;
                        Unit.Drop;
                     end;
                  when N_Character_Constant =>
                     declare
                        Text : constant String :=
                                 To_String (Get_Name (Value));
                     begin
                        Unit.Push
                          (Character'Pos (Text (Text'First)));
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

      if Has_Destination_Type (Expression) then
         Generate_Assignment_Conversion
           (Unit      => Unit,
            From_Type => Get_Type (Expression),
            To_Type   => Get_Destination_Type (Expression));
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

   -------------------
   -- Generate_Loop --
   -------------------

   procedure Generate_Loop
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Context   : not null access constant Root_Entity_Type'Class;
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
         Generate_Compound (Unit, Context, Compound (Initialization_Node));
      end if;

      if Has_Iterator then
         Generate_Expression (Unit, Context, It_Expression);
         New_Cursor_Feature.Push_Entity
           (Have_Current => True,
            Context      => Iterable_Type.Class_Context,
            Unit         => Unit);
         Unit.Allocate_Local;
      end if;

      Unit.Label (Top_Label);

      if Has_Iterator then
         Unit.Duplicate;
         After_Feature.Push_Entity
           (Have_Current => True,
            Context      => Iterator_Type.Class_Context,
            Unit         => Unit);
         Unit.Jump (Out_Label, Tagatha.C_Not_Equal);
      end if;

      if Exit_Condition_Node /= No_Node then
         Generate_Expression (Unit, Context, Expression (Exit_Condition_Node));
         Unit.Operate (Tagatha.Op_Test);
         Unit.Jump (Out_Label, Tagatha.C_Not_Equal);
      end if;

      Generate_Compound (Unit, Context, Compound (Loop_Body_Node));

      if Has_Iterator then
         Unit.Duplicate;
         Next_Feature.Push_Entity
           (Have_Current => True,
            Context      => Iterator_Type.Class_Context,
            Unit         => Unit);
      end if;

      Unit.Jump (Top_Label, Tagatha.C_Always);

      Unit.Label (Out_Label);

      if Iteration_Node /= No_Node then
         Unit.Drop;
         Unit.Deallocate_Local;
      end if;

   end Generate_Loop;

   ----------------------------------
   -- Generate_Operator_Expression --
   ----------------------------------

   procedure Generate_Operator_Expression
     (Unit          : in out Tagatha.Units.Tagatha_Unit;
      Context       : not null access constant Root_Entity_Type'Class;
      Operator_Node : Node_Id)
   is
      Operator  : constant Name_Id := Get_Name (Operator_Node);
      Left      : constant Node_Id := Field_1 (Operator_Node);
      Right     : constant Node_Id := Field_2 (Operator_Node);
   begin

      if Operator = Get_Name_Id ("andthen") then
         declare
            Maybe : constant Positive := Unit.Next_Label;
            Leave : constant Positive := Unit.Next_Label;
         begin
            Generate_Expression (Unit, Context, Left);
            Unit.Operate (Tagatha.Op_Test);
            Unit.Jump (Maybe, Tagatha.C_Not_Equal);
            Unit.Push (0);
            Unit.Jump (Leave);
            Unit.Label (Maybe);
            Generate_Expression (Unit, Context, Right);
            Unit.Label (Leave);
         end;
      elsif Operator = Get_Name_Id ("orelse") then
         declare
            Maybe : constant Positive := Unit.Next_Label;
            Leave : constant Positive := Unit.Next_Label;
         begin
            Generate_Expression (Unit, Context, Left);
            Unit.Operate (Tagatha.Op_Test);
            Unit.Jump (Maybe, Tagatha.C_Equal);
            Unit.Push (1);
            Unit.Jump (Leave);
            Unit.Label (Maybe);
            Generate_Expression (Unit, Context, Right);
            Unit.Label (Leave);
         end;
      elsif Operator = Get_Name_Id ("implies") then
         Generate_Expression (Unit, Context, Left);
         Unit.Operate (Tagatha.Op_Not);
         Generate_Expression (Unit, Context, Right);
         Unit.Operate (Tagatha.Op_Or);
      else
         declare
            Entity : constant Entity_Type := Get_Entity (Operator_Node);

            procedure Push (Argument_Index : Positive);

            ----------
            -- Push --
            ----------

            procedure Push (Argument_Index : Positive) is
               Arg : constant Node_Id :=
                       (if Argument_Index = 1
                        then Left else Right);
            begin
               Generate_Expression (Unit, Context, Arg);
            end Push;

         begin
            if Entity.Intrinsic then
               Entity.Push_Entity
                 (Have_Current => True,
                  Context      => Get_Type (Left).Class_Context,
                  Unit         => Unit);
               Ack.Generate.Intrinsics.Generate_Intrinsic
                 (Unit      => Unit,
                  Name      =>
                    To_Standard_String
                      (Ack.Features.Feature_Entity
                           (Entity).External_Label),
                  Arg_Count => (if Right = No_Node then 1 else 2),
                  Push      => Push'Access);
            else
               if Right /= No_Node then
                  Generate_Expression (Unit, Context, Right);
               end if;
               Generate_Expression (Unit, Context, Left);

               Entity.Push_Entity
                 (Have_Current => True,
                  Context      => Get_Type (Left).Class_Context,
                  Unit         => Unit);
            end if;
         end;
      end if;
   end Generate_Operator_Expression;

   ------------------------
   -- Generate_Precursor --
   ------------------------

   procedure Generate_Precursor
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Context   : not null access constant Root_Entity_Type'Class;
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
        (Actuals_List : List_Id);

      procedure Process
        (Element : Node_Id;
         Last    : Boolean);

      ---------------------
      -- Apply_Arguments --
      ---------------------

      procedure Apply_Arguments
        (Actuals_List : List_Id)
      is
         Actuals_Node_List : constant List_Of_Nodes.List :=
                               List_Table.Element
                                 (Actuals_List).List;

         procedure Apply (Item : Node_Id);

         -----------
         -- Apply --
         -----------

         procedure Apply (Item : Node_Id) is
         begin
            Generate_Expression (Unit, Context, Item);
         end Apply;

      begin
         for Item of reverse Actuals_Node_List loop
            Apply (Item);
         end loop;
      end Apply_Arguments;

      -------------
      -- Process --
      -------------

      procedure Process
        (Element : Node_Id;
         Last    : Boolean)
      is
         use type Ack.Types.Constant_Type_Entity;
         Entity : constant Entity_Type := Get_Entity (Element);
         E_Type : constant Ack.Types.Constant_Type_Entity :=
                    Ack.Types.Constant_Type_Entity (Entity.Get_Type);
      begin
         if not Node_Table.Element (Element).Attached
           and then E_Type /= null
           and then Entity.Standard_Name /= "void"
           and then not E_Type.Expanded
           and then Entity.Can_Update
           and then not Entity.Attached
           and then not E_Type.Detachable
           and then not E_Type.Deferred
           and then not E_Type.Is_Generic_Formal_Type
         then

--              Ada.Text_IO.Put_Line
--                (Get_Program (Element).Show_Location
--                 & ": implicit create test: entity = "
--                 & Entity.Description
--                 & "; context = "
--                 & E_Type.Class_Context.Description);

            Entity.Push_Entity_Address
              (Have_Current => Element /= First_Element,
               Context      => Get_Context (Element),
               Unit         => Unit);

            declare
               Label : constant Positive := Unit.Next_Label;
            begin
               Unit.Duplicate;
               Unit.Dereference;
               Unit.Operate (Tagatha.Op_Test);
               Unit.Jump (Label, Tagatha.C_Not_Equal);

               if not E_Type.Has_Default_Creation_Routine then
                  Unit.Push_Text
                    (Get_Program (Element).Show_Location
                     & ": no default create routine for "
                     & Entity.Qualified_Name);
                  Unit.Native_Operation ("trap 15", 0, 0, "");
               else
                  Unit.Duplicate;
                  Unit.Call
                    (E_Type.Link_Name & "$create", 0);
                  Unit.Push_Return;

                  declare
                     Create_Routine : constant Entity_Type :=
                                        E_Type.Default_Creation_Routine;
                  begin
                     if Create_Routine /= null then
                        Unit.Duplicate;
                        Create_Routine.Push_Entity
                          (Have_Current => True,
                           Context      => E_Type.Class_Context,
                           Unit         => Unit);
                     end if;
                  end;

                  Unit.Swap;
                  Unit.Store;
               end if;
               Unit.Label (Label);
            end;

            Unit.Dereference;

         else

            Entity.Push_Entity
              (Have_Current => Element /= First_Element,
               Context      => Get_Context (Element),
               Unit         => Unit);

         end if;

         if not Last then
            Previous_Entity := Constant_Entity_Type (Entity);
            Previous_Context :=
              Ack.Classes.Constant_Class_Entity
                (Get_Context (Element).Class_Context);
         end if;

      exception
         when others =>
            Ada.Text_IO.Put_Line
              (Get_Program (Element).Show_Location
               & ": process failed for "
               & Entity.Qualified_Name);
            raise;
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
            Entity           : constant Entity_Type := Get_Entity (Element);
         begin
            if Element = Last_Element
              or else Actual_List /= No_List
              or else Entity.Intrinsic
            then

               if Entity.Intrinsic then
                  declare
                     Arg_Count : constant Natural :=
                                   Natural
                                     (List_Table.Element (Actual_List)
                                      .List.Length);
                     Args      : Array_Of_Nodes (1 .. Arg_Count);
                     Index     : Natural := 0;

                     procedure Push (Arg : Positive);

                     ----------
                     -- Push --
                     ----------

                     procedure Push (Arg : Positive) is
                     begin
                        Generate_Expression (Unit, Context, Args (Arg));
                     end Push;

                  begin
                     for Arg of List_Table.Element (Actual_List).List loop
                        Index := Index + 1;
                        Args (Index) := Arg;
                     end loop;
                     pragma Assert (Index = Arg_Count);

                     for Item of Pending loop
                        Process (Item, Item = Last_Element);
                     end loop;

                     Ack.Generate.Intrinsics.Generate_Intrinsic
                       (Unit      => Unit,
                        Arg_Count => Args'Length,
                        Name      =>
                          To_Standard_String
                            (Ack.Features.Feature_Entity
                                 (Entity).External_Label),
                        Push      => Push'Access);
                  end;
               else

                  if Actual_List /= No_List then
                     Apply_Arguments (Actual_List);
                  end if;

                  for Item of Pending loop
                     Process (Item, Item = Last_Element);
                  end loop;
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
         if Expanded
           and then Current_Context.Update_Expanded_Value
           and then not Has_Result
         then
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
      Context : not null access constant Root_Entity_Type'Class;
      Retry   : Node_Id)
   is
      pragma Unreferenced (Context);
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
      Context    : not null access constant Root_Entity_Type'Class;
      Value_Type : not null access constant Root_Entity_Type'Class;
      Node       : Node_Id)
   is
      pragma Unreferenced (Context);
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

      Entity.Clear_Attached;

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
      Context    : not null access constant Root_Entity_Type'Class;
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
      for Arg of reverse Actual_Nodes loop
         Generate_Expression (Unit, Context, Arg);
      end loop;

      Unit.Call
        (Tuple_Type.Link_Name & "$create", 0);
      Unit.Push_Return;
      Unit.Duplicate;
      Unit.Pop_Local
        (Tagatha.Local_Offset (Context.Shelf ("tuple-expression")));

      Unit.Call (Make_Name, Actual_Nodes'Length + 1);

      for I in 1 .. Actual_Nodes'Length + 1 loop
         Unit.Drop;
      end loop;

      Unit.Push_Local
        (Tagatha.Local_Offset (Context.Shelf ("tuple-expression")));

   end Generate_Tuple_Expression;

   -----------------------
   -- Next_String_Label --
   -----------------------

   function Next_String_Label
     (Base_Name : String)
      return String
   is
      Head : String := Base_Name;
      S : constant String := Natural'Image (String_Label_Index);
   begin
      for Ch of Head loop
         if Ch = '-' then
            Ch := '_';
         end if;
      end loop;
      String_Label_Index := String_Label_Index + 1;
      return Head & "$str_" & S (2 .. S'Last);
   end Next_String_Label;

end Ack.Generate;
