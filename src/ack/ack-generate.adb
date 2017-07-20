with Tagatha.Operands;
with Tagatha.Units;

with Aquarius.Config_Paths;

with Ack.Classes;
with Ack.Features;

package body Ack.Generate is

   procedure Generate_Allocator
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Class : not null access Ack.Classes.Class_Entity_Record'Class);

   procedure Generate_Default_Create
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Class : not null access Ack.Classes.Class_Entity_Record'Class);

   procedure Generate_Feature
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Feature : not null access constant
        Ack.Features.Feature_Entity_Record'Class);

   procedure Generate_Expression
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Expression : Node_Id);

   procedure Generate_Precursor
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Precursor : Node_Id);

   procedure Generate_Set_Value
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Node    : Node_Id);

   ------------------------
   -- Generate_Allocator --
   ------------------------

   procedure Generate_Allocator
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Class : not null access Ack.Classes.Class_Entity_Record'Class)
   is
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
         Feature.Set_Default_Value (Unit);
      end Set_Value;

   begin
      Unit.Begin_Routine
        (Class.Link_Name & "$allocate",
         Argument_Words => 0,
         Frame_Words    => 0,
         Result_Words   => 1,
         Global         => True);
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

      Class.Scan_Ancestors (True, Generate_Local_Allocator'Access);
      Class.Scan_Features (Set_Value'Access);

      Unit.Pop_Result;
      Unit.End_Routine;

   end Generate_Allocator;

   --------------------------------
   -- Generate_Class_Declaration --
   --------------------------------

   procedure Generate_Class_Declaration
     (Node : Node_Id)
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

      Generate_Allocator (Unit, Entity);
      Generate_Default_Create (Unit, Entity);

      declare

         function Class_Defined_Feature
           (Feature : not null access constant
              Ack.Features.Feature_Entity_Record'Class)
            return Boolean
         is (Feature.Definition_Class = Entity);

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
            Generate_Feature (Unit, Feature);
         end Generate_Feature;

      begin

         Entity.Scan_Features
           (Class_Defined_Feature'Access,
            Generate_Feature'Access);
      end;

      Unit.Finish_Unit;
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
         case N_Instruction (Kind (Instruction)) is
            when N_Assignment =>
               Generate_Expression (Unit, Expression (Instruction));
               Generate_Set_Value (Unit, Variable (Instruction));
            when N_Creation_Instruction =>
               null;
            when N_Conditional =>
               null;
            when N_Loop =>
               null;
            when N_Precursor =>
               Generate_Precursor (Unit, Instruction);
         end case;
      end Generate_Instruction;

   begin
      Scan (Instructions (Node), Generate_Instruction'Access);
   end Generate_Compound;

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
      case N_Expression_Node (Kind (Expression)) is
         when N_Operator =>
            null;
         when N_Precursor =>
            Generate_Precursor (Unit, Expression);
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
               end case;
            end;
      end case;
   end Generate_Expression;

   ----------------------
   -- Generate_Feature --
   ----------------------

   procedure Generate_Feature
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Feature : not null access constant
        Ack.Features.Feature_Entity_Record'Class)
   is
   begin
      Feature.Generate_Routine (Unit);
   end Generate_Feature;

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

--        First_Element : constant Node_Id :=
--                          List_Table (List).List.First_Element;
      Last_Element  : constant Node_Id :=
                        List_Table (List).List.Last_Element;

      procedure Apply_Arguments
        (Actuals_List   : List_Id;
         Push_Arguments : Boolean);

      procedure Process
        (Element      : Node_Id);

      ---------------------
      -- Apply_Arguments --
      ---------------------

      procedure Apply_Arguments
        (Actuals_List   : List_Id;
         Push_Arguments : Boolean)
      is
         Actuals_Node_List : constant List_Of_Nodes.List :=
                               List_Table.Element
                                 (Actuals_List).List;
      begin
         for Item of reverse Actuals_Node_List loop
            if Push_Arguments then
               Generate_Expression (Unit, Item);
            else
               Unit.Drop;
            end if;
         end loop;
      end Apply_Arguments;

      -------------
      -- Process --
      -------------

      procedure Process
        (Element      : Node_Id)
      is
      begin
         Get_Entity (Element).Push_Entity (Unit);

--           Stack_Result := True;
--           case Get_Kind (Entity) is
--              when Class_Entity | Instantiated_Class_Entity =>
--                 null;
--              when Table_Entity | Generic_Argument_Entity =>
--                 null;
--              when Feature_Entity_Kind =>
--                 declare
--                    Original_Feature : constant Entity_Type :=
--                                         Get_Original_Ancestor (Entity);
--                    Original_Class   : constant Entity_Type :=
--                                         Get_Context (Original_Feature);
--                    Feature_Kind     : constant Feature_Entity_Kind :=
--                                  Feature_Entity_Kind (Get_Kind (Entity));
--                 begin
--
--                    if Element = First_Element then
--                       Unit.Push_Argument (1);
--                    end if;
--
--                    Unit.Pop_Register ("op");
--                    Unit.Push_Register ("op");
--
--                    Unit.Native_Operation
--                  ("get_property " & Get_Link_Name (Original_Class) & ",0",
--                       Input_Stack_Words  => 0,
--                       Output_Stack_Words => 0,
--                       Changed_Registers  => "pv");
--                    Unit.Push_Register ("pv");
--                    Unit.Pop_Register ("op");
--                    Unit.Native_Operation
--                      ("get_property "
--                       & To_Standard_String
--                         (Get_Name (Original_Feature)) & ",0",
--                       Input_Stack_Words  => 0,
--                       Output_Stack_Words => 0,
--                       Changed_Registers  => "pv");
--
--                    case Feature_Kind is
--                       when Routine_Feature_Entity =>
--                          Unit.Push_Register ("pv");
--                          Unit.Indirect_Call;
--                          Unit.Drop;
--                          if Get_Type (Original_Feature) /= No_Entity then
--                             Unit.Push_Register ("r0");
--                          else
--                             Stack_Result := False;
--                          end if;
--                       when Property_Feature_Entity =>
--                          Unit.Drop;
--                          Unit.Push_Register ("pv");
--                    end case;
--                 end;
--
--              when Argument_Entity =>
--                 Unit.Push_Argument
--                   (Tagatha.Argument_Offset
--                      (Get_Argument_Offset (Entity)));
--              when Local_Entity =>
--                 Unit.Push_Local
--                   (Tagatha.Local_Offset (Get_Local_Offset (Entity)));
--              when Result_Entity =>
--                 Unit.Push_Local (1);
--           end case;
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
                  Apply_Arguments (Actual_List, True);
               end if;

               for Item of Pending loop
                  Process (Item);
               end loop;

               Unit.Pop_Register ("r0");

               if Actual_List /= No_List then
                  Apply_Arguments (Actual_List, False);
               end if;

               Unit.Push_Register ("r0");

               Pending.Clear;
            end if;
         end;
      end loop;

   end Generate_Precursor;

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

--        case Get_Kind (Entity) is
--           when Class_Entity | Instantiated_Class_Entity =>
--              null;
--           when Table_Entity | Generic_Argument_Entity =>
--              null;
--           when Property_Feature_Entity =>
--              declare
--                 Original_Feature : constant Entity_Type :=
--                                      Get_Original_Ancestor (Entity);
--                 Original_Class   : constant Entity_Type :=
--                                      Get_Context (Original_Feature);
--              begin
--                 Unit.Push_Argument (1);
--                 Unit.Pop_Register ("op");
--                 Unit.Native_Operation
--                   ("get_property " & Get_Link_Name (Original_Class) & ",0",
--                    Input_Stack_Words  => 0,
--                    Output_Stack_Words => 0,
--                    Changed_Registers  => "pv");
--                 Unit.Push_Register ("pv");
--                 Unit.Pop_Register ("op");
--                 Unit.Pop_Register ("pv");
--                 Unit.Native_Operation
--                   ("set_property "
--                    & To_Standard_String (Get_Name (Original_Feature)));
--              end;
--
--           when Routine_Feature_Entity =>
--              raise Program_Error;
--           when Argument_Entity =>
--              Unit.Pop_Argument
--                (Tagatha.Argument_Offset
--                   (Get_Argument_Offset (Entity)));
--           when Local_Entity =>
--              Unit.Pop_Local
--                (Tagatha.Local_Offset (Get_Local_Offset (Entity)));
--           when Result_Entity =>
--              Unit.Pop_Local (1);
--        end case;
   end Generate_Set_Value;

end Ack.Generate;
