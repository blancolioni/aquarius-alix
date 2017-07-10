with Tagatha.Operands;
with Tagatha.Units;

with Aquarius.Config_Paths;

with Aquarius.Ack.Files;

package body Aquarius.Ack.Generate is

   procedure Generate_Feature
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Feature : Entity_Id);

   procedure Generate_Compound
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Table   : Entity_Id;
      Node    : Node_Id);

   procedure Generate_Expression
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Table      : Entity_Id;
      Expression : Node_Id);

   procedure Generate_Precursor
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Table     : Entity_Id;
      Precursor : Node_Id);

   procedure Generate_Set_Value
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Table   : Entity_Id;
      Node    : Node_Id);

   --------------------------------
   -- Generate_Class_Declaration --
   --------------------------------

   procedure Generate_Class_Declaration
     (Node : Node_Id)
   is
      Unit : Tagatha.Units.Tagatha_Unit;
      Entity : constant Entity_Id := Get_Entity (Node);
   begin
      Unit.Create_Unit
        (Aquarius.Ack.Files.Base_File_Name (Get_Entity (Node)),
         Get_Program (Node).Source_File_Name);

      Unit.Begin_Routine
        (Get_Link_Name (Entity) & "$default_create",
         Argument_Words => 1,
         Frame_Words    => 0,
         Result_Words   => 0,
         Global         => True);

      Unit.Push_Argument (1);
      Unit.Pop_Register ("r0");

      declare

         function Virtual_Table_Feature
           (Feature : Entity_Id)
            return Boolean
         is (Get_Kind (Feature) = Routine_Feature_Entity);

         function Property_Feature
           (Feature : Entity_Id)
            return Boolean
         is (Get_Kind (Feature) = Property_Feature_Entity);

         function Class_Defined_Feature
           (Feature : Entity_Id)
            return Boolean
         is (Get_Kind (Feature) in Feature_Entity_Kind
             and then Get_Defined_In (Feature) = Entity);

         procedure Clear_Feature_Value (Feature : Entity_Id);
         procedure Feature_Address (Feature : Entity_Id);
         procedure Generate_Feature (Feature : Entity_Id);

         -------------------------
         -- Clear_Feature_Value --
         -------------------------

         procedure Clear_Feature_Value (Feature : Entity_Id) is
            pragma Unreferenced (Feature);
         begin
            Unit.Push_Register ("r0");
            Unit.Push_Operand
              (Tagatha.Operands.Constant_Operand (4), Tagatha.Default_Size);
            Unit.Operate (Tagatha.Op_Add);
            Unit.Pop_Register ("r0");
            Unit.Push_Operand
              (Tagatha.Operands.Constant_Operand (0), Tagatha.Default_Size);
            Unit.Pop_Operand
              (Tagatha.Operands.Register_Operand
                 ("r0", Dereference => True),
               Tagatha.Default_Size);
         end Clear_Feature_Value;

         ---------------------
         -- Feature_Address --
         ---------------------

         procedure Feature_Address (Feature : Entity_Id) is
         begin
            Unit.Data (Get_Link_Name (Feature));
         end Feature_Address;

         ----------------------
         -- Generate_Feature --
         ----------------------

         procedure Generate_Feature (Feature : Entity_Id) is
         begin
            Generate_Feature (Unit, Feature);
         end Generate_Feature;

         VT_Label : constant String :=
                      Get_Link_Name (Entity) & "$vt";

      begin
         Unit.Push_Operand
           (Tagatha.Operands.External_Operand
              (VT_Label, Immediate => True),
            Tagatha.Default_Size);
         Unit.Pop_Operand
           (Tagatha.Operands.Register_Operand
              ("r0", Dereference => True),
            Tagatha.Default_Size);

         Scan_Children (Entity, Property_Feature'Access,
                        Clear_Feature_Value'Access);

         Unit.End_Routine;
         Unit.Segment (Tagatha.Read_Only);
         Unit.Label (VT_Label, Export => True);

         Scan_Children (Entity, Virtual_Table_Feature'Access,
                        Feature_Address'Access);
         Scan_Children (Entity, Class_Defined_Feature'Access,
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
      Table   : Entity_Id;
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
               Generate_Expression (Unit, Table, Expression (Instruction));
               Generate_Set_Value (Unit, Table, Variable (Instruction));
            when N_Creation_Instruction =>
               null;
            when N_Conditional =>
               null;
         end case;
      end Generate_Instruction;

   begin
      Scan (Instructions (Node), Generate_Instruction'Access);
   end Generate_Compound;

   -------------------------
   -- Generate_Expression --
   -------------------------

   procedure Generate_Expression
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Table      : Entity_Id;
      Expression : Node_Id)
   is
   begin
      case N_Expression_Node (Kind (Expression)) is
         when N_Operator =>
            null;
         when N_Precursor =>
            Generate_Precursor (Unit, Table, Expression);
         when N_Constant =>
            declare
               Value      : constant Node_Id := Constant_Value (Expression);
            begin
               case N_Constant_Value (Kind (Value)) is
                  when N_String_Constant =>
                     Unit.Push_Operand
                       (Tagatha.Operands.Constant_Operand (0),
                        Tagatha.Default_Size);
                  when N_Integer_Constant =>
                     Unit.Push_Operand
                       (Tagatha.Operands.Constant_Operand
                          (Tagatha.Tagatha_Integer'Value
                               (To_String
                                    (Get_Name (Value)))),
                        Tagatha.Default_Size);
               end case;
            end;
      end case;
   end Generate_Expression;

   ----------------------
   -- Generate_Feature --
   ----------------------

   procedure Generate_Feature
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Feature : Entity_Id)
   is
      Feature_Node : constant Node_Id := Get_Declaration (Feature);
      Dec_Body     : constant Node_Id := Declaration_Body (Feature_Node);
      Arg_Node     : constant Node_Id := Formal_Arguments (Dec_Body);
      Type_Node    : constant Node_Id := Value_Type (Dec_Body);
      Value_Node   : constant Node_Id := Value (Dec_Body);
      Table        : constant Entity_Id := Get_Entity (Feature_Node);

      Arg_Count    : constant Natural :=
                       (if Arg_Node = No_Node then 0
                        else Declaration_Count
                          (Entity_Declaration_Group_List (Arg_Node)));
      Frame_Count  : constant Natural :=
                       (if Type_Node = No_Node then 0 else 1);
   begin
      Unit.Begin_Routine
        (Name           => Get_Link_Name (Feature),
         Argument_Words => Arg_Count,
         Frame_Words    => Frame_Count,
         Result_Words   => (if Type_Node = No_Node then 0 else 1),
         Global         => True);

      if Value_Node /= No_Node then
         declare
            Routine_Node : constant Node_Id :=
                             Effective_Routine (Value_Node);
            Compound_Node : constant Node_Id :=
                              Compound (Routine_Node);
         begin
            Generate_Compound (Unit, Table, Compound_Node);
         end;
      end if;

      if Type_Node /= No_Node then
         Unit.Push_Local (1);
         Unit.Pop_Result;
      end if;

      Unit.End_Routine;
   end Generate_Feature;

   ------------------------
   -- Generate_Precursor --
   ------------------------

   procedure Generate_Precursor
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Table     : Entity_Id;
      Precursor : Node_Id)
   is
      pragma Unreferenced (Table);
      List   : constant List_Id :=
                 Node_Table (Precursor).List;

      procedure Process (Element : Node_Id);

      -------------
      -- Process --
      -------------

      procedure Process (Element : Node_Id) is
         Entity : constant Entity_Id := Get_Entity (Element);
      begin
         case Get_Kind (Entity) is
            when Class_Entity | Table_Entity =>
               null;
            when Routine_Feature_Entity =>
               Unit.Push_Argument (1);
               Unit.Dereference (Tagatha.Default_Size);
               Unit.Push_Operand
                 (Tagatha.Operands.Constant_Operand
                    (Tagatha.Tagatha_Integer
                         (Get_Virtual_Table_Offset (Entity) * 4)),
                     Tagatha.Default_Size);
               Unit.Operate (Tagatha.Op_Add, Tagatha.Default_Size);
               Unit.Indirect_Call;
               Unit.Push_Result;
            when Property_Feature_Entity =>
               Unit.Push_Argument (1);
               Unit.Push_Operand
                 (Tagatha.Operands.Constant_Operand
                    (Tagatha.Tagatha_Integer
                         (Get_Property_Offset (Entity) * 4)),
                  Tagatha.Default_Size);
               Unit.Operate (Tagatha.Op_Add, Tagatha.Default_Size);
               Unit.Dereference;
            when Argument_Entity =>
               Unit.Push_Argument
                 (Tagatha.Argument_Offset
                    (Get_Argument_Offset (Entity)));
            when Local_Entity =>
               Unit.Push_Local
                 (Tagatha.Local_Offset (Get_Local_Offset (Entity)));
            when Result_Entity =>
               Unit.Push_Local (1);
         end case;
      end Process;

   begin

      Scan (List, Process'Access);

   end Generate_Precursor;

   ------------------------
   -- Generate_Set_Value --
   ------------------------

   procedure Generate_Set_Value
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Table   : Entity_Id;
      Node    : Node_Id)
   is
      pragma Unreferenced (Table);
      Entity : constant Entity_Id := Get_Entity (Node);
   begin
      case Get_Kind (Entity) is
         when Class_Entity | Table_Entity =>
            null;
         when Property_Feature_Entity =>
            Unit.Push_Argument (1);
            Unit.Push_Operand
              (Tagatha.Operands.Constant_Operand
                 (Tagatha.Tagatha_Integer
                      (Get_Property_Offset (Entity) * 4)),
               Tagatha.Default_Size);
            Unit.Operate (Tagatha.Op_Add, Tagatha.Default_Size);
         when Routine_Feature_Entity =>
            raise Program_Error;
         when Argument_Entity =>
            Unit.Pop_Argument
              (Tagatha.Argument_Offset
                 (Get_Argument_Offset (Entity)));
         when Local_Entity =>
            Unit.Pop_Local
              (Tagatha.Local_Offset (Get_Local_Offset (Entity)));
         when Result_Entity =>
            Unit.Pop_Local (1);
      end case;
   end Generate_Set_Value;

end Aquarius.Ack.Generate;
