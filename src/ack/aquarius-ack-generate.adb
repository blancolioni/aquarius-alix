with Ada.Strings.Fixed;

with Tagatha.Operands;
with Tagatha.Units;

with Aquarius.Config_Paths;

--  with Aquarius.Ack.Files;

package body Aquarius.Ack.Generate is

   procedure Generate_Allocator
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Class : Entity_Id);

   procedure Generate_Default_Create
     (Unit  : in out Tagatha.Units.Tagatha_Unit;
      Class : Entity_Id);

   procedure Generate_Feature
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Feature : Entity_Id);

   procedure Generate_Compound
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Node    : Node_Id);

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
      Class : Entity_Id)
   is
      Scanned : List_Of_Entities.List;

      function Is_Feature (Item : Entity_Id) return Boolean
      is (Get_Kind (Item) in Feature_Entity_Kind);

      procedure Set_Value (Item : Entity_Id);

      procedure Generate_Local_Allocator
        (Ancestor_Class : Entity_Id);

      procedure Scan_Hierarchy
        (Current : Entity_Id);

      ------------------------------
      -- Generate_Local_Allocator --
      ------------------------------

      procedure Generate_Local_Allocator
        (Ancestor_Class : Entity_Id)
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
           ("set_property " & Get_Link_Name (Ancestor_Class));
      end Generate_Local_Allocator;

      --------------------
      -- Scan_Hierarchy --
      --------------------

      procedure Scan_Hierarchy (Current : Entity_Id) is

         procedure Generate (Inherit : Node_Id);

         --------------
         -- Generate --
         --------------

         procedure Generate (Inherit : Node_Id) is
         begin
            Scan_Hierarchy (Get_Entity (Inherit));
         end Generate;

      begin
         if not Scanned.Contains (Current) then
            Scanned.Append (Current);
            Generate_Local_Allocator (Current);

            declare
               Inherited : constant Node_Id :=
                             Inheritance
                               (Get_Declaration (Current));
            begin
               if Inherited /= No_Node then
                  Scan (Inherits (Inherited), Generate'Access);
               end if;
            end;
         end if;
      end Scan_Hierarchy;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value (Item : Entity_Id) is
         Definition_Class : constant Entity_Id := Get_Defined_In (Item);
         Original_Feature : constant Entity_Id := Get_Original_Ancestor (Item);
         Original_Class   : constant Entity_Id :=
                              Get_Context (Original_Feature);

      begin
         Unit.Push_Register ("agg");
         Unit.Pop_Register ("op");
         Unit.Native_Operation
           ("get_property " & Get_Link_Name (Original_Class) & ",0",
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");
         Unit.Push_Register ("pv");
         Unit.Pop_Register ("op");

         case Feature_Entity_Kind (Get_Kind (Item)) is
            when Property_Feature_Entity =>
               Unit.Push (0);
               Unit.Pop_Register ("pv");
               Unit.Native_Operation
                 ("set_property " & To_Standard_String (Get_Name (Item)));

            when Routine_Feature_Entity =>
               Unit.Push_Operand
                 (Tagatha.Operands.External_Operand
                    (Get_Link_Name (Definition_Class)
                     & "__" & To_Standard_String (Get_Name (Item)),
                     True),
                  Tagatha.Default_Size);
               Unit.Pop_Register ("pv");
               Unit.Native_Operation
                 ("set_property " & To_Standard_String (Get_Name (Item)));
         end case;
      end Set_Value;

   begin
      Unit.Begin_Routine
        (Get_Link_Name (Class) & "$allocate",
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
      Scan_Hierarchy (Class);
      Scan_Children (Class, Is_Feature'Access, Set_Value'Access);

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
      Entity : constant Entity_Id := Get_Entity (Node);
   begin
      Unit.Create_Unit
        (Get_File_Name (Get_Entity (Node)),
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
           (Feature : Entity_Id)
            return Boolean
         is (Get_Kind (Feature) in Feature_Entity_Kind
             and then Get_Defined_In (Feature) = Entity);

         procedure Generate_Feature (Feature : Entity_Id);

         ----------------------
         -- Generate_Feature --
         ----------------------

         procedure Generate_Feature (Feature : Entity_Id) is
         begin
            Generate_Feature (Unit, Feature);
         end Generate_Feature;

      begin

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
      Class : Entity_Id)
   is
      function Property_Feature
        (Feature : Entity_Id)
            return Boolean
      is (Get_Kind (Feature) = Property_Feature_Entity);

      procedure Clear_Feature_Value (Feature : Entity_Id);

      -------------------------
      -- Clear_Feature_Value --
      -------------------------

      procedure Clear_Feature_Value (Feature : Entity_Id) is
         Original_Feature : constant Entity_Id :=
                              Get_Original_Ancestor (Feature);
         Original_Class   : constant Entity_Id :=
                              Get_Context (Original_Feature);
      begin
         Unit.Push_Register ("r0");
         Unit.Pop_Register ("op");
         Unit.Native_Operation
           ("get_property " & Get_Link_Name (Original_Class) & ",0",
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");
         Unit.Push_Register ("pv");
         Unit.Pop_Register ("op");
         Unit.Push (0);
         Unit.Pop_Register ("pv");
         Unit.Native_Operation
           ("set_property "
            & To_Standard_String (Get_Name (Original_Feature)));
      end Clear_Feature_Value;

   begin
      Unit.Begin_Routine
        (Get_Link_Name (Class) & "$default_create",
         Argument_Words => 1,
         Frame_Words    => 0,
         Result_Words   => 0,
         Global         => True);

      Unit.Push_Argument (1);
      Unit.Pop_Register ("r0");

      Scan_Children (Class, Property_Feature'Access,
                     Clear_Feature_Value'Access);
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
      Feature : Entity_Id)
   is
      Feature_Node : constant Node_Id := Get_Declaration (Feature);
      Dec_Body     : constant Node_Id := Declaration_Body (Feature_Node);
      Arg_Node     : constant Node_Id := Formal_Arguments (Dec_Body);
      Type_Node    : constant Node_Id := Value_Type (Dec_Body);
      Value_Node   : constant Node_Id := Value (Dec_Body);

      Arg_Count    : constant Natural :=
                       1 + (if Arg_Node = No_Node then 0
                            else Declaration_Count
                              (Entity_Declaration_Group_List (Arg_Node)));
      Frame_Count  : constant Natural := 0;
   begin
      Unit.Begin_Routine
        (Name           =>
           Get_Link_Name (Get_Context (Feature)) & "__"
           & To_Standard_String (Get_Name (Feature)),
         Argument_Words => Arg_Count,
         Frame_Words    => Frame_Count,
         Result_Words   => (if Type_Node = No_Node then 0 else 1),
         Global         => True);

      if Value_Node /= No_Node then
         declare
            Routine_Node : constant Node_Id :=
                             Effective_Routine (Value_Node);
         begin
            case N_Effective_Routine (Kind (Routine_Node)) is
               when N_Internal =>
                  Generate_Compound (Unit, Compound (Routine_Node));
               when N_External =>
                  declare
                     Call : constant String :=
                              (if Feature_Alias (Routine_Node) /= No_Node
                               then To_String
                                 (Get_Name (Feature_Alias (Routine_Node)))
                               else Get_Link_Name (Get_Context (Feature))
                               & "."
                               & To_Standard_String (Get_Name (Feature)));
                     Dot  : constant Natural :=
                              Ada.Strings.Fixed.Index (Call, ".");
                     Object_Name : constant String :=
                                     (if Dot > 0
                                      then Call (Call'First .. Dot - 1)
                                      else "");
                     Property_Name : constant String :=
                                       Call (Dot + 1 .. Call'Last);
                  begin
                     if Dot = 0 then
                        for I in reverse 1 .. Arg_Count loop
                           Unit.Push_Argument
                             (Tagatha.Argument_Offset (I));
                        end loop;
                        Unit.Call (Call);
                        Unit.Drop;
                        if Type_Node /= No_Node then
                           Unit.Push_Register ("r0");
                           Unit.Pop_Local (1);
                        end if;
                     else
                        for I in reverse 2 .. Arg_Count loop
                           Unit.Push_Argument
                             (Tagatha.Argument_Offset (I));
                        end loop;
                        Unit.Push_Operand
                          (Tagatha.Operands.External_Operand
                             (Object_Name, True),
                           Tagatha.Default_Size);
                        Unit.Pop_Register ("op");
                        Unit.Native_Operation
                          ("get_property " & Property_Name & ","
                           & Natural'Image (Arg_Count - 1),
                           Input_Stack_Words  => 0,
                           Output_Stack_Words => 0,
                           Changed_Registers  => "pv");

                        for I in 2 .. Arg_Count loop
                           Unit.Drop;
                        end loop;

                        Unit.Push_Register ("pv");

                     end if;
                  end;
            end case;
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
        (Element        : Node_Id;
         Push_Arguments : Boolean);

      procedure Process
        (Element      : Node_Id;
         Stack_Result : out Boolean);

      ---------------------
      -- Apply_Arguments --
      ---------------------

      procedure Apply_Arguments
        (Element        : Node_Id;
         Push_Arguments : Boolean)
      is
         Entity : constant Entity_Id := Get_Entity (Element);
      begin
         case Get_Kind (Entity) is
            when Class_Entity | Table_Entity =>
               null;
            when Feature_Entity_Kind =>
               declare
                  Actual_List_Node : constant Node_Id :=
                                       Actual_List (Element);
               begin

                  if Actual_List_Node /= No_Node then
                     declare
                        use List_Of_Nodes;
                        Actuals_List      : constant List_Id :=
                                              Node_Table.Element
                                                (Actual_List_Node).List;
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
                     end;
                  end if;

               end;

            when Argument_Entity =>
               null;
            when Local_Entity =>
               null;
            when Result_Entity =>
               null;
         end case;
      end Apply_Arguments;

      -------------
      -- Process --
      -------------

      procedure Process
        (Element      : Node_Id;
         Stack_Result : out Boolean)
      is
         Entity : constant Entity_Id := Get_Entity (Element);
      begin
         Stack_Result := True;
         case Get_Kind (Entity) is
            when Class_Entity | Table_Entity =>
               null;
            when Feature_Entity_Kind =>
               declare
                  Original_Feature : constant Entity_Id :=
                                       Get_Original_Ancestor (Entity);
                  Original_Class   : constant Entity_Id :=
                                       Get_Context (Original_Feature);
                  Feature_Kind     : constant Feature_Entity_Kind :=
                                       Feature_Entity_Kind (Get_Kind (Entity));
               begin

                  if Element = First_Element then
                     Unit.Push_Argument (1);
                  end if;

                  Unit.Pop_Register ("op");
                  Unit.Push_Register ("op");

                  Unit.Native_Operation
                    ("get_property " & Get_Link_Name (Original_Class) & ",0",
                     Input_Stack_Words  => 0,
                     Output_Stack_Words => 0,
                     Changed_Registers  => "pv");
                  Unit.Push_Register ("pv");
                  Unit.Pop_Register ("op");
                  Unit.Native_Operation
                    ("get_property "
                     & To_Standard_String
                       (Get_Name (Original_Feature)) & ",0",
                     Input_Stack_Words  => 0,
                     Output_Stack_Words => 0,
                     Changed_Registers  => "pv");

                  case Feature_Kind is
                     when Routine_Feature_Entity =>
                        Unit.Push_Register ("pv");
                        Unit.Indirect_Call;
                        Unit.Drop;
                        if Get_Type (Original_Feature) /= No_Entity then
                           Unit.Push_Register ("r0");
                        else
                           Stack_Result := False;
                        end if;
                     when Property_Feature_Entity =>
                        Unit.Drop;
                        Unit.Push_Register ("pv");
                  end case;
               end;

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

      for Element of List_Table (List).List loop
         Pending.Append (Element);

         declare
            Entity : constant Entity_Id := Get_Entity (Element);
            Kind   : constant Entity_Kind := Get_Kind (Entity);
            Stack_Result : Boolean;
         begin
            if Element = Last_Element
              or else (Kind in Feature_Entity_Kind
                       and then Actual_List (Element) /= No_Node)
            then
               Apply_Arguments (Element, Push_Arguments => True);
               for Item of Pending loop
                  Process (Item, Stack_Result);
                  pragma Assert
                    (Stack_Result or else Item = Pending.Last_Element);
               end loop;

               if Stack_Result then
                  Unit.Pop_Register ("r0");
               end if;

               Apply_Arguments (Element, Push_Arguments => False);

               if Stack_Result then
                  Unit.Push_Register ("r0");
               end if;

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
      Entity : constant Entity_Id := Get_Entity (Node);
   begin
      case Get_Kind (Entity) is
         when Class_Entity | Table_Entity =>
            null;
         when Property_Feature_Entity =>
            declare
               Original_Feature : constant Entity_Id :=
                                    Get_Original_Ancestor (Entity);
               Original_Class   : constant Entity_Id :=
                                    Get_Context (Original_Feature);
            begin
               Unit.Push_Argument (1);
               Unit.Pop_Register ("op");
               Unit.Native_Operation
                 ("get_property " & Get_Link_Name (Original_Class) & ",0",
                  Input_Stack_Words  => 0,
                  Output_Stack_Words => 0,
                  Changed_Registers  => "pv");
               Unit.Push_Register ("pv");
               Unit.Pop_Register ("op");
               Unit.Pop_Register ("pv");
               Unit.Native_Operation
                 ("set_property "
                  & To_Standard_String (Get_Name (Original_Feature)));
            end;

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
