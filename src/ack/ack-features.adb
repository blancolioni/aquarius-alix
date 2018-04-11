with Ada.Strings.Fixed;

with Tagatha.Operands;

with Ack.Classes;
with Ack.Generate;
with Ack.Types;

with Ack.Generate.Primitives;

package body Ack.Features is

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
     (Feature   : in out Feature_Entity_Record'Class;
      Name_Node : in     Node_Id;
      Arg_Type  : not null access Ack.Types.Type_Entity_Record'Class)
   is
   begin
      Feature.Arguments.Append
        (Ack.Variables.New_Argument_Entity
           (Get_Name (Name_Node), Name_Node, Arg_Type));
      Feature.Property := False;
   end Add_Argument;

   ------------------
   -- Add_Implicit --
   ------------------

   overriding procedure Add_Implicit
     (Feature         : in out Feature_Entity_Record;
      Implicit_Entity : not null access Root_Entity_Type'Class)
   is
   begin
      Root_Entity_Type (Feature).Add_Implicit (Implicit_Entity);
      if Ack.Variables.Is_Variable (Implicit_Entity) then
         Feature.Local_Count := Feature.Local_Count + 1;
         Ack.Variables.Variable_Entity_Record (Implicit_Entity.all)
           .Set_Offset (Feature.Local_Count);
      end if;
   end Add_Implicit;

   ---------------
   -- Add_Local --
   ---------------

   procedure Add_Local
     (Feature    : in out Feature_Entity_Record'Class;
      Name_Node  : in     Node_Id;
      Local_Type : not null access Ack.Types.Type_Entity_Record'Class)
   is
   begin
      Feature.Locals.Append
        (Ack.Variables.New_Local_Entity
           (Get_Name (Name_Node), Name_Node, Local_Type));
   end Add_Local;

   -----------------------
   -- Add_Postcondition --
   -----------------------

   procedure Add_Postcondition
     (Feature   : in out Feature_Entity_Record'Class;
      Tag       : Name_Id;
      Condition : Node_Id)
   is
   begin
      Feature.Postconditions.Append ((Tag, Condition));

   end Add_Postcondition;

   ----------------------
   -- Add_Precondition --
   ----------------------

   procedure Add_Precondition
     (Feature   : in out Feature_Entity_Record'Class;
      Tag       : Name_Id;
      Condition : Node_Id)
   is
   begin
      Feature.Preconditions.Append ((Tag, Condition));
   end Add_Precondition;

   ----------
   -- Bind --
   ----------

   overriding procedure Bind
     (Feature : not null access Feature_Entity_Record)
   is
      use type Ack.Classes.Class_Entity;
      Next_Argument : Positive := 1;
      Next_Local    : Positive := 1;
      Current_Class : constant Ack.Classes.Class_Entity :=
                        Ack.Classes.Class_Entity
                          (if Feature.Effective_Class = null
                           then Feature.Definition_Class
                           else Feature.Effective_Class);
      Current       : constant Ack.Variables.Variable_Entity :=
                        Ack.Variables.New_Argument_Entity
                          (Name          => Get_Name_Id ("Current"),
                           Node          => Feature.Declaration_Node,
                           Argument_Type =>
                             Ack.Types.New_Class_Type
                               (Feature.Declaration_Node, Current_Class,
                                Detachable => False));
   begin
      Current.Set_Attached;
      if Current_Class.Frame_Words > 0 then
         Current.Set_Offset (Current_Class.Frame_Words);
         Feature.Insert (Current);
      end if;

      Next_Argument := Current_Class.Frame_Words + 1;

      for Argument of Feature.Arguments loop
         Argument.Set_Offset (Next_Argument);
         Next_Argument := Next_Argument + 1;
         Feature.Insert (Argument);
      end loop;

      if Feature.Has_Result then
         declare
            Result : constant Ack.Variables.Variable_Entity :=
                        Ack.Variables.New_Local_Entity
                         (Get_Name_Id ("Result"),
                          Feature.Declaration_Node,
                          Feature.Value_Type);
         begin
            Result.Set_Offset (1);
            Feature.Insert (Result);
            Next_Local := 2;
         end;
      end if;

      for Local of Feature.Locals loop
         Local.Set_Offset (Next_Local);
         Next_Local := Next_Local + 1;
         Feature.Insert (Local);
      end loop;

      Feature.Local_Count := Next_Local - 1;
   end Bind;

   ------------------------
   -- Check_Precondition --
   ------------------------

   procedure Check_Precondition
     (Feature       : Feature_Entity_Record'Class;
      Unit          : in out Tagatha.Units.Tagatha_Unit;
      Success_Label : Positive;
      Fail_Label    : Natural)
   is
   begin
      if Feature.Redefined_Feature /= null then
         if False then
            declare
               Ancestor_Fail : constant Positive := Unit.Next_Label;
            begin
               Feature.Redefined_Feature.Check_Precondition
                 (Unit          => Unit,
                  Success_Label => Success_Label,
                  Fail_Label    => Ancestor_Fail);
               Unit.Label (Ancestor_Fail);
            end;
         end if;
      end if;

      for Clause of Feature.Preconditions loop
         declare
            Out_Label : constant Positive := Unit.Next_Label;
         begin
            Ack.Generate.Generate_Expression (Unit, Clause.Node);
            Unit.Operate (Tagatha.Op_Test);
            Unit.Jump (Out_Label, Tagatha.C_Not_Equal);

            if Fail_Label > 0 then
               Unit.Jump (Fail_Label, Tagatha.C_Always);
            else
               Unit.Push_Text
                 (Get_Program (Clause.Node).Show_Location
                  & ": assertion failure in precondition of "
                  & Feature.Qualified_Name
                  & (if Clause.Tag = No_Name then ""
                    else ": " & To_String (Clause.Tag)));
               Unit.Pop_Register ("r0");

               if Feature.Rescue_Node in Real_Node_Id then
                  Unit.Jump (Feature.Link_Name & "$rescue");
               else
                  Unit.Push_Register ("pc");
                  Unit.Native_Operation ("trap 15", 0, 0, "");
               end if;
            end if;

            Unit.Label (Out_Label);
         end;
      end loop;
      Unit.Jump (Success_Label);
   end Check_Precondition;

   -------------------
   -- Class_Context --
   -------------------

   overriding function Class_Context
     (Feature : not null access constant Feature_Entity_Record)
      return Constant_Entity_Type
   is (if Feature.Redefined_Class /= null
       then Constant_Entity_Type (Feature.Redefined_Class)
       else Constant_Entity_Type (Feature.Definition_Class));

   -----------------
   -- Description --
   -----------------

   overriding function Description
     (Feature : Feature_Entity_Record)
      return String
   is
   begin
      return Root_Entity_Type (Feature).Description
        & " parent " & Feature.Declaration_Context.Full_Name;
   end Description;

   ---------------------
   -- Effective_Class --
   ---------------------

   function Effective_Class
     (Feature : Feature_Entity_Record'Class)
      return access constant Ack.Classes.Class_Entity_Record'Class
   is
   begin
      return Feature.Effective_Class;
   end Effective_Class;

   ---------------
   -- Full_Name --
   ---------------

   overriding function Full_Name
     (Feature : Feature_Entity_Record)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      if not Feature.Arguments.Is_Empty then
         for Arg of Feature.Arguments loop
            if Result = Null_Unbounded_String then
               Result := Result & "(";
            else
               Result := Result & ",";
            end if;
            Result := Result
              & Entity_Type (Arg).Value_Type.Full_Name;
         end loop;
         Result := Result & ")";
      end if;

      Result := Feature.Qualified_Name & Result;

      if Feature.Value_Type /= null then
         Result := Result & ": " & Feature.Value_Type.Full_Name;
      end if;
      return To_String (Result);
   end Full_Name;

   -------------------------------
   -- Generate_Allocation_Value --
   -------------------------------

   procedure Generate_Allocation_Value
     (Feature : Feature_Entity_Record'Class;
      Unit    : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      null;
   end Generate_Allocation_Value;

   -------------------------
   -- Generate_Definition --
   -------------------------

   procedure Generate_Routine
     (Feature : Feature_Entity_Record'Class;
      Class   : not null access constant Ack.Classes.Class_Entity_Record'Class;
      Unit    : in out Tagatha.Units.Tagatha_Unit)
   is
      Arg_Count    : constant Natural :=
                       1 + Natural (Feature.Arguments.Length);
      Result_Count : constant Natural :=
                       (if Feature.Value_Type /= null
                        then 1 else 0);
      Once_Flag_Label  : constant String :=
                           "once_flag$" & Feature.Link_Name;
      Once_Value_Label : constant String :=
                           "once_value$" & Feature.Link_Name;
      Exit_Label       : constant Positive := Unit.Next_Label;
      Rescue_Label     : constant String :=
                           Feature.Link_Name & "$rescue";
   begin
      if Feature.Property then
         --  Generate a property routine, in case it redefines
         --  an actual routine

         Unit.Begin_Routine
           (Name           => Feature.Link_Name,
            Argument_Words => 1,
            Frame_Words    => 0,
            Result_Words   => 1,
            Global         => True);

         Unit.Push_Argument (1);
         Push_Offset (Unit, Feature.Property_Offset);
         Unit.Operate (Tagatha.Op_Add);
         Unit.Dereference;

         Unit.Pop_Result;

         Unit.End_Routine;

      elsif Feature.External then
         --  Generate a wrapper that can be accessed via a virtual table entry

         Unit.Begin_Routine
           (Name           => Feature.Link_Name,
            Argument_Words => Arg_Count,
            Frame_Words    => 0,
            Result_Words   => Result_Count,
            Global         => True);

         for I in reverse 1 .. Arg_Count loop
            Unit.Push_Argument (Tagatha.Argument_Offset (I));
         end loop;

         Feature.Push_Entity (True, Feature.Active_Class, Unit);

         if Result_Count > 0 then
            Unit.Pop_Result;
         end if;

         Unit.End_Routine;

      elsif Feature.Routine then

         --  Don't allocate a frame, because we do it later
         --  by pushing zeroes onto the stack.  We don't need
         --  to worry about popping the frame off at the end
         --  of the routine, because this is done when we
         --  transfer the frame pointer to the stack pointer.
         Unit.Begin_Routine
           (Name           => Feature.Link_Name,
            Argument_Words => Arg_Count,
            Frame_Words    => 0,
            Result_Words   => Result_Count,
            Global         => True);

         --  restore Current to our needs

         if not Feature.Active_Class.Expanded
           and then Feature.Definition_Class /= Class
         then
            Unit.Push_Argument (1);
            Unit.Push_Argument (1);
            Unit.Dereference;
            Unit.Dereference;
            Unit.Operate (Tagatha.Op_Sub);
            Unit.Pop_Argument (1);
         end if;

         if Feature.Rescue_Node in Real_Node_Id then
            Unit.Directive (".exception "
                            & Feature.Link_Name
                            & " "
                            & Rescue_Label
                            & " "
                            & Rescue_Label);

            Unit.Label (Feature.Link_Name & "$retry");
         end if;

         if Feature.Declaration_Context.Monitor_Preconditions then
            declare
               Success_Label : constant Positive := Unit.Next_Label;
            begin
               Feature.Check_Precondition
                 (Unit          => Unit,
                  Success_Label => Success_Label,
                  Fail_Label    => 0);
               Unit.Label (Success_Label);
            end;

         end if;

         if Feature.Once then
            declare
               Continue_Once : constant Positive := Unit.Next_Label;
            begin
               Unit.Push_Register (Once_Flag_Label);
               Unit.Operate (Tagatha.Op_Test, Tagatha.Default_Size);
               Unit.Jump (Continue_Once, Tagatha.C_Equal);
               if Feature.Has_Result then
                  Unit.Push_Register (Once_Value_Label);
                  Unit.Pop_Result;
               end if;
               Unit.Jump (Exit_Label);
               Unit.Label (Continue_Once);
            end;
         end if;

         for I in 1 .. Feature.Local_Count loop
            Unit.Push (0);
         end loop;

         if Feature.Monitor_Postconditions then
            for Old of Feature.Olds loop
               Ack.Generate.Generate_Expression (Unit, Old);
            end loop;
         end if;

         Ack.Generate.Generate_Compound
           (Unit, Compound (Feature.Routine_Node));

         if Feature.Monitor_Postconditions then
            for Clause of Feature.Postconditions loop
               declare
                  Out_Label : constant Positive := Unit.Next_Label;
               begin
                  Ack.Generate.Generate_Expression (Unit, Clause.Node);
                  Unit.Operate (Tagatha.Op_Test);
                  Unit.Jump (Out_Label, Tagatha.C_Not_Equal);
                  Unit.Push_Text
                    (Get_Program (Clause.Node).Show_Location
                     & ": assertion failure in postcondition of "
                     & Feature.Qualified_Name
                     & (if Clause.Tag = No_Name then ""
                       else ": " & To_String (Clause.Tag)));
                  Unit.Pop_Register ("r0");
                  if Feature.Rescue_Node in Real_Node_Id then
                     Unit.Jump (Rescue_Label);
                  else
                     Unit.Push_Register ("pc");
                     Unit.Native_Operation ("trap 15", 0, 0, "");
                  end if;
                  Unit.Label (Out_Label);
               end;
            end loop;
         end if;

         if Feature.Has_Result then
            if Feature.Once then
               Unit.Push (1);
               Unit.Pop_Register (Once_Flag_Label);
               Unit.Push_Local (1);
               Unit.Pop_Register (Once_Value_Label);
            end if;

            Unit.Push_Local (1);
            Unit.Pop_Result;
         end if;

         Unit.Jump (Exit_Label, Tagatha.C_Always);

         Unit.Label (Feature.Link_Name & "$rescue", Export => True);
         if Feature.Rescue_Node in Real_Node_Id then
            Unit.Set_Property ("retry_label", Feature.Link_Name & "$retry");
            Ack.Generate.Generate_Compound
              (Unit, Compound (Feature.Rescue_Node));
            Unit.Clear_Property ("retry_label");
         end if;

         Unit.Native_Operation ("mov fp, sp", 0, 0, "sp");
         Unit.Native_Operation ("mov (sp)+, fp", 1, 0, "sp,fp");
         Unit.Native_Operation ("trap 15", 1, 0, "r0");

         Unit.Label (Exit_Label);
         Unit.End_Routine;

         if Feature.Once then
            Unit.Label (Once_Flag_Label);
            Unit.Data (0);
            Unit.Label (Once_Value_Label);
            Unit.Data (0);
         end if;

      end if;
   end Generate_Routine;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Entity             : not null access Feature_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type
   is
      Instan : constant Feature_Entity :=
                 new Feature_Entity_Record'(Entity.all);
   begin
      if Instan.Value_Type /= null then
         Instan.Value_Type :=
           Instan.Value_Type.Instantiate (Type_Instantiation);
      end if;

      for Arg of Instan.Arguments loop
         Arg :=
           Ack.Variables.Variable_Entity
             (Arg.Instantiate (Type_Instantiation));
      end loop;

      return Entity_Type (Instan);

   end Instantiate;

   --------------------------
   -- Is_Property_Of_Class --
   --------------------------

   function Is_Property_Of_Class
     (Feature : Feature_Entity_Record'Class;
      Class   : not null access constant
        Ack.Classes.Class_Entity_Record'Class)
      return Boolean
   is
      use Ack.Classes;
   begin
      return not Feature.Deferred
        and then Feature.Is_Property
        and then Constant_Class_Entity (Feature.Active_Class)
        = Constant_Class_Entity (Class);
   end Is_Property_Of_Class;

   -----------------
   -- New_Feature --
   -----------------

   function New_Feature
     (Name        : Name_Id;
      Alias       : Name_Id;
      Declaration : Node_Id;
      Property    : Boolean;
      Class       : not null access Ack.Classes.Class_Entity_Record'Class)
      return Feature_Entity
   is
   begin
      return Feature : constant Feature_Entity := new Feature_Entity_Record do
         Feature.Create
           (Name, Declaration,
            Table              => True,
            Parent_Environment => Class,
            Context            => Class);
         Feature.Alias := Alias;
         Feature.Effective_Class := Class;
         Feature.Definition_Class := Class;
         Feature.Property := Property;
      end return;
   end New_Feature;

   ----------------
   -- Pop_Entity --
   ----------------

   overriding procedure Pop_Entity
     (Feature : Feature_Entity_Record;
      Unit    : in out Tagatha.Units.Tagatha_Unit)
   is
      pragma Assert (Feature.Property);
   begin
--        Ada.Text_IO.Put_Line
--          ("pop entity: " & Feature.Active_Class.Qualified_Name
--           & "." & Feature.Declared_Name
--           & ": expanded = "
--           & (if Feature.Active_Class.Expanded then "yes" else "no"));

      if Feature.Active_Class.Expanded then
         Unit.Pop_Argument (1);
      else
         Unit.Push_Argument (1);
         Push_Offset (Unit, Feature.Property_Offset);
         Unit.Operate (Tagatha.Op_Add);
         Unit.Pop_Register ("op");
         Unit.Pop_Register ("pv");
         Unit.Native_Operation
           ("mov pv, (op)",
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "");
      end if;
   end Pop_Entity;

   -----------------
   -- Push_Entity --
   -----------------

   overriding procedure Push_Entity
     (Feature       : Feature_Entity_Record;
      Have_Current  : Boolean;
      Context       : not null access constant Root_Entity_Type'Class;
      Unit          : in out Tagatha.Units.Tagatha_Unit)
   is
      Current        : constant Ack.Classes.Constant_Class_Entity :=
                         Ack.Classes.Constant_Class_Entity (Context);
   begin

--        Ada.Text_IO.Put_Line
--          ("push feature: context " & Current.Qualified_Name
--           & "; feature "
--           & Feature.Active_Class.Qualified_Name
--           & "." & Feature.Declared_Name);

      if Feature.Standard_Name = "void" then
         Unit.Push (0);
         return;
      elsif Feature.Explicit_Value then
         case N_Constant_Value (Kind (Feature.Explicit_Value_Node)) is
            when N_String_Constant =>
               Unit.Push_Text
                 (To_String (Get_Name (Feature.Explicit_Value_Node)));
            when N_Integer_Constant =>
               Unit.Push
                 (Tagatha.Tagatha_Integer'Value
                    (To_String (Get_Name (Feature.Explicit_Value_Node))));
            when N_Boolean_Constant =>
               Unit.Push
                 (Boolean'Pos (Boolean_Value (Feature.Explicit_Value_Node)));
         end case;
         return;
      end if;

      if not Have_Current then
         Unit.Push_Argument (1);
      end if;

      if Feature.Definition_Class /= Current
        and then not Current.Expanded
        and then not Feature.Intrinsic
      then

         Unit.Pop_Register ("op");
         Unit.Push_Register ("op");
         Unit.Push_Register ("op");
         Unit.Dereference;
         Push_Offset
           (Unit,
            Current.Ancestor_Table_Offset (Feature.Definition_Class));
         Unit.Operate (Tagatha.Op_Add);
         Unit.Dereference;
         Unit.Operate (Tagatha.Op_Add);
      end if;

      if Feature.Intrinsic then
         Ack.Generate.Primitives.Generate_Intrinsic
           (Unit, Feature.External_Label);
      elsif Feature.Is_Property then
         if not Feature.Active_Class.Expanded then
            Push_Offset (Unit, Feature.Property_Offset);
            Unit.Operate (Tagatha.Op_Add);
            Unit.Dereference;
         end if;
      else
         if Current.Expanded then
            Unit.Call (Feature.Link_Name);
         else
            --  push feature address from virtual table
            Unit.Pop_Register ("op");
            Unit.Push_Register ("op");
            Unit.Push_Register ("op");
            Unit.Dereference;
            Push_Offset (Unit, Feature.Virtual_Table_Offset);
            Unit.Operate (Tagatha.Op_Add);
            Unit.Dereference;
            Unit.Indirect_Call;
         end if;

         if Current.Expanded and then not Feature.Has_Result then
            if Current.Frame_Words = 1 then
               if Feature.Argument_Count > 0 then
                  Unit.Pop_Register ("r0");
                  for I in 1 .. Feature.Argument_Count loop
                     Unit.Drop;
                  end loop;
                  Unit.Push_Register ("r0");
               end if;
            end if;
         else

            Unit.Drop;  --  current
            for I in 1 .. Feature.Argument_Count loop
               Unit.Drop;
            end loop;

            --  push result
            if Feature.Has_Result then
               Unit.Push_Register ("r0");
            end if;
         end if;
      end if;

   end Push_Entity;

   --------------------
   -- Push_Old_Value --
   --------------------

   overriding procedure Push_Old_Value
     (Feature : in out Feature_Entity_Record;
      Unit    : in out Tagatha.Units.Tagatha_Unit;
      Node    : Node_Id)
   is
   begin
      for I in 1 .. Feature.Olds.Last_Index loop
         if Feature.Olds.Element (I) = Node then
            Unit.Push_Local
              (Tagatha.Local_Offset
                 (Feature.Local_Count
                  + (if Feature.Has_Result then 1 else 0)
                  + I));
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "can't find old value";
   end Push_Old_Value;

   ---------------------
   -- Remove_Implicit --
   ---------------------

   overriding procedure Remove_Implicit
     (Feature         : in out Feature_Entity_Record)
   is
      Implicit_Entity : constant Entity_Type :=
                          Feature.Children.Implicits.Last_Element;
   begin
      Root_Entity_Type (Feature).Remove_Implicit;
      if Ack.Variables.Is_Variable (Implicit_Entity) then
         Feature.Local_Count := Feature.Local_Count - 1;
      end if;
   end Remove_Implicit;

   --------------------
   -- Save_Old_Value --
   --------------------

   overriding procedure Save_Old_Value
     (Feature : in out Feature_Entity_Record;
      Node    : Node_Id)
   is
   begin
      for N of Feature.Olds loop
         if N = Node then
            return;
         end if;
      end loop;
      Feature.Olds.Append (Node);
   end Save_Old_Value;

   ---------------------------
   -- Scan_Original_Classes --
   ---------------------------

   procedure Scan_Original_Classes
     (Feature : Feature_Entity_Record'Class;
      Process : not null access
        procedure (Class : not null access constant
                     Ack.Classes.Class_Entity_Record'Class))
   is
   begin
      if Feature.Original_Classes.Is_Empty then
         if Feature.Deferred then
            null;
         else
            Process (Feature.Effective_Class);
         end if;
      else
         for Class of Feature.Original_Classes loop
            Process (Ack.Classes.Class_Entity (Class));
         end loop;
      end if;
   end Scan_Original_Classes;

   -----------------
   -- Set_Creator --
   -----------------

   procedure Set_Creator
     (Feature     : in out Feature_Entity_Record'Class)
   is
   begin
      Feature.Creator := True;
   end Set_Creator;

   -----------------------
   -- Set_Default_Value --
   -----------------------

   procedure Set_Default_Value
     (Feature          : Feature_Entity_Record;
      Current_Property : in out Name_Id;
      Unit             : in out Tagatha.Units.Tagatha_Unit)
   is

      procedure Set (Class : not null access constant
                       Ack.Classes.Class_Entity_Record'Class);

      ---------
      -- Set --
      ---------

      procedure Set (Class : not null access constant
                       Ack.Classes.Class_Entity_Record'Class)
      is
      begin
         if Current_Property = No_Name
           or else Class.Link_Name /= To_Standard_String (Current_Property)
         then
            Unit.Push_Register ("agg");
            Unit.Pop_Register ("op");
            Unit.Native_Operation
              ("get_property "
               & Class.Link_Name & ",0",
               Input_Stack_Words  => 0,
               Output_Stack_Words => 0,
               Changed_Registers  => "pv");
            Unit.Push_Register ("pv");
            Unit.Pop_Register ("op");
            Current_Property := Get_Name_Id (Class.Link_Name);
         end if;

         if Feature.Property then
            Unit.Push (0);
            Unit.Pop_Register ("pv");
            Unit.Native_Operation
              ("set_property " & Feature.Standard_Name);
         elsif Feature.Routine then
            Unit.Push_Operand
              (Tagatha.Operands.External_Operand
                 (Feature.Effective_Class.Link_Name
                  & "__" & Feature.Standard_Name,
                  True),
               Tagatha.Default_Size);
            Unit.Pop_Register ("pv");
            Unit.Native_Operation
              ("set_property " & Feature.Standard_Name);
         else
            raise Constraint_Error with
              "expected a property or a routine, but found '"
              & (-Feature.Source_Name) & "'";
         end if;
      end Set;

   begin
      Feature.Scan_Original_Classes (Set'Access);
   end Set_Default_Value;

   ------------------
   -- Set_Deferred --
   ------------------

   procedure Set_Deferred
     (Feature     : in out Feature_Entity_Record'Class)
   is
   begin
      Feature.Property := False;
      Feature.Routine := True;
      Feature.Deferred_Feature := True;
      Feature.Effective_Class := null;
      if Feature.Value_Type /= null then
         Feature.Has_Result := True;
      end if;
   end Set_Deferred;

   ------------------------
   -- Set_Explicit_Value --
   ------------------------

   procedure Set_Explicit_Value
     (Feature : in out Feature_Entity_Record'Class;
      Value   : Node_Id)
   is
   begin
      Feature.Explicit_Value := True;
      Feature.Explicit_Value_Node := Value;
   end Set_Explicit_Value;

   ------------------
   -- Set_External --
   ------------------

   procedure Set_External
     (Feature        : in out Feature_Entity_Record'Class;
      External_Type  : String;
      External_Alias : String)
   is
      Dot_Index       : constant Natural :=
                          Ada.Strings.Fixed.Index (External_Alias, ".");
      External_Object : constant String :=
                          (if Dot_Index > 0
                           then External_Alias
                             (External_Alias'First .. Dot_Index - 1)
                           else "");
      External_Label  : constant String :=
                          External_Alias
                            (Dot_Index + 1 .. External_Alias'Last);
   begin
      Feature.Property := External_Type = "aqua_property";
      Feature.Intrinsic := External_Type = "intrinsic";
      Feature.Routine := not Feature.Property;
      Feature.External := True;
      if Feature.Value_Type /= null then
         Feature.Has_Result := True;
      end if;
      Feature.External_Object := Get_Name_Id (External_Object);
      Feature.External_Type := Get_Name_Id (External_Type);
      Feature.External_Label := Get_Name_Id (External_Label);
   end Set_External;

   ------------------------
   -- Set_Feature_Entity --
   ------------------------

   procedure Set_Feature_Entity
     (Node    : Node_Id;
      Feature : not null access Feature_Entity_Record'Class)
   is
   begin
      Set_Entity (Node, Feature);
   end Set_Feature_Entity;

   -------------------------
   -- Set_Property_Offset --
   -------------------------

   procedure Set_Property_Offset
     (Feature : in out Feature_Entity_Record'Class;
      Offset  : Word_Offset)
   is
   begin
      Feature.Property_Offset := Offset;
   end Set_Property_Offset;

   -------------------
   -- Set_Redefined --
   -------------------

   procedure Set_Redefined
     (Feature          : in out Feature_Entity_Record'Class;
      Class            : not null access
        Ack.Classes.Class_Entity_Record'Class;
      Original_Feature : not null access constant
        Feature_Entity_Record'Class)
   is
   begin
      Feature.Property := False;
      Feature.Definition_Class :=
        Original_Feature.Definition_Class;
      Feature.Original_Classes.Append
        (Original_Feature.Definition_Class);
      Feature.Redefined_Class := Class;
      Feature.Redefined_Feature :=
        Constant_Feature_Entity (Original_Feature);
   end Set_Redefined;

   ---------------------
   -- Set_Rescue_Node --
   ---------------------

   procedure Set_Rescue_Node
     (Feature : in out Feature_Entity_Record'Class;
      Node    : Node_Id)
   is
   begin
      Feature.Rescue_Node := Node;
   end Set_Rescue_Node;

   ---------------------
   -- Set_Result_Type --
   ---------------------

   procedure Set_Result_Type
     (Feature     : in out Feature_Entity_Record'Class;
      Result_Type : not null access Ack.Types.Type_Entity_Record'Class)
   is
   begin
      Feature.Value_Type := Entity_Type (Result_Type);
      if Feature.Routine or else Feature.External or else Feature.Deferred then
         Feature.Has_Result := True;
      end if;
   end Set_Result_Type;

   -----------------
   -- Set_Routine --
   -----------------

   procedure Set_Routine
     (Feature      : in out Feature_Entity_Record'Class;
      Routine_Node : Node_Id)
   is
   begin
      Feature.Property := False;
      Feature.Routine_Node := Routine_Node;
      Feature.Routine := True;
      Feature.Once := Once_Routine (Routine_Node);
      if Feature.Value_Type /= null then
         Feature.Has_Result := True;
      end if;
      Feature.Has_Current := True;
   end Set_Routine;

   ------------------------------
   -- Set_Virtual_Table_Offset --
   ------------------------------

   procedure Set_Virtual_Table_Offset
     (Feature : in out Feature_Entity_Record'Class;
      Offset  : Word_Offset)
   is
   begin
      Feature.Virtual_Table_Offset := Offset;
   end Set_Virtual_Table_Offset;

end Ack.Features;
