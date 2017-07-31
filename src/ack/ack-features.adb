with Ada.Strings.Fixed;

with Tagatha.Operands;

with Ack.Classes;
with Ack.Generate;
with Ack.Types;

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

   ----------
   -- Bind --
   ----------

   overriding procedure Bind
     (Feature : in out Feature_Entity_Record)
   is
      Next_Argument : Positive := 1;
      Next_Local    : Positive := 1;
   begin
      if Feature.Has_Current then
         declare
            Current : constant Ack.Variables.Variable_Entity :=
                        Ack.Variables.New_Argument_Entity
                          (Get_Name_Id ("Current"),
                           Feature.Declaration_Node,
                           Ack.Types.New_Class_Type
                             (Feature.Declaration_Node,
                              Feature.Definition_Class,
                              Detachable => False));
         begin
            Current.Set_Attached;
            Current.Set_Offset (1);
            Feature.Insert (Current);
            Next_Argument := 2;
         end;
      end if;

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

   ----------------------
   -- Definition_Class --
   ----------------------

   function Definition_Class
     (Feature : Feature_Entity_Record'Class)
      return access constant Ack.Classes.Class_Entity_Record'Class
   is
   begin
      return Feature.Definition_Class;
   end Definition_Class;

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
      Unit    : in out Tagatha.Units.Tagatha_Unit)
   is
      Arg_Count    : constant Natural :=
                       1 + Natural (Feature.Arguments.Length);
      Frame_Count  : constant Natural :=
                       Natural (Feature.Locals.Length);
      Result_Count : constant Natural :=
                       (if Feature.Value_Type /= null
                        then 1 else 0);
   begin
      if Feature.Routine then
         Unit.Begin_Routine
           (Name           => Feature.Link_Name,
            Argument_Words => Arg_Count,
            Frame_Words    => Frame_Count,
            Result_Words   => Result_Count,
            Global         => True);

         if Feature.External then
            declare
               Object_Name : constant String := -Feature.External_Object;
               Label       : constant String := -Feature.External_Label;
            begin
               if Object_Name = "" then
                  for I in reverse 1 .. Arg_Count loop
                     Unit.Push_Argument
                       (Tagatha.Argument_Offset (I));
                  end loop;

                  Unit.Pop_Register ("op");
                  Unit.Native_Operation
                    ("get_property " & Label & ","
                     & Natural'Image (Arg_Count - 1),
                     Input_Stack_Words  => 0,
                     Output_Stack_Words => 0,
                     Changed_Registers  => "pv");

                  if Feature.Has_Result then
                     Unit.Push_Register ("pv");
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
                    ("get_property " & Label & ","
                     & Natural'Image (Arg_Count - 1),
                     Input_Stack_Words  => 0,
                     Output_Stack_Words => 0,
                     Changed_Registers  => "pv");

                  Unit.Push_Register ("pv");

               end if;
            end;
         else
            for I in 1 .. Feature.Local_Count loop
               Unit.Push (0);
            end loop;
            Ack.Generate.Generate_Compound
              (Unit, Compound (Feature.Routine_Node));
         end if;

         if Feature.Has_Result then
            Unit.Push_Local (1);
            Unit.Pop_Result;
         end if;

         Unit.End_Routine;
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

   -----------------
   -- New_Feature --
   -----------------

   function New_Feature
     (Name        : Name_Id;
      Alias       : Name_Id;
      Declaration : Node_Id;
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
         Feature.Definition_Class := Class;
         Feature.Property := True;
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
      Original_Class : constant Ack.Classes.Class_Entity :=
                         (if Feature.Original_Classes.Is_Empty
                          then Feature.Definition_Class
                          else Ack.Classes.Class_Entity
                            (Feature.Original_Classes.First_Element));
   begin
      Unit.Push_Argument (1);
      Unit.Pop_Register ("op");
      Unit.Native_Operation
        ("get_property " & Original_Class.Link_Name & ",0",
         Input_Stack_Words  => 0,
         Output_Stack_Words => 0,
         Changed_Registers  => "pv");
      Unit.Push_Register ("pv");
      Unit.Pop_Register ("op");
      Unit.Pop_Register ("pv");
      Unit.Native_Operation
        ("set_property " & Feature.Standard_Name);
   end Pop_Entity;

   -----------------
   -- Push_Entity --
   -----------------

   overriding procedure Push_Entity
     (Feature      : Feature_Entity_Record;
      Have_Context : Boolean;
      Unit         : in out Tagatha.Units.Tagatha_Unit)
   is
      Original_Class : constant Ack.Classes.Class_Entity :=
                         (if Feature.Original_Classes.Is_Empty
                          then Feature.Definition_Class
                          else Ack.Classes.Class_Entity
                            (Feature.Original_Classes.First_Element));
   begin
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
         end case;
         return;
      end if;

      if not Have_Context then
         Unit.Push_Argument (1);
      end if;

      Unit.Pop_Register ("op");

      if Feature.Definition_Class.Standard_Name = "string" then
         Unit.Native_Operation
           ("get_property " & Feature.Standard_Name & ","
            & Natural'Image (Feature.Argument_Count),
            Input_Stack_Words  => 0,
            Output_Stack_Words => 0,
            Changed_Registers  => "pv");

         if Feature.Has_Result then
            Unit.Push_Register ("pv");
         end if;

         return;
      end if;

      if Feature.Routine
        or else (Feature.Property
                 and then not Feature.Get_Type.Detachable
                 and then not Feature.Attached)
      then
         Unit.Push_Register ("op");
      end if;

      Unit.Native_Operation
        ("get_property "
         & Original_Class.Link_Name & ",0",
         Input_Stack_Words  => 0,
         Output_Stack_Words => 0,
         Changed_Registers  => "pv");
      Unit.Push_Register ("pv");
      Unit.Pop_Register ("op");
      Unit.Native_Operation
        ("get_property " & Feature.Standard_Name & ",0",
         Input_Stack_Words  => 0,
         Output_Stack_Words => 0,
         Changed_Registers  => "pv");

      if Feature.Routine then
         Unit.Push_Register ("pv");
         Unit.Indirect_Call;
         Unit.Drop;
         for I in 1 .. Feature.Argument_Count loop
            Unit.Drop;
         end loop;
         Unit.Push_Register ("r0");
      elsif Feature.Property then
         if not Feature.Get_Type.Detachable
           and then not Feature.Attached
         then

            declare
               Continue_Label : constant Positive :=
                                  Unit.Next_Label;
            begin
               Unit.Push_Register ("pv");
               Unit.Operate (Tagatha.Op_Test, Tagatha.Default_Size);
               Unit.Jump (Continue_Label, Tagatha.C_Not_Equal);
               Feature.Get_Type.Allocate (Unit);
               Unit.Pop_Register ("pv");
               Unit.Pop_Register ("op");
               Unit.Push_Register ("op");

               declare
                  procedure Set_Value
                    (Class : not null access constant
                       Ack.Classes.Class_Entity_Record'Class);

                  ---------------
                  -- Set_Value --
                  ---------------

                  procedure Set_Value
                    (Class : not null access constant
                       Ack.Classes.Class_Entity_Record'Class)
                  is
                  begin
                     Unit.Push_Register ("pv");
                     Unit.Native_Operation
                       ("get_property "
                        & Class.Link_Name & ",0",
                        Input_Stack_Words  => 0,
                        Output_Stack_Words => 0,
                        Changed_Registers  => "pv");
                     Unit.Push_Register ("pv");
                     Unit.Pop_Register ("op");
                     Unit.Pop_Register ("pv");
                     Unit.Native_Operation
                       ("set_property " & Feature.Standard_Name);
                  end Set_Value;

               begin
                  Feature.Scan_Original_Classes
                    (Set_Value'Access);
               end;

               Unit.Label (Continue_Label);
               Unit.Drop;
            end;
         end if;

--         Unit.Drop;
         Unit.Push_Register ("pv");
      end if;

   end Push_Entity;

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
         if Feature.Definition_Class = null then
            --  deferred
            null;
         else
            Process (Feature.Definition_Class);
         end if;
      else
         for Class of Feature.Original_Classes loop
            Process (Ack.Classes.Class_Entity (Class));
         end loop;
      end if;
   end Scan_Original_Classes;

   -----------------------
   -- Set_Default_Value --
   -----------------------

   procedure Set_Default_Value
     (Feature : Feature_Entity_Record;
      Unit    : in out Tagatha.Units.Tagatha_Unit)
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

         if Feature.Property then
            Unit.Push (0);
            Unit.Pop_Register ("pv");
            Unit.Native_Operation
              ("set_property " & Feature.Standard_Name);
         elsif Feature.Routine then
            Unit.Push_Operand
              (Tagatha.Operands.External_Operand
                 (Feature.Definition_Class.Link_Name
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
      Feature.Deferred := True;
      Feature.Definition_Class := null;
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
      Feature.Property := False;
      Feature.Routine := True;
      Feature.External := True;
      if Feature.Value_Type /= null then
         Feature.Has_Result := True;
      end if;
      Feature.External_Object := +External_Object;
      Feature.External_Type := +External_Type;
      Feature.External_Label := +External_Label;
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

   -------------------
   -- Set_Redefined --
   -------------------

   procedure Set_Redefined
     (Feature     : in out Feature_Entity_Record'Class;
      Original    : not null access Ack.Classes.Class_Entity_Record'Class)
   is
   begin
      Feature.Original_Classes.Append (Original);
   end Set_Redefined;

   ---------------------
   -- Set_Result_Type --
   ---------------------

   procedure Set_Result_Type
     (Feature     : in out Feature_Entity_Record'Class;
      Result_Type : not null access Ack.Types.Type_Entity_Record'Class)
   is
   begin
      Feature.Value_Type := Entity_Type (Result_Type);
      if Feature.Routine or else Feature.External then
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
      if Feature.Value_Type /= null then
         Feature.Has_Result := True;
      end if;
      Feature.Has_Current := True;
   end Set_Routine;

end Ack.Features;
