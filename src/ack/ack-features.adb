with Ada.Strings.Fixed;

with Tagatha.Operands;

with Ack.Classes;
with Ack.Generate;
with Ack.Types;

package body Ack.Features is

   function New_Feature
     (Name             : Name_Id;
      Declaration_Node : Node_Id;
      Class            : not null access Ack.Classes.Class_Entity_Record'Class;
      Result_Type      : access Ack.Types.Type_Entity_Record'Class;
      Routine          : Boolean := False;
      Property         : Boolean := False;
      Named_Value      : Boolean := False;
      Deferred         : Boolean := False;
      External         : Boolean := False;
      Has_Result       : Boolean := False;
      Has_Current      : Boolean := True;
      External_Type    : String := "";
      External_Alias   : String := "";
      Routine_Node     : Node_Id := No_Node)
      return Feature_Entity;

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
            Feature.Insert (Current);
         end;
      end if;

      for Argument of Feature.Arguments loop
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
            Feature.Insert (Result);
         end;
      end if;

      for Local of Feature.Locals loop
         Feature.Insert (Local);
      end loop;
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
        & " parent " & Feature.Declaration_Context.Qualified_Name;
   end Description;

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
                  Unit.Call (Label);
                  for I in 1 .. Arg_Count loop
                     Unit.Drop;
                  end loop;

                  if Feature.Has_Result then
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
                    ("get_property " & Label & ","
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
         else
            Ack.Generate.Generate_Compound
              (Unit, Compound (Effective_Routine (Feature.Routine_Node)));
         end if;

         if Feature.Has_Result then
            Unit.Push_Local (1);
            Unit.Pop_Result;
         end if;

         Unit.End_Routine;
      end if;
   end Generate_Routine;

   --------------------------
   -- New_External_Feature --
   --------------------------

   function New_External_Feature
     (Name            : Name_Id;
      Class           : not null access Ack.Classes.Class_Entity_Record'Class;
      External_Type   : String;
      External_Alias  : String;
      Result_Type     : access Ack.Types.Type_Entity_Record'Class;
      Declaration     : Node_Id)
      return Feature_Entity
   is
   begin
      return New_Feature
        (Name             => Name,
         Declaration_Node => Declaration,
         Class            => Class,
         Result_Type      => Result_Type,
         Routine          => True,
         External         => True,
         Has_Result       => Result_Type /= null,
         Has_Current      => True,
         External_Type    => External_Type,
         External_Alias   => External_Alias);
   end New_External_Feature;

   -----------------
   -- New_Feature --
   -----------------

   function New_Feature
     (Name             : Name_Id;
      Declaration_Node : Node_Id;
      Class            : not null access Ack.Classes.Class_Entity_Record'Class;
      Result_Type      : access Ack.Types.Type_Entity_Record'Class;
      Routine          : Boolean := False;
      Property         : Boolean := False;
      Named_Value      : Boolean := False;
      Deferred         : Boolean := False;
      External         : Boolean := False;
      Has_Result       : Boolean := False;
      Has_Current      : Boolean := True;
      External_Type    : String := "";
      External_Alias   : String := "";
      Routine_Node     : Node_Id := No_Node)
      return Feature_Entity
   is
      Dot_Index : constant Natural :=
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
      return Feature : constant Feature_Entity :=
        new Feature_Entity_Record'
          (Name                => +(To_Standard_String (Name)),
           Source_Name         => +(To_String (Name)),
           Declaration_Node    => Declaration_Node,
           Declaration_Context => Entity_Type (Class),
           Value_Type          => Entity_Type (Result_Type),
           Child_Map           => <>,
           Child_List          => <>,
           Parent_Environment  => Entity_Type (Class),
           Routine             => Routine,
           Property            => Property,
           Named_Value         => Named_Value,
           Deferred            => Deferred,
           External            => External,
           Has_Result          => Has_Result,
           Has_Current         => Has_Current,
           External_Object     => +External_Object,
           External_Type       => +External_Type,
           External_Label      => +External_Label,
           Original_Classes    => <>,
           Definition_Class    => Class,
           Arguments           => <>,
           Locals              => <>,
           Routine_Node        => Routine_Node)
      do
         if Class.Is_Redefinition (Name) then
            declare
               procedure Add_Original_Classes
                 (Redefined_Feature : not null access constant
                    Feature_Entity_Record'Class);

               --------------------------
               -- Add_Original_Classes --
               --------------------------

               procedure Add_Original_Classes
                 (Redefined_Feature : not null access constant
                    Feature_Entity_Record'Class)
               is
               begin
                  for Original of Redefined_Feature.Original_Classes loop
                     Feature.Original_Classes.Append (Original);
                  end loop;
               end Add_Original_Classes;

            begin
               Class.Scan_Old_Definitions (Name, Add_Original_Classes'Access);
            end;
         else
            Feature.Original_Classes.Append (Class);
         end if;

         Class.Add_Feature (Feature);
      end return;
   end New_Feature;

   --------------------------
   -- New_Property_Feature --
   --------------------------

   function New_Property_Feature
     (Name            : Name_Id;
      Class           : not null access Ack.Classes.Class_Entity_Record'Class;
      Property_Type   : not null access Ack.Types.Type_Entity_Record'Class;
      Declaration     : Node_Id)
      return Feature_Entity
   is
   begin
      return New_Feature
        (Name             => Name,
         Declaration_Node => Declaration,
         Class            => Class,
         Result_Type      => Property_Type,
         Property         => True,
         Has_Result       => False,
         Has_Current      => False);
   end New_Property_Feature;

   -------------------------
   -- New_Routine_Feature --
   -------------------------

   function New_Routine_Feature
     (Name            : Name_Id;
      Class           : not null access Ack.Classes.Class_Entity_Record'Class;
      Result_Type     : access Ack.Types.Type_Entity_Record'Class;
      Deferred        : Boolean;
      Declaration     : Node_Id;
      Routine         : Node_Id)
      return Feature_Entity
   is
   begin
      return New_Feature
        (Name             => Name,
         Declaration_Node => Declaration,
         Class            => Class,
         Result_Type      => Result_Type,
         Deferred         => Deferred,
         Routine          => True,
         Has_Result       => Result_Type /= null,
         Has_Current      => True,
         Routine_Node     => Routine);
   end New_Routine_Feature;

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
      for Class of Feature.Original_Classes loop
         Process (Ack.Classes.Class_Entity (Class));
      end loop;
   end Scan_Original_Classes;

   -----------------------
   -- Set_Default_Value --
   -----------------------

   procedure Set_Default_Value
     (Feature : Feature_Entity_Record;
      Unit    : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      for Class of Feature.Original_Classes loop
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
      end loop;
   end Set_Default_Value;

end Ack.Features;
