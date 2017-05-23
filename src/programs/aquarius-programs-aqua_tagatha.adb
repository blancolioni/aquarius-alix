with Ada.Characters.Handling;

with Aqua.Primitives;
with Tagatha.Operands;
with Tagatha.Units;
with Tagatha.Units.Listing;

package body Aquarius.Programs.Aqua_Tagatha is

   Current_Unit : Tagatha.Units.Tagatha_Unit;

   type Root_Tree_Handler is
     abstract new Aqua.Primitives.Handler_Interface with null record;

   function Get_Tree
     (Handler   : Root_Tree_Handler'Class;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Program_Tree;

   type Procedure_Tree is access
     procedure (Tree : Program_Tree);

   type Handle_Procedure_Tree is
     new Root_Tree_Handler with
      record
         Handler : Procedure_Tree;
      end record;

   overriding function Handle
     (Primitive : Handle_Procedure_Tree;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   type Procedure_Tree_Tree is access
     procedure (P1, P2 : Program_Tree);

   type Handle_Procedure_Tree_Tree is
     new Aqua.Primitives.Handler_Interface with
      record
         Handler : Procedure_Tree_Tree;
      end record;

   overriding function Handle
     (Primitive : Handle_Procedure_Tree_Tree;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   type Procedure_Tree_Integer is access
     procedure (Tree  : Program_Tree;
                Value : Tagatha.Tagatha_Integer);

   type Handle_Procedure_Tree_Integer is
     new Aqua.Primitives.Handler_Interface with
      record
         Handler : Procedure_Tree_Integer;
      end record;

   overriding function Handle
     (Primitive : Handle_Procedure_Tree_Integer;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   type Procedure_Tree_String is access
     procedure (Tree  : Program_Tree;
                Value : String);

   type Handle_Procedure_Tree_String is
     new Root_Tree_Handler with
      record
         Handler : Procedure_Tree_String;
      end record;

   overriding function Handle
     (Primitive : Handle_Procedure_Tree_String;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   type Procedure_Tree_String_Boolean is access
     procedure (Tree  : Program_Tree;
                Text  : String;
                Flag  : Boolean);

   type Handle_Procedure_Tree_String_Boolean is
     new Root_Tree_Handler with
      record
         Handler : Procedure_Tree_String_Boolean;
      end record;

   overriding function Handle
     (Primitive : Handle_Procedure_Tree_String_Boolean;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   procedure Tagatha_Integer_Constant
     (Tree  : Program_Tree;
      Value : Tagatha.Tagatha_Integer);

   procedure Tagatha_String_Constant
     (Tree  : Program_Tree;
      Label : String);

   procedure Tagatha_Frame_Offset
     (Tree  : Program_Tree;
      Value : Tagatha.Tagatha_Integer);

   procedure Tagatha_Operator
     (Tree     : Program_Tree;
      Operator : String);

   procedure Tagatha_Pop
     (Tree  : Program_Tree);

   procedure Tagatha_Pop_Register
     (Tree : Program_Tree;
      Name : String);

   procedure Tagatha_Push
     (Tree  : Program_Tree);

   procedure Tagatha_Push_External
     (Tree      : Program_Tree;
      Name      : String;
      Immediate : Boolean);

   procedure Tagatha_Push_Register
     (Tree : Program_Tree;
      Name : String);

   procedure Tagatha_Join_Fragment
     (Parent, Child : Program_Tree);

   function Handle_Native_Operation
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word;

   ------------------
   -- Add_Handlers --
   ------------------

   procedure Add_Handlers is
   begin
      Aqua.Primitives.New_Primitive_Handler
        (Name           => "tree__integer_constant",
         Argument_Count => 2,
         Handler        =>
           Handle_Procedure_Tree_Integer'
             (Handler => Tagatha_Integer_Constant'Access));

      Aqua.Primitives.New_Primitive_Handler
        (Name           => "tree__string_constant",
         Argument_Count => 2,
         Handler        =>
           Handle_Procedure_Tree_String'
             (Handler => Tagatha_String_Constant'Access));

      Aqua.Primitives.New_Primitive_Handler
        (Name           => "tree__operator",
         Argument_Count => 2,
         Handler        =>
           Handle_Procedure_Tree_String'
             (Handler => Tagatha_Operator'Access));

      Aqua.Primitives.New_Primitive_Handler
        (Name           => "tree__frame_offset",
         Argument_Count => 2,
         Handler        =>
           Handle_Procedure_Tree_Integer'
             (Handler => Tagatha_Frame_Offset'Access));

      Aqua.Primitives.New_Primitive_Handler
        (Name           => "tree__pop",
         Argument_Count => 1,
         Handler        =>
           Handle_Procedure_Tree'
             (Handler => Tagatha_Pop'Access));

      Aqua.Primitives.New_Primitive_Handler
        (Name           => "tree__pop_register",
         Argument_Count => 2,
         Handler        =>
           Handle_Procedure_Tree_String'
             (Handler => Tagatha_Pop_Register'Access));

      Aqua.Primitives.New_Primitive_Handler
        (Name           => "tree__push",
         Argument_Count => 1,
         Handler        =>
           Handle_Procedure_Tree'
             (Handler => Tagatha_Push'Access));

      Aqua.Primitives.New_Primitive_Handler
        (Name           => "tree__push_external",
         Argument_Count => 3,
         Handler        =>
           Handle_Procedure_Tree_String_Boolean'
             (Handler => Tagatha_Push_External'Access));

      Aqua.Primitives.New_Primitive_Handler
        (Name           => "tree__push_register",
         Argument_Count => 2,
         Handler        =>
           Handle_Procedure_Tree_String'
             (Handler => Tagatha_Push_Register'Access));

      Aqua.Primitives.New_Primitive_Handler
        (Name           => "tree__join_fragment",
         Argument_Count => 2,
         Handler        =>
           Handle_Procedure_Tree_Tree'
             (Handler => Tagatha_Join_Fragment'Access));

      Aqua.Primitives.New_Primitive_Function
        (Name           => "tree__branch",
         Argument_Count => 3,
         Handler        => Tagatha_Branch'Access);

      Aqua.Primitives.New_Primitive_Function
        (Name           => "tree__native_operation",
         Argument_Count => 5,
         Handler        => Handle_Native_Operation'Access);

   end Add_Handlers;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree
     (Handler   : Root_Tree_Handler'Class;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Program_Tree
   is
      pragma Unreferenced (Handler);
   begin
      declare
         Tree : constant Program_Tree :=
           Program_Tree (Context.To_External_Object (Arguments (1)));
      begin
         return Tree;
      end;
   exception
      when others =>
         raise Constraint_Error with
           "first argument must be a tree; found "
           & Context.To_String (Arguments (1));
   end Get_Tree;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Primitive : Handle_Procedure_Tree;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
   begin

      Primitive.Handler (Primitive.Get_Tree (Context, Arguments));
      return 0;

   end Handle;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Primitive : Handle_Procedure_Tree_Tree;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      P1, P2 : Program_Tree;
   begin

      begin
         P1 := Program_Tree (Context.To_External_Object (Arguments (1)));
      exception
         when others =>
            raise Constraint_Error with
              "first argument must be a tree; found "
              & Context.To_String (Arguments (1));
      end;

      begin
         P2 := Program_Tree (Context.To_External_Object (Arguments (2)));
      exception
         when others =>
            raise Constraint_Error with
              "second argument must be a tree; found "
              & Context.To_String (Arguments (2));
      end;

      Primitive.Handler (P1, P2);
      return 0;

   end Handle;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Primitive : Handle_Procedure_Tree_Integer;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Tree : Program_Tree;
      Value : Tagatha.Tagatha_Integer;
   begin

      begin
         Tree := Program_Tree (Context.To_External_Object (Arguments (1)));
      exception
         when others =>
            raise Constraint_Error with
              "first argument must be a tree; found "
              & Context.To_String (Arguments (1));
      end;

      declare
         use Aqua;
         Arg : constant Word := Arguments (2);
         X   : Aqua_Integer;
      begin
         if Is_String_Reference (Arg) then
            X := Aqua_Integer'Value (Context.To_String (Arg));
         else
            X := Get_Integer (Arg);
         end if;

         Value := Tagatha.Tagatha_Integer (X);

      exception
         when others =>
            raise Constraint_Error with
              "second argument must be an integer; found "
              & Context.To_String (Arguments (2));
      end;

      Primitive.Handler (Tree, Value);

      return 0;

   end Handle;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Primitive : Handle_Procedure_Tree_String;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Tree : constant Program_Tree := Primitive.Get_Tree (Context, Arguments);
   begin

      if Aqua.Is_String_Reference (Arguments (2)) then
         declare
            S   : constant String :=
                    Context.To_String (Arguments (2));
         begin
            Primitive.Handler (Tree, S);
         end;
      else
         raise Constraint_Error with
           "second argument must be a string; found "
           & Context.To_String (Arguments (2));
      end if;

      return 0;

   end Handle;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Primitive : Handle_Procedure_Tree_String_Boolean;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Tree : constant Program_Tree := Primitive.Get_Tree (Context, Arguments);
   begin

      if Aqua.Is_String_Reference (Arguments (2)) then
         declare
            use type Aqua.Aqua_Integer;
            S   : constant String :=
                    Context.To_String (Arguments (2));
         begin
            Primitive.Handler (Tree, S,
                               Context.To_Integer (Arguments (3)) /= 0);
         end;
      else
         raise Constraint_Error with
           "second argument must be a string; found "
           & Context.To_String (Arguments (2));
      end if;

      return 0;

   end Handle;

   -----------------------------
   -- Handle_Native_Operation --
   -----------------------------

   function Handle_Native_Operation
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Op : constant String := Context.To_String (Arguments (2));
      Inputs : constant Natural :=
                 Natural (Context.To_Integer (Arguments (3)));
      Outputs : constant Natural :=
                  Natural (Context.To_Integer (Arguments (4)));
      Changed : constant String :=
                  Context.To_String (Arguments (5));
   begin
      Current_Unit.Native_Operation
        (Name               => Op,
         Input_Stack_Words  => Inputs,
         Output_Stack_Words => Outputs,
         Changed_Registers  => Changed);
      return 0;
   end Handle_Native_Operation;

   ----------------------
   -- Tagatha_Allocate --
   ----------------------

   function Tagatha_Allocate
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      Word_Count : constant Natural :=
                     Natural (Aqua.Get_Integer (Arguments (2)));
   begin
      Current_Unit.Push (Tagatha.Tagatha_Integer (Word_Count));
      Current_Unit.Call ("tagatha__allocate");
      return 0;
   end Tagatha_Allocate;

   ----------------------------
   -- Tagatha_Apply_Fragment --
   ----------------------------

   function Tagatha_Apply_Fragment
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Tree : constant Program_Tree :=
               Program_Tree (Context.To_External_Object (Arguments (1)));
   begin
      Tagatha.Fragments.Append_To_Unit (Current_Unit, Tree.Fragment);
      return 0;
   end Tagatha_Apply_Fragment;

   -----------------------------
   -- Tagatha_Begin_Procedure --
   -----------------------------

   function Tagatha_Begin_Procedure
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Name : constant String := Context.To_String (Arguments (2));
      Arg_Count : constant Natural :=
                    Natural (Aqua.Get_Integer (Arguments (3)));
      Frame_Count : constant Natural :=
                      Natural (Aqua.Get_Integer (Arguments (4)));
      Result_Count : constant Natural :=
                       Natural (Aqua.Get_Integer (Arguments (5)));
   begin
      Current_Unit.Begin_Routine
        (Name, Arg_Count, Frame_Count, Result_Count, True);
      return 0;
   end Tagatha_Begin_Procedure;

   ------------------------
   -- Tagatha_Begin_Unit --
   ------------------------

   function Tagatha_Begin_Unit
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Program : constant Program_Tree :=
                  Program_Tree (Context.To_External_Object (Arguments (1)));
      Unit_Name : constant String := Context.To_String (Arguments (2));
   begin
      Current_Unit.Create_Unit
        (Name           => Unit_Name,
         Source_File    =>
           Aquarius.Names.To_String (Program.Source_File_Name));
      return 0;
   end Tagatha_Begin_Unit;

   --------------------
   -- Tagatha_Branch --
   --------------------

   function Tagatha_Branch
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Tree : constant Program_Tree :=
               Program_Tree (Context.To_External_Object (Arguments (1)));
      Conditional : constant Boolean := Arguments'Length > 2;
      Condition_Word : constant Aqua.Word :=
                         (if Conditional
                          then Arguments (2)
                          else 0);
      Label_Word     : constant Aqua.Word :=
                         (if Conditional
                          then Arguments (3)
                          else Arguments (2));

      function Condition return Tagatha.Tagatha_Condition;

      ---------------
      -- Condition --
      ---------------

      function Condition return Tagatha.Tagatha_Condition is
         Text : constant String :=
                  Ada.Characters.Handling.To_Lower
                    (Context.To_String (Condition_Word));
      begin
         if Text = "eq" or else Text = "equal" then
            return Tagatha.C_Equal;
         elsif Text = "ne" or else Text = "not_equal" then
            return Tagatha.C_Not_Equal;
         else
            return Tagatha.C_Always;
         end if;
      end Condition;

   begin

      if Conditional then
         Tagatha.Fragments.Append
           (Tree.Fragment, Tagatha.Fragments.Condition (Condition));
      end if;

      Tagatha.Fragments.Append
        (Tree.Fragment,
         Tagatha.Fragments.Branch
           (Positive (Aqua.Get_Integer (Label_Word)),
            Conditional));
      return 1;
   end Tagatha_Branch;

   --------------------------
   -- Tagatha_Code_Segment --
   --------------------------

   function Tagatha_Code_Segment
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      Current_Unit.Segment (Tagatha.Executable);
      return 0;
   end Tagatha_Code_Segment;

   -----------------------
   -- Tagatha_Code_Unit --
   -----------------------

   function Tagatha_Code_Unit
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Arch : constant String :=
               Context.To_String (Arguments (2));
   begin
      Tagatha.Units.Listing.Write_Command_Listing
        (Current_Unit);
      Current_Unit.Write (Arch, ".");
      return 1;
   exception
      when others =>
         return 0;
   end Tagatha_Code_Unit;

   ------------------
   -- Tagatha_Data --
   ------------------

   function Tagatha_Data
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      use Aqua;
      Data : constant Word := Arguments (2);
   begin
      if Is_String_Reference (Data) then
         Current_Unit.Data
           (Context.To_String (Data));
      else
         Current_Unit.Data
           (Tagatha.Tagatha_Integer
              (Get_Integer (Data)));
      end if;
      return 0;
   end Tagatha_Data;

   ---------------------------
   -- Tagatha_End_Procedure --
   ---------------------------

   function Tagatha_End_Procedure
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      Current_Unit.End_Routine;
      return 1;
   end Tagatha_End_Procedure;

   ----------------------
   -- Tagatha_End_Unit --
   ----------------------

   function Tagatha_End_Unit
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      Current_Unit.Finish_Unit;
      return 0;
   end Tagatha_End_Unit;

   --------------------------
   -- Tagatha_Frame_Offset --
   --------------------------

   procedure Tagatha_Frame_Offset
     (Tree  : Program_Tree;
      Value : Tagatha.Tagatha_Integer)
   is
   begin
      Tagatha.Fragments.Append
        (Tree.Fragment,
         Tagatha.Fragments.Reference_Local
           (Tagatha.Local_Offset (Value)));
   end Tagatha_Frame_Offset;

   ------------------------------
   -- Tagatha_Integer_Constant --
   ------------------------------

   procedure Tagatha_Integer_Constant
     (Tree  : Program_Tree;
      Value : Tagatha.Tagatha_Integer)
   is
   begin
      Tagatha.Fragments.Append
        (Tree.Fragment,
         Tagatha.Fragments.Integer_Constant (Value));
   end Tagatha_Integer_Constant;

   ---------------------------
   -- Tagatha_Join_Fragment --
   ---------------------------

   procedure Tagatha_Join_Fragment
     (Parent, Child : Program_Tree)
   is
   begin
      Tagatha.Fragments.Check_Source_Location
        (Child.Fragment,
         Positive (Parent.Location_Line),
         Positive (Parent.Location_Column));
      Tagatha.Fragments.Append (Parent.Fragment, Child.Fragment);
   end Tagatha_Join_Fragment;

   -------------------
   -- Tagatha_Label --
   -------------------

   function Tagatha_Label
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Tree : constant Program_Tree :=
               Program_Tree (Context.To_External_Object (Arguments (1)));
   begin
      if Aqua.Is_Integer (Arguments (2)) then
         Tagatha.Fragments.Append
             (Tree.Fragment,
              Tagatha.Fragments.Label
                (Positive (Aqua.Get_Integer (Arguments (2)))));
      else
         Current_Unit.Label
           (Context.To_String (Arguments (2)));
      end if;
      return 0;
   end Tagatha_Label;

   ----------------------
   -- Tagatha_Operator --
   ----------------------

   procedure Tagatha_Operator
     (Tree     : Program_Tree;
      Operator : String)
   is
      use Tagatha;
      Op : Tagatha.Tagatha_Operator := Op_Nop;
   begin
      if Operator = "*" then
         Op := Op_Mul;
      elsif Operator = "+" then
         Op := Op_Add;
      elsif Operator = "-" then
         Op := Op_Sub;
      elsif Operator = "/" then
         Op := Op_Div;
      elsif Operator = "mod" then
         Op := Op_Mod;
      elsif Operator = "=" then
         Op := Op_Compare;
      end if;

      Tagatha.Fragments.Append
        (Tree.Fragment,
         Tagatha.Fragments.Operator (Op));

   end Tagatha_Operator;

   -----------------
   -- Tagatha_Pop --
   -----------------

   procedure Tagatha_Pop
     (Tree  : Program_Tree)
   is
   begin
      Tagatha.Fragments.Append
        (Tree.Fragment,
         Tagatha.Fragments.Pop);
   end Tagatha_Pop;

   --------------------------
   -- Tagatha_Pop_Register --
   --------------------------

   procedure Tagatha_Pop_Register
     (Tree : Program_Tree;
      Name : String)
   is
      pragma Unreferenced (Tree);
   begin
      Current_Unit.Pop_Register (Name);
   end Tagatha_Pop_Register;

   ------------------
   -- Tagatha_Push --
   ------------------

   procedure Tagatha_Push
     (Tree  : Program_Tree)
   is
   begin
      Tagatha.Fragments.Append
        (Tree.Fragment,
         Tagatha.Fragments.Push);
   end Tagatha_Push;

   ---------------------------
   -- Tagatha_Push_External --
   ---------------------------

   procedure Tagatha_Push_External
     (Tree      : Program_Tree;
      Name      : String;
      Immediate : Boolean)
   is
      pragma Unreferenced (Tree);
   begin
      Current_Unit.Push_Operand
        (Tagatha.Operands.External_Operand
           (Name      => Name,
            Immediate => Immediate),
         Size => Tagatha.Default_Size);
   end Tagatha_Push_External;

   ---------------------------
   -- Tagatha_Push_Register --
   ---------------------------

   procedure Tagatha_Push_Register
     (Tree : Program_Tree;
      Name : String)
   is
      pragma Unreferenced (Tree);
   begin
      Current_Unit.Push_Register (Name);
   end Tagatha_Push_Register;

   --------------------
   -- Tagatha_Return --
   --------------------

   function Tagatha_Return
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      Current_Unit.End_Routine;
      Current_Unit.Optimise;
      return 0;
   end Tagatha_Return;

   -----------------------------
   -- Tagatha_String_Constant --
   -----------------------------

   procedure Tagatha_String_Constant
     (Tree  : Program_Tree;
      Label : String)
   is
   begin
      Tagatha.Fragments.Append
        (Tree.Fragment,
         Tagatha.Fragments.Reference_External (Label, True));
   end Tagatha_String_Constant;

   --------------------------
   -- Tagatha_Text_Segment --
   --------------------------

   function Tagatha_Text_Segment
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      Current_Unit.Segment (Tagatha.Read_Only);
      return 0;
   end Tagatha_Text_Segment;

end Aquarius.Programs.Aqua_Tagatha;
