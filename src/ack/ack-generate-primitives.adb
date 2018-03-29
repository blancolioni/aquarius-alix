package body Ack.Generate.Primitives is

   type Primitive_Operator_Generator is access
     procedure (Unit : in out Tagatha.Units.Tagatha_Unit);

   type Primitive_Operator_Record is
      record
         Operator : Name_Id;
         Left_Type : Ack.Types.Type_Entity;
         Generator : Primitive_Operator_Generator;
      end record;

   package Primitive_Operator_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Primitive_Operator_Record);

   Primitive_Operators : Primitive_Operator_Lists.List;

   procedure Create_Primitives;

   procedure Generate_Equal (Unit : in out Tagatha.Units.Tagatha_Unit);
   procedure Generate_Not_Equal (Unit : in out Tagatha.Units.Tagatha_Unit);

   procedure Generate_GE (Unit : in out Tagatha.Units.Tagatha_Unit);
   procedure Generate_GT (Unit : in out Tagatha.Units.Tagatha_Unit);
   procedure Generate_LE (Unit : in out Tagatha.Units.Tagatha_Unit);
   procedure Generate_LT (Unit : in out Tagatha.Units.Tagatha_Unit);

   procedure Generate_Not (Unit : in out Tagatha.Units.Tagatha_Unit);
   procedure Generate_And (Unit : in out Tagatha.Units.Tagatha_Unit);
   procedure Generate_Or (Unit : in out Tagatha.Units.Tagatha_Unit);
   procedure Generate_Xor (Unit : in out Tagatha.Units.Tagatha_Unit);
   procedure Generate_Implies (Unit : in out Tagatha.Units.Tagatha_Unit);

   procedure Generate_Add (Unit : in out Tagatha.Units.Tagatha_Unit);
   procedure Generate_Join (Unit : in out Tagatha.Units.Tagatha_Unit);

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is

      Any_Type : constant Ack.Types.Type_Entity :=
                   Ack.Types.Get_Top_Level_Type ("any");
      Boolean_Type : constant Ack.Types.Type_Entity :=
                       Ack.Types.Get_Top_Level_Type ("boolean");
      String_Type : constant Ack.Types.Type_Entity :=
                   Ack.Types.Get_Top_Level_Type ("string");
      Integer_Type  : constant Ack.Types.Type_Entity :=
                        Ack.Types.Get_Top_Level_Type ("integer");

      procedure Add
        (Name      : String;
         Left_Type : Ack.Types.Type_Entity;
         Generator : Primitive_Operator_Generator);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name      : String;
         Left_Type : Ack.Types.Type_Entity;
         Generator : Primitive_Operator_Generator)
      is
      begin
         Primitive_Operators.Append
           (Primitive_Operator_Record'
              (Operator  => Get_Name_Id (Name),
               Left_Type => Left_Type,
               Generator => Generator));
      end Add;

   begin
      Add ("=", Any_Type, Generate_Equal'Access);
      Add ("/=", Any_Type, Generate_Not_Equal'Access);

      Add (">", Integer_Type, Generate_GT'Access);
      Add ("<", Integer_Type, Generate_LT'Access);
      Add (">=", Integer_Type, Generate_GE'Access);
      Add ("<=", Integer_Type, Generate_LE'Access);

      Add ("not", Boolean_Type, Generate_Not'Access);
      Add ("and", Boolean_Type, Generate_And'Access);
      Add ("or", Boolean_Type, Generate_Or'Access);
      Add ("xor", Boolean_Type, Generate_Xor'Access);
      Add ("implies", Boolean_Type, Generate_Implies'Access);

      Add ("&", String_Type, Generate_Join'Access);
      Add ("+", Integer_Type, Generate_Add'Access);
   end Create_Primitives;

   ------------------
   -- Generate_Add --
   ------------------

   procedure Generate_Add (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Add);
   end Generate_Add;

   ------------------
   -- Generate_And --
   ------------------

   procedure Generate_And (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_And);
   end Generate_And;

   --------------------
   -- Generate_Equal --
   --------------------

   procedure Generate_Equal (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Equal);
   end Generate_Equal;

   -----------------
   -- Generate_GE --
   -----------------

   procedure Generate_GE (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Greater_Equal);
   end Generate_GE;

   -----------------
   -- Generate_GT --
   -----------------

   procedure Generate_GT (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Greater);
   end Generate_GT;

   ----------------------
   -- Generate_Implies --
   ----------------------

   procedure Generate_Implies (Unit : in out Tagatha.Units.Tagatha_Unit) is
      False_Label : constant Positive := Unit.Next_Label;
      Out_Label : constant Positive := Unit.Next_Label;
   begin
      Unit.Operate (Tagatha.Op_Test);
      Unit.Jump (False_Label, Tagatha.C_Equal);
      Unit.Drop;
      Unit.Push (1);
      Unit.Jump (Out_Label, Tagatha.C_Always);
      Unit.Label (False_Label);
      Unit.Operate (Tagatha.Op_Not);
      Unit.Label (Out_Label);
   end Generate_Implies;

   -------------------
   -- Generate_Join --
   -------------------

   procedure Generate_Join (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Add);
   end Generate_Join;

   -----------------
   -- Generate_LE --
   -----------------

   procedure Generate_LE (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Less_Equal);
   end Generate_LE;

   -----------------
   -- Generate_LT --
   -----------------

   procedure Generate_LT (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Less);
   end Generate_LT;

   ------------------
   -- Generate_Not --
   ------------------

   procedure Generate_Not (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Not);
   end Generate_Not;

   ------------------------
   -- Generate_Not_Equal --
   ------------------------

   procedure Generate_Not_Equal (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Not_Equal);
   end Generate_Not_Equal;

   -----------------------
   -- Generate_Operator --
   -----------------------

   function Generate_Operator
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Operator  : Name_Id;
      Left_Type : Ack.Types.Type_Entity)
      return Boolean
   is
   begin
      if Primitive_Operators.Is_Empty then
         Create_Primitives;
      end if;

      for Rec of Primitive_Operators loop
         if Rec.Operator = Operator
           and then Left_Type.Conforms_To (Rec.Left_Type)
         then
            Rec.Generator (Unit);
            return True;
         end if;
      end loop;
      return False;
   end Generate_Operator;

   -----------------
   -- Generate_Or --
   -----------------

   procedure Generate_Or (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Or);
   end Generate_Or;

   ------------------
   -- Generate_Xor --
   ------------------

   procedure Generate_Xor (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Xor);
   end Generate_Xor;

end Ack.Generate.Primitives;
