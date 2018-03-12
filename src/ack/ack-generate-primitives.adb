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
   procedure Generate_Add (Unit : in out Tagatha.Units.Tagatha_Unit);
   procedure Generate_Join (Unit : in out Tagatha.Units.Tagatha_Unit);

   -----------------------
   -- Create_Primitives --
   -----------------------

   procedure Create_Primitives is

      Any_Type : constant Ack.Types.Type_Entity :=
                   Ack.Types.Get_Top_Level_Type ("any");
      Boolean_Type : constant Ack.Types.Type_Entity :=
                       Ack.Types.Get_Top_Level_Type ("boolean")
                       with Unreferenced;
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

   --------------------
   -- Generate_Equal --
   --------------------

   procedure Generate_Equal (Unit : in out Tagatha.Units.Tagatha_Unit) is
      Label : constant Positive := Unit.Next_Label;
   begin
      Unit.Push (0);
      Unit.Pop_Register ("r0");
      Unit.Operate (Tagatha.Op_Compare);
      Unit.Jump (Label, Tagatha.C_Not_Equal);
      Unit.Push (1);
      Unit.Pop_Register ("r0");
      Unit.Label (Label);
      Unit.Push_Register ("r0");
   end Generate_Equal;

   -------------------
   -- Generate_Join --
   -------------------

   procedure Generate_Join (Unit : in out Tagatha.Units.Tagatha_Unit) is
   begin
      Unit.Operate (Tagatha.Op_Add);
   end Generate_Join;

   ------------------------
   -- Generate_Not_Equal --
   ------------------------

   procedure Generate_Not_Equal (Unit : in out Tagatha.Units.Tagatha_Unit) is
      Label : constant Positive := Unit.Next_Label;
   begin
      Unit.Push (0);
      Unit.Pop_Register ("r0");
      Unit.Operate (Tagatha.Op_Compare);
      Unit.Jump (Label, Tagatha.C_Equal);
      Unit.Push (1);
      Unit.Pop_Register ("r0");
      Unit.Label (Label);
      Unit.Push_Register ("r0");
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

end Ack.Generate.Primitives;
