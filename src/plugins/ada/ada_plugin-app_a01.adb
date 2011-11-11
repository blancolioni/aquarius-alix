with Aquarius.Entries;
with Aquarius.Entries.Objects;
with Aquarius.Entries.Packages;
with Aquarius.Entries.Types;
with Aquarius.Trees;
with Aquarius.Types;
with Aquarius.Types.Arrays;
with Aquarius.Types.Enumeration;
with Aquarius.Types.Errors;
with Aquarius.Types.Integral;
with Aquarius.Types.Maps;
with Aquarius.Types.Scalar;
with Aquarius.Types.Subtypes;
with Aquarius.Values;

with Tagatha.Commands;
with Tagatha.Fragments;

package body Ada_Plugin.App_A01 is

   procedure Declare_Standard_Object_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      For_Type    : Aquarius.Types.Aquarius_Type;
      Bool        : Aquarius.Types.Aquarius_Type);

   procedure Declare_Standard_Boolean_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      Bool        : Aquarius.Types.Aquarius_Type);

   procedure Declare_Standard_Integral_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      For_Type    : Aquarius.Types.Aquarius_Type);

   procedure Declare_Standard_Scalar_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      For_Type    : Aquarius.Types.Aquarius_Type;
      Bool        : Aquarius.Types.Aquarius_Type);

   procedure Declare_Standard_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      For_Type    : Aquarius.Types.Aquarius_Type;
      Bool        : Aquarius.Types.Aquarius_Type);

   procedure Declare_Operator
     (Table       : Aquarius.Entries.Symbol_Table;
      Name        : String;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class;
      Op_Type     : not null access Aquarius.Types.Root_Aquarius_Type'Class;
      Fragment    : Tagatha.Fragments.Tagatha_Fragment);

   ----------------------
   -- Declare_Operator --
   ----------------------

   procedure Declare_Operator
     (Table       : Aquarius.Entries.Symbol_Table;
      Name        : String;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class;
      Op_Type     : not null access Aquarius.Types.Root_Aquarius_Type'Class;
      Fragment    : Tagatha.Fragments.Tagatha_Fragment)
   is
      Operator : constant Aquarius.Entries.Table_Entry :=
        Aquarius.Entries.Objects.New_Tagatha_Entry
        (Name, Declaration, Op_Type, Fragment);
   begin
      Table.Insert (Operator);
   end Declare_Operator;

   ----------------------------------------
   -- Declare_Standard_Boolean_Operators --
   ----------------------------------------

   procedure Declare_Standard_Boolean_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      Bool        : Aquarius.Types.Aquarius_Type)
   is
      use Tagatha.Fragments, Tagatha.Commands;
      Binary_Boolean : constant Aquarius.Types.Aquarius_Type :=
        Aquarius.Types.Maps.New_Map_Type ((Bool, Bool), Bool);
      Unary_Boolean  : constant Aquarius.Types.Aquarius_Type :=
        Aquarius.Types.Maps.New_Map_Type ((1 => Bool), Bool);
   begin
      Declare_Operator (Table, "and",  Type_Entry.Declaration, Binary_Boolean,
                        Command (Operate (Tagatha.Op_And)));
      Declare_Operator (Table, "or",   Type_Entry.Declaration, Binary_Boolean,
                        Command (Operate (Tagatha.Op_Or)));
      Declare_Operator (Table, "xor",  Type_Entry.Declaration, Binary_Boolean,
                        Command (Operate (Tagatha.Op_Xor)));
      Declare_Operator (Table, "not",  Type_Entry.Declaration, Unary_Boolean,
                        Command (Operate (Tagatha.Op_Not)));
   end Declare_Standard_Boolean_Operators;

   -----------------------------------------
   -- Declare_Standard_Integral_Operators --
   -----------------------------------------

   procedure Declare_Standard_Integral_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      For_Type    : Aquarius.Types.Aquarius_Type)
   is
      use Tagatha.Fragments, Tagatha.Commands;
      X_X_To_X       : constant Aquarius.Types.Aquarius_Type :=
        Aquarius.Types.Maps.New_Map_Type ((For_Type, For_Type), For_Type);
   begin
      Declare_Operator (Table, "+",   Type_Entry.Declaration, X_X_To_X,
                        Command (Operate (Tagatha.Op_Add)));
      Declare_Operator (Table, "-",   Type_Entry.Declaration, X_X_To_X,
                        Command (Operate (Tagatha.Op_Sub)));
      Declare_Operator (Table, "*",   Type_Entry.Declaration, X_X_To_X,
                        Command (Operate (Tagatha.Op_Mul)));
      Declare_Operator (Table, "/",   Type_Entry.Declaration, X_X_To_X,
                        Command (Operate (Tagatha.Op_Div)));
      Declare_Operator (Table, "mod", Type_Entry.Declaration, X_X_To_X,
                        Command (Operate (Tagatha.Op_Mod)));

   end Declare_Standard_Integral_Operators;

   ---------------------------------------
   -- Declare_Standard_Object_Operators --
   ---------------------------------------

   procedure Declare_Standard_Object_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      For_Type    : Aquarius.Types.Aquarius_Type;
      Bool        : Aquarius.Types.Aquarius_Type)
   is
      use Tagatha.Fragments, Tagatha.Commands;
      X_X_To_Boolean : constant Aquarius.Types.Aquarius_Type :=
        Aquarius.Types.Maps.New_Map_Type ((For_Type, For_Type), Bool);
   begin
      Declare_Operator (Table, "=",  Type_Entry.Declaration, X_X_To_Boolean,
                        Compare (Tagatha.C_Equal));
      Declare_Operator (Table, "/=", Type_Entry.Declaration, X_X_To_Boolean,
                        Compare (Tagatha.C_Not_Equal));
   end Declare_Standard_Object_Operators;

   --------------------------------
   -- Declare_Standard_Operators --
   --------------------------------

   procedure Declare_Standard_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      For_Type    : Aquarius.Types.Aquarius_Type;
      Bool        : Aquarius.Types.Aquarius_Type)
   is
      use type Aquarius.Types.Aquarius_Type;
   begin
      Declare_Standard_Object_Operators (Table, Type_Entry, For_Type, Bool);
      if For_Type.all in Aquarius.Types.Scalar.Root_Scalar_Type'Class then
         Declare_Standard_Scalar_Operators (Table, Type_Entry,
                                            For_Type, Bool);
      end if;
      if For_Type.all in Aquarius.Types.Integral.Root_Integer_Type'Class then
         Declare_Standard_Integral_Operators (Table, Type_Entry, For_Type);
      end if;
      if For_Type = Bool then
         Declare_Standard_Boolean_Operators (Table, Type_Entry, Bool);
      end if;

   end Declare_Standard_Operators;

   ---------------------------------------
   -- Declare_Standard_Scalar_Operators --
   ---------------------------------------

   procedure Declare_Standard_Scalar_Operators
     (Table       : Aquarius.Entries.Symbol_Table;
      Type_Entry  : Aquarius.Entries.Table_Entry;
      For_Type    : Aquarius.Types.Aquarius_Type;
      Bool        : Aquarius.Types.Aquarius_Type)
   is
      use Tagatha.Fragments, Tagatha.Commands;
      X_X_To_Boolean : constant Aquarius.Types.Aquarius_Type :=
        Aquarius.Types.Maps.New_Map_Type ((For_Type, For_Type), Bool);
   begin
      Declare_Operator (Table, ">",  Type_Entry.Declaration, X_X_To_Boolean,
                        Compare (Tagatha.C_Greater));
      Declare_Operator (Table, "<",  Type_Entry.Declaration, X_X_To_Boolean,
                        Compare (Tagatha.C_Less));
      Declare_Operator (Table, ">=", Type_Entry.Declaration, X_X_To_Boolean,
                        Compare (Tagatha.C_At_Least));
      Declare_Operator (Table, "<=", Type_Entry.Declaration, X_X_To_Boolean,
                        Compare (Tagatha.C_At_Most));
   end Declare_Standard_Scalar_Operators;

   ---------------------------
   -- Load_Package_Standard --
   ---------------------------

   procedure Load_Package_Standard
     (Plugin : not null access Ada_Plugin_Type'Class)
   is
      Root_Integer_Type   : Aquarius.Types.Aquarius_Type;
      Root_Boolean_Type   : Aquarius.Types.Aquarius_Type;
      Root_String_Type    : Aquarius.Types.Aquarius_Type;
      Root_Character_Type : Aquarius.Types.Aquarius_Type;
      False_Entry         : Aquarius.Entries.Table_Entry;
      True_Entry          : Aquarius.Entries.Table_Entry;
      Integer_Entry       : Aquarius.Entries.Table_Entry;
      Natural_Entry       : Aquarius.Entries.Table_Entry;
      Positive_Entry      : Aquarius.Entries.Table_Entry;
      Boolean_Entry       : Aquarius.Entries.Table_Entry;
      Character_Entry     : Aquarius.Entries.Table_Entry;
      String_Entry        : Aquarius.Entries.Table_Entry;
      Standard_Table      : Aquarius.Entries.Symbol_Table;
      Package_Standard    : Aquarius.Entries.Table_Entry;
   begin

      Root_Integer_Type :=
        Aquarius.Types.Integral.New_Signed_Integer_Type
        (Aquarius.Values.To_Value (Integer'First),
         Aquarius.Values.To_Value (Integer'Last));

      Integer_Entry :=
        Aquarius.Entries.Types.New_Type_Entry
        ("integer", null, Root_Integer_Type);

      Natural_Entry :=
        Aquarius.Entries.Types.New_Type_Entry
        ("natural", null,
         Aquarius.Types.Subtypes.New_Subtype
           (Root_Integer_Type,
            Aquarius.Values.To_Value (0),
            Aquarius.Values.To_Value (Integer'Last)));
      Positive_Entry :=
        Aquarius.Entries.Types.New_Type_Entry
        ("positive", null,
         Aquarius.Types.Subtypes.New_Subtype
           (Root_Integer_Type,
            Aquarius.Values.To_Value (1),
            Aquarius.Values.To_Value (Integer'Last)));

      Root_Integer_Type.Set_Entry (Integer_Entry);

      Root_Boolean_Type :=
        Aquarius.Types.Enumeration.New_Enumeration_Type;
      False_Entry :=
        Aquarius.Types.Enumeration.New_Enumeration_Literal
        (Root_Boolean_Type, null, "false");
      False_Entry.Set_Display_Name ("False");
      True_Entry :=
        Aquarius.Types.Enumeration.New_Enumeration_Literal
        (Root_Boolean_Type, null, "true");
      True_Entry.Set_Display_Name ("True");

      Plugin.Root_Boolean_Type := Root_Boolean_Type;

      Boolean_Entry :=
        Aquarius.Entries.Types.New_Type_Entry
        ("boolean", null, Root_Boolean_Type);

      Root_Boolean_Type.Set_Entry (Boolean_Entry);

      Root_Character_Type :=
        Aquarius.Types.Enumeration.New_Enumeration_Type;
      for Ch in Character'Range loop
         Aquarius.Types.Enumeration.New_Enumeration_Literal
           (Root_Character_Type, Ch);
      end loop;

      Character_Entry :=
        Aquarius.Entries.Types.New_Type_Entry
        ("character", null, Root_Character_Type);

      Root_Character_Type.Set_Entry (Character_Entry);

      Root_String_Type :=
        Aquarius.Types.Arrays.New_Array_Type
        (Aquarius.Entries.Types.Get_Type (Positive_Entry),
         Root_Character_Type, Constrained => False);
      String_Entry :=
        Aquarius.Entries.Types.New_Type_Entry
        ("string", null, Root_String_Type);

      Root_String_Type.Set_Entry (String_Entry);

      Aquarius.Entries.Set_Display_Name (Integer_Entry, "Integer");
      Aquarius.Entries.Set_Display_Name (Integer_Entry, "Natural");
      Aquarius.Entries.Set_Display_Name (Integer_Entry, "Positive");
      Aquarius.Entries.Set_Display_Name (Boolean_Entry, "Boolean");
      Aquarius.Entries.Set_Display_Name (Character_Entry, "Character");
      Aquarius.Entries.Set_Display_Name (String_Entry, "String");

      Standard_Table := Aquarius.Entries.New_Symbol_Table ("standard");
      Standard_Table.Insert (Integer_Entry);
      Standard_Table.Insert (Natural_Entry);
      Standard_Table.Insert (Positive_Entry);
      Standard_Table.Insert (Boolean_Entry);
      Standard_Table.Insert (Character_Entry);
      Standard_Table.Insert (String_Entry);
      Standard_Table.Insert (False_Entry);
      Standard_Table.Insert (True_Entry);

      Declare_Standard_Operators (Standard_Table,
                                  Integer_Entry,
                                  Root_Integer_Type,
                                  Root_Boolean_Type);

      Declare_Standard_Operators (Standard_Table,
                                  Boolean_Entry,
                                  Root_Boolean_Type,
                                  Root_Boolean_Type);

      Declare_Standard_Operators (Standard_Table,
                                  Character_Entry,
                                  Root_Character_Type,
                                  Root_Boolean_Type);

      Declare_Standard_Operators (Standard_Table,
                                  String_Entry,
                                  Root_String_Type,
                                  Root_Boolean_Type);

      Package_Standard :=
        Aquarius.Entries.Packages.New_Standard_Package
        ("standard", Standard_Table);

      Plugin.Add_Standard_Entry (Package_Standard);

      Plugin.Error_Type := Aquarius.Types.Errors.New_Error_Type;

   end Load_Package_Standard;

end Ada_Plugin.App_A01;
