with WL.String_Maps;

with Ack.Generate.Primitives;

package body Ack.Generate.Intrinsics is

   package Generator_Maps is
     new WL.String_Maps (Intrinsic_Generator);

   Generator_Map : Generator_Maps.Map;

   procedure Create_Builtin_Generators;

   function System_Memory_Block_32_Put
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean;

   function Put_Relative_Word_32
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean;

   function Offset_Words
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean;

   -------------------
   -- Add_Intrinsic --
   -------------------

   procedure Add_Intrinsic
     (Name      : String;
      Generator : Intrinsic_Generator)
   is
   begin
      Generator_Map.Insert (Name, Generator);
   end Add_Intrinsic;

   -------------------------------
   -- Create_Builtin_Generators --
   -------------------------------

   procedure Create_Builtin_Generators is
   begin
      Add_Intrinsic
        ("offset_words", Offset_Words'Access);
      Add_Intrinsic
        ("put_relative_word_32", Put_Relative_Word_32'Access);
      Add_Intrinsic
        ("system-memory-block_32-put", System_Memory_Block_32_Put'Access);
   end Create_Builtin_Generators;

   ------------------------
   -- Generate_Intrinsic --
   ------------------------

   procedure Generate_Intrinsic
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Name      : String;
      Arg_Count : Natural;
      Push      : not null access
        procedure (Argument_Index : Positive))
   is
   begin
      if Generator_Map.Is_Empty then
         Create_Builtin_Generators;
      end if;

      if not Generator_Map.Contains (Name) then
         for I in 1 .. Arg_Count loop
            Push (I);
         end loop;
         Ack.Generate.Primitives.Generate_Intrinsic
           (Unit, Get_Name_Id (Name));
         return;
--           raise Constraint_Error with
--             "unknown intrinsic: " & Name;
      end if;

      if not Generator_Map.Element (Name)
        (Unit, Push)
      then
         raise Constraint_Error with
           "generating intrinsic '" & Name & "' failed";
      end if;

   end Generate_Intrinsic;

   ------------------
   -- Offset_Words --
   ------------------

   function Offset_Words
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean
   is
   begin
      Push (1);
      Unit.Push (4);
      Unit.Operate (Tagatha.Op_Mul, Tagatha.Default_Size);
      Unit.Operate (Tagatha.Op_Add, Tagatha.Default_Size);
      return True;
   end Offset_Words;

   --------------------------
   -- Put_Relative_Word_32 --
   --------------------------

   function Put_Relative_Word_32
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean
   is
   begin
      Push (1);
      Unit.Push (4);
      Unit.Operate (Tagatha.Op_Mul, Tagatha.Default_Size);
      Unit.Operate (Tagatha.Op_Add, Tagatha.Default_Size);
      Push (2);
      Unit.Swap;
      Unit.Store;
      return True;
   end Put_Relative_Word_32;

   --------------------------------
   -- System_Memory_Block_32_Put --
   --------------------------------

   function System_Memory_Block_32_Put
     (Unit       : in out Tagatha.Units.Tagatha_Unit;
      Push       : not null access
        procedure (Argument_Index : Positive))
      return Boolean
   is
   begin
      Unit.Push (4);
      Unit.Operate (Tagatha.Op_Add);
      Unit.Dereference;
      Push (1);
      Unit.Push (4);
      Unit.Operate (Tagatha.Op_Mul, Tagatha.Default_Size);
      Unit.Operate (Tagatha.Op_Add, Tagatha.Default_Size);
      Push (2);
      Unit.Swap;
      Unit.Store;
      return True;
   end System_Memory_Block_32_Put;

end Ack.Generate.Intrinsics;
