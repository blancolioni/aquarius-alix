with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with As.Instructions;

package body As.Environment is

   --------------------------
   -- Contains_Local_Label --
   --------------------------

   function Contains_Local_Label
     (This    : Instance'Class;
      Index   : Positive;
      Forward : Boolean)
      return Boolean
   is
   begin
      if Index <= This.Locals.Last_Index then
         declare
            Set : Local_Label_Sets.Set renames This.Locals (Index);
            Position : constant Local_Label_Sets.Cursor :=
                         (if Forward
                          then Set.Ceiling (This.Loc + 4)
                          else Set.Floor (This.Loc));
         begin
            return Local_Label_Sets.Has_Element (Position);
         end;
      else
         return False;
      end if;
   end Contains_Local_Label;

   ------------
   -- Create --
   ------------

   function Create return Reference is

      Env : constant Reference := new Instance;

      procedure Instr
        (Name : String;
         Value : As.Instructions.Reference);

      procedure Branch
        (Name    : String;
         Base_Op : Word_8);

      procedure YZ_Imm
        (Name    : String;
         Base_Op : Word_8);

      procedure Z_Imm
        (Name  : String;
         Base_Op : Word_8;
         Y_Imm   : Boolean := False);

      ------------
      -- Branch --
      ------------

      procedure Branch
        (Name    : String;
         Base_Op : Word_8)
      is
      begin
         Instr (Name, As.Instructions.I_Branch (Base_Op));
      end Branch;

      -----------
      -- Instr --
      -----------

      procedure Instr
        (Name : String;
         Value : As.Instructions.Reference)
      is
      begin
         Env.Insert (Name, As.Expressions.Instruction (Value));
      end Instr;

      ------------
      -- YZ_Imm --
      ------------

      procedure YZ_Imm
        (Name    : String;
         Base_Op : Word_8)
      is
      begin
         Instr (Name, As.Instructions.I_YZ_Imm (Base_Op));
      end YZ_Imm;

      -----------
      -- Z_Imm --
      -----------

      procedure Z_Imm
        (Name    : String;
         Base_Op : Word_8;
         Y_Imm   : Boolean := False)
      is
      begin
         Instr (Name, As.Instructions.I_Z_Imm (Base_Op, Y_Imm));
      end Z_Imm;

   begin
      Z_Imm ("mul", 16#18#);
      Z_Imm ("div", 16#1C#);
      Z_Imm ("add", 16#20#);
      Z_Imm ("sub", 16#24#);
      Z_Imm ("cmp", 16#30#);
      Z_Imm ("sl", 16#38#);
      Z_Imm ("sr", 16#3A#);
      Z_Imm ("neg", 16#34#, True);

      Z_Imm ("ldb", 16#80#);
      Z_Imm ("ld", 16#88#);
      Z_Imm ("stb", 16#A0#);
      Z_Imm ("st", 16#A8#);

      Z_Imm ("or", 16#C0#);
      Z_Imm ("and", 16#C8#);

      Z_Imm ("csn", 16#60#);
      Z_Imm ("csz", 16#62#);
      Z_Imm ("csp", 16#64#);
      Z_Imm ("csod", 16#66#);
      Z_Imm ("csnn", 16#68#);
      Z_Imm ("csnz", 16#6A#);
      Z_Imm ("csnp", 16#6C#);
      Z_Imm ("csev", 16#6E#);

      Z_Imm ("zsn", 16#70#);
      Z_Imm ("zsz", 16#72#);
      Z_Imm ("zsp", 16#74#);
      Z_Imm ("zsod", 16#76#);
      Z_Imm ("zsnn", 16#78#);
      Z_Imm ("zsnz", 16#7A#);
      Z_Imm ("zsnp", 16#7C#);
      Z_Imm ("zsev", 16#7E#);

      YZ_Imm ("seth", 16#E0#);
      YZ_Imm ("setl", 16#E1#);

      YZ_Imm ("inch", 16#E4#);
      YZ_Imm ("incl", 16#E5#);

      Branch ("bn", 16#40#);
      Branch ("bz", 16#42#);
      Branch ("bp", 16#44#);
      Branch ("bod", 16#46#);
      Branch ("bnn", 16#48#);
      Branch ("bnz", 16#4A#);
      Branch ("bnp", 16#4C#);
      Branch ("bev", 16#4E#);

      Branch ("pushj", 16#F2#);
      Branch ("geta", 16#F4#);

      Instr ("get", As.Instructions.I_Get);

      Instr ("jmp", As.Instructions.I_Jmp);

      Instr ("pop", As.Instructions.I_Pop);

      Instr ("put", As.Instructions.I_Put);

      Instr ("set", As.Instructions.I_Set);

      Instr ("resume", As.Instructions.I_Resume);

      Instr ("trap", As.Instructions.I_Trap);

      Instr ("byte", As.Instructions.Data (1));
      Instr ("word", As.Instructions.Data (4));

      Env.Insert ("@", As.Expressions.Current_Location);

      return Env;
   end Create;

   ----------------------
   -- Find_Local_Label --
   ----------------------

   function Find_Local_Label
     (This    : Instance'Class;
      Index   : Positive;
      Forward : Boolean)
      return Word_32
   is
      pragma Assert (Index <= This.Locals.Last_Index);
      Set      : Local_Label_Sets.Set renames This.Locals (Index);
      Position : constant Local_Label_Sets.Cursor :=
                   (if Forward
                    then Set.Ceiling (This.Loc + 4)
                    else Set.Floor (This.Loc));
      pragma Assert (Local_Label_Sets.Has_Element (Position));
   begin
      return Local_Label_Sets.Element (Position);
   end Find_Local_Label;

   ----------------
   -- Get_Global --
   ----------------

   procedure Get_Global
     (This    : Instance'Class;
      Context : As.Files.File_Context;
      G       : out Register_Index;
      W       : out Word_32)
   is
      use type As.Files.Reference;
   begin
      for Index in This.Last_Global .. Register_Index'Last loop
         if This.Globals (Index).Context.File = Context.File
           and then This.Globals (Index).Context.Line = Context.Line
         then
            G := Index;
            W := This.Globals (Index).Value;
            return;
         end if;
      end loop;
      G := 0;
      W := 0;
   end Get_Global;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (This  : in out Instance'Class;
      Name  : String;
      Value : As.Expressions.Reference)
   is
   begin
      This.Map.Insert
        (Name, Entry_Record'(True, Value));
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (This : in out Instance'Class;
      Name  : String;
      Value : Word_32)
   is
   begin
      This.Insert (Name, Expressions.Word_Value (Value));
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (This : in out Instance'Class;
      Name : String)
   is
   begin
      This.Map.Insert
        (Name, Entry_Record'(False, null));
   end Insert;

   -------------------
   -- Insert_Global --
   -------------------

   procedure Insert_Global
     (This    : in out Instance'Class;
      Context : As.Files.File_Context;
      Name    : String;
      Value   : Word_32)
   is
      Index : constant Register_Index := This.Last_Global - 1;
   begin
      if Name /= "" then
         This.Insert (Name, As.Expressions.Register (Index));
      end if;
      This.Globals (Index) := (Context, Index, Value);
      This.Last_Global := This.Last_Global - 1;
   end Insert_Global;

   ------------------------
   -- Insert_Local_Label --
   ------------------------

   procedure Insert_Local_Label
     (This  : in out Instance'Class;
      Index : Positive)
   is
   begin
      while This.Locals.Last_Index < Index loop
         This.Locals.Append (Local_Label_Sets.Empty);
      end loop;

      This.Locals (Index).Insert (This.Loc);
   end Insert_Local_Label;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This : Instance'Class;
      Process : not null access
        procedure (Name : String))
   is
      package Name_Lists is
        new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
      Names : Name_Lists.List;
      package Name_Sorting is new Name_Lists.Generic_Sorting ("<");
   begin
      for Position in This.Map.Iterate loop
         Names.Append (Entry_Maps.Key (Position));
      end loop;
      Name_Sorting.Sort (Names);
      for Name of Names loop
         Process (Name);
      end loop;
   end Iterate;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (This : in out Instance'Class;
      Loc  : Word_32)
   is
   begin
      This.Loc := Loc;
   end Set_Location;

   ------------
   -- Update --
   ------------

   procedure Update
     (This  : in out Instance'Class;
      Name  : String;
      Value : As.Expressions.Reference)
   is
   begin
      This.Map (Name) := Entry_Record'(True, Value);
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update
     (This : in out Instance'Class;
      Name  : String;
      Value : Word_32)
   is
   begin
      This.Update (Name, Expressions.Word_Value (Value));
   end Update;

end As.Environment;
