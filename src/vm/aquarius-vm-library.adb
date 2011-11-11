with Aquarius.Grammars;
with Aquarius.Programs;
with Aquarius.Programs.Parser;
with Aquarius.Trees;

package body Aquarius.VM.Library is

   Have_Standard_Library  : Boolean        := False;
   Local_Standard_Library : Values.VM_Environment;

   function Create_Standard_Library
     return Values.VM_Environment;

   function Eval_Get_Named_Direct_Child
     (Env  : Aquarius.VM.Values.VM_Environment;
      Args : Aquarius.VM.Values.Array_Of_Values)
     return Aquarius.VM.Values.VM_Value;

   function Eval_Create_Tree
     (Env  : Aquarius.VM.Values.VM_Environment;
      Args : Aquarius.VM.Values.Array_Of_Values)
     return Aquarius.VM.Values.VM_Value;

   function Eval_Create_Stub
     (Env  : Aquarius.VM.Values.VM_Environment;
      Args : Aquarius.VM.Values.Array_Of_Values)
     return Aquarius.VM.Values.VM_Value;

   function Eval_Parse
     (Env  : Aquarius.VM.Values.VM_Environment;
      Args : Aquarius.VM.Values.Array_Of_Values)
     return Aquarius.VM.Values.VM_Value;

   function Eval_Build_Tree
     (Env  : Aquarius.VM.Values.VM_Environment;
      Args : Aquarius.VM.Values.Array_Of_Values)
     return Aquarius.VM.Values.VM_Value;

   -----------------------------
   -- Create_Standard_Library --
   -----------------------------

   function Create_Standard_Library
     return Values.VM_Environment
   is
      Env : constant Values.VM_Environment :=
        Values.New_Environment (Values.Null_Environment);
   begin
      Values.Insert (Env, "->",
                     Values.To_Value (Eval_Get_Named_Direct_Child'Access,
                                      (Values.Null_Value, Values.Null_Value,
                                       Values.To_Value (1))));
      Values.Insert (Env, "create_tree",
                     Values.To_Value (Eval_Create_Tree'Access,
                                       (1 => Values.Null_Value)));
      Values.Insert (Env, "create_stub",
                     Values.To_Value (Eval_Create_Stub'Access,
                                      (1 => Values.Null_Value,
                                       2 => Values.Null_Value,
                                       3 => Values.Null_Value,
                                       4 => Values.Null_Value)));
      Values.Insert (Env, "parse",
                     Values.To_Value (Eval_Parse'Access,
                                      (1 => Values.Null_Value,
                                       2 => Values.Null_Value)));
      Values.Insert (Env, "build_tree",
                     Values.To_Value (Eval_Build_Tree'Access,
                                       (1 => Values.Null_Value)));
      return Env;
   end Create_Standard_Library;

   ----------------------
   -- Eval_Build_Tree --
   ----------------------

   function Eval_Build_Tree
     (Env  : Aquarius.VM.Values.VM_Environment;
      Args : Aquarius.VM.Values.Array_Of_Values)
     return Aquarius.VM.Values.VM_Value
   is
      use Aquarius.Programs;
      use Aquarius.VM.Values;
      Grammar   : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Aquarius_Grammar
        (Aquarius.VM.Values.To_Property
           (Aquarius.VM.Values.Get_Value (Env, "grammar")));
      Tree_Name_List : constant VM_Value := Args (Args'First);
      Top_Tree_Name  : constant String   :=
        To_String (Head (Tree_Name_List));
      Top_Tree       : constant Program_Tree :=
        Grammar.Make_Program_Tree (Top_Tree_Name);
      It_Tree        : Program_Tree := Top_Tree;
      It_List        : VM_Value := Tail (Tree_Name_List);
   begin
      while It_List /= Null_Value loop
         declare
            Child_Name : constant String :=
              To_String (Head (It_List));
            Child : constant Program_Tree :=
              Grammar.Make_Program_Tree (Child_Name);
         begin
            Aquarius.Programs.Parser.Parse_Tree (It_Tree, Child);
            It_Tree := Child;
            It_List := Tail (It_List);
         end;
      end loop;

      return To_Value (Top_Tree);

   end Eval_Build_Tree;

   ----------------------
   -- Eval_Create_Stub --
   ----------------------

   function Eval_Create_Stub
     (Env  : Aquarius.VM.Values.VM_Environment;
      Args : Aquarius.VM.Values.Array_Of_Values)
     return Aquarius.VM.Values.VM_Value
   is
      Grammar   : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Aquarius_Grammar
        (Aquarius.VM.Values.To_Property
           (Aquarius.VM.Values.Get_Value (Env, "grammar")));
      Top_Name    : constant String :=
        Aquarius.VM.Values.To_String (Args (Args'First));
      Before_Text : constant String :=
        Aquarius.VM.Values.To_String (Args (Args'First + 1));
      Middle_Name : constant String :=
        Aquarius.VM.Values.To_String (Args (Args'First + 2));
      After_Text : constant String :=
        Aquarius.VM.Values.To_String (Args (Args'First + 3));
      Top_Tree : constant Aquarius.Programs.Program_Tree :=
        Grammar.Make_Program_Tree (Top_Name);
      Middle_Tree : constant Aquarius.Programs.Program_Tree :=
        Grammar.Make_Program_Tree (Middle_Name);
   begin
      Aquarius.Programs.Parser.Parse_Tree
        (Top    => Top_Tree,
         Before => Before_Text,
         Child  => Middle_Tree,
         After  => After_Text);
      return Values.To_Value (Top_Tree);
   end Eval_Create_Stub;

   ----------------------
   -- Eval_Create_Tree --
   ----------------------

   function Eval_Create_Tree
     (Env  : Aquarius.VM.Values.VM_Environment;
      Args : Aquarius.VM.Values.Array_Of_Values)
     return Aquarius.VM.Values.VM_Value
   is
      Tree_Name : constant String :=
        Aquarius.VM.Values.To_String (Args (Args'First));
      Grammar   : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Aquarius_Grammar
        (Aquarius.VM.Values.To_Property
           (Aquarius.VM.Values.Get_Value (Env, "grammar")));
      Result : constant Aquarius.Programs.Program_Tree :=
        Grammar.Make_Program_Tree (Tree_Name);
   begin
      Result.Expand;
      return Values.To_Value (Result);
   end Eval_Create_Tree;

   ---------------------------------
   -- Eval_Get_Named_Direct_Child --
   ---------------------------------

   function Eval_Get_Named_Direct_Child
     (Env  : Aquarius.VM.Values.VM_Environment;
      Args : Aquarius.VM.Values.Array_Of_Values)
     return Aquarius.VM.Values.VM_Value
   is
      pragma Unreferenced (Env);
      T : constant Aquarius.Trees.Tree :=
        Aquarius.Trees.Tree (Values.To_Property (Args (Args'First)));
      Ts : constant Aquarius.Trees.Array_Of_Trees :=
        T.Get_Named_Children;
      Name : constant String := Values.To_String (Args (Args'First + 1));
      Index : Natural := Values.To_Integer (Args (Args'First + 2));
   begin
      for I in Ts'Range loop
         if Ts (I).Name = Name then
            Index := Index - 1;
            if Index = 0 then
               return Values.To_Value (Ts (I));
            end if;
         end if;
      end loop;

      return Values.Error_Value ("no such child: " & Name & Index'Img);

   end Eval_Get_Named_Direct_Child;

   ----------------
   -- Eval_Parse --
   ----------------

   function Eval_Parse
     (Env  : Aquarius.VM.Values.VM_Environment;
      Args : Aquarius.VM.Values.Array_Of_Values)
     return Aquarius.VM.Values.VM_Value
   is
      pragma Unreferenced (Env);
      Top_Tree : constant Aquarius.Programs.Program_Tree :=
        Aquarius.Programs.Program_Tree
        (VM.Values.To_Tree (Args (Args'First)));
      Code : constant String :=
        Aquarius.VM.Values.To_String (Args (Args'First + 1));
   begin
      Aquarius.Programs.Parser.Parse_Tree
        (Top    => Top_Tree,
         Code   => Code);
      return Values.To_Value (Top_Tree);
   end Eval_Parse;

   ----------------------
   -- Standard_Library --
   ----------------------

   function Standard_Library return Values.VM_Environment is
   begin
      if not Have_Standard_Library then
         Local_Standard_Library := Create_Standard_Library;
         Have_Standard_Library  := True;
      end if;

      return Local_Standard_Library;
   end Standard_Library;

end Aquarius.VM.Library;
