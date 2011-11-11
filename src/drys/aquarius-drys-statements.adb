package body Aquarius.Drys.Statements is

   type Null_Statement_Record is
     new Statement with null record;

   --------------------
   -- Null_Statement --
   --------------------

   function Null_Statement return Statement'Class is
   begin
      return Result : Null_Statement_Record do
         Result.Aquarius_Tree :=
           Aquarius.Programs.New_Program_Tree
             (Ada_Grammar.Get_Definition ("null_statement"));
      end return;
   end Null_Statement;

   ------------
   -- Append --
   ------------

   procedure Append
     (Sequence : in out Sequence_Of_Statements;
      Item     : in     Statement'Class)
   is
      use type Aquarius.Programs.Program_Tree;
   begin
      if Sequence.Aquarius_Tree = null then
         Sequence.Aquarius_Tree :=
           Aquarius.Programs.New_Program_Tree
             (Ada_Grammar.Get_Definition ("sequence_of_statements"));
      end if;

      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Append unimplemented");
      raise Program_Error;
   end Append;

end Aquarius.Drys.Statements;
