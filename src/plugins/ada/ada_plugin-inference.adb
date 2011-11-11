with Aquarius.Entries.Objects;
with Aquarius.Errors;
with Aquarius.Messages;
with Aquarius.Types.Inference;
with Aquarius.Types.Maps;

package body Ada_Plugin.Inference is

   use Aquarius.Programs;

   function Is_Leaf (Item : Program_Tree) return Boolean;

   function Get_Possible_Argument_Types
     (Candidates : Aquarius.Entries.Array_Of_Entries;
      Index      : Positive)
     return Aquarius.Types.Possible_Types;

   function Match_Functions
     (Tree       : Program_Tree;
      Candidates : Aquarius.Entries.Array_Of_Entries;
      Args       : Array_Of_Program_Trees)
     return Aquarius.Entries.Table_Entry;

   ----------------------
   -- Check_Expression --
   ----------------------

   procedure Check_Expression
     (Expr      : in Aquarius.Programs.Program_Tree;
      Possibles : in Aquarius.Types.Possible_Types)
   is
   begin

      if Is_Leaf (Expr) then
         Check_Primary (Expr, Possibles);
      elsif Expr.Has_Property (Plugin.Property_Object_Reference) then
         declare
            Function_Name_Tree : constant Program_Tree :=
              Program_Tree (Expr.Property (Plugin.Last_Identifier_Property));
         begin
            Check_Function (Expr          => Expr,
                            Possibles     => Possibles,
                            Function_Name => Function_Name_Tree,
                            Args          =>
                              Expr.Direct_Children ("actual_argument"));
         end;
      elsif Expr.Has_Property (Plugin.Property_Function) then
         Check_Function (Expr          => Expr,
                         Possibles     => Possibles,
                         Function_Name => Expr,
                         Args          => Expr.Direct_Children);
      end if;
   end Check_Expression;

   --------------------
   -- Check_Function --
   --------------------

   procedure Check_Function
     (Expr          : in Aquarius.Programs.Program_Tree;
      Possibles     : in Aquarius.Types.Possible_Types;
      Function_Name : in Aquarius.Programs.Program_Tree;
      Args          : in Aquarius.Programs.Array_Of_Program_Trees)
   is
      Candidates : constant Aquarius.Entries.Array_Of_Entries :=
        Aquarius.Types.Inference.Get_Function_Candidates
        (Function_Name.Symbol_Table, Function_Name.Standard_Text,
         Args'Length, Possibles);
   begin

      if Candidates'Length = 0 then
         if Expr.Symbol_Table.Exists (Function_Name.Standard_Text) then
            Aquarius.Errors.Error (Expr,
                                   "no visible interpretation of " &
                                     Function_Name.Text &
                                     " matches expected type " &
                                     Possibles.Name);
         else
            Aquarius.Errors.Error (Expr,
                                   Function_Name.Text & " is undefined");
         end if;

      else

         for I in Args'Range loop
            declare
               Arg       : constant Program_Tree := Args (I);
               Possibles : constant Aquarius.Types.Possible_Types :=
                 Get_Possible_Argument_Types (Candidates, I);
            begin
               Check_Expression (Arg, Possibles);
            end;
         end loop;

         declare
            use Aquarius.Types.Maps;
            use Aquarius.Entries.Objects;
            Found_Match : constant Aquarius.Entries.Table_Entry :=
              Match_Functions (Expr, Candidates, Args);
         begin
            if not Aquarius.Entries.Is_Null (Found_Match) then
               Expr.Set_Entry (Found_Match);
               Expr.Set_Type
                 (Get_Result_Type (Object_Entry_Type (Found_Match)));
               Aquarius.Types.Inference.Set_Inferred_Types
                 (Expr,
                  Aquarius.Types.Single_Possible_Type
                    (Expr.Get_Type));
            end if;
         end;
      end if;
   end Check_Function;

   -------------------
   -- Check_Primary --
   -------------------

   procedure Check_Primary
     (Expr      : in Aquarius.Programs.Program_Tree;
      Possibles : in Aquarius.Types.Possible_Types)
   is
   begin
      Aquarius.Types.Inference.Set_Possible_Types (Expr, Possibles);
      Aquarius.Types.Inference.Check_Types (Expr);
   end Check_Primary;

   ---------------------------------
   -- Get_Possible_Argument_Types --
   ---------------------------------

   function Get_Possible_Argument_Types
     (Candidates : Aquarius.Entries.Array_Of_Entries;
      Index      : Positive)
     return Aquarius.Types.Possible_Types
   is
      use Aquarius.Types;
      Result : constant Possible_Types := new Possible_Type_Record;
   begin
      for I in Candidates'Range loop
         Result.Add_Type
           (Aquarius.Types.Maps.Get_Argument_Type
              (Aquarius.Entries.Objects.Object_Entry_Type
                 (Candidates (I)),
               Index));
      end loop;
      return Result;
   end Get_Possible_Argument_Types;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (Item : Program_Tree) return Boolean is
   begin
      return Item.Has_Property (Plugin.Property_Node);
   end Is_Leaf;

   ---------------------
   -- Match_Functions --
   ---------------------

   function Match_Functions
     (Tree       : Program_Tree;
      Candidates : Aquarius.Entries.Array_Of_Entries;
      Args       : Array_Of_Program_Trees)
     return Aquarius.Entries.Table_Entry
   is
      use Aquarius.Types;
      Match       : array (Candidates'Range) of Boolean :=
        (others => False);
      Match_Count : Natural := 0;
      Found_Match : Aquarius.Entries.Table_Entry;
      Success     : Boolean;
      Inferred_Types : array (Args'Range) of Possible_Types;
   begin

      for I in Inferred_Types'Range loop
         if Aquarius.Types.Inference.Has_Inferred_Types (Args (I)) then
            Inferred_Types (I) :=
              Aquarius.Types.Inference.Get_Inferred_Types (Args (I));
         else
            return null;
         end if;
      end loop;

      for I in Candidates'Range loop
         declare
            Formal_Types     : constant Array_Of_Types :=
              Aquarius.Types.Maps.Get_Argument_Types
              (Aquarius.Entries.Objects.Object_Entry_Type
                 (Candidates (I)));
         begin
            Success := True;
            for A in Inferred_Types'Range loop
               if not Unifies (Inferred_Types (A), Formal_Types (A)) then
                  Success := False;
                  exit;
               end if;
            end loop;
         end;

         Match (I) := Success;
         if Success then
            Match_Count := Match_Count + 1;
            Found_Match := Candidates (I);
         end if;
      end loop;

      if Match_Count = 0 then
         Aquarius.Errors.Error (Tree,
                                "no visible interpretation of " &
                                  Candidates (1).Name &
                                  " matches the actuals");
         return null;
      elsif Match_Count > 1 then
         declare
            use Aquarius.Messages;
            M : constant Message :=
              New_Message (Aquarius.Messages.Error, Tree,
                           "ambiguous expression " &
                             "(cannot resolve """ &
                             Candidates (1).Name & """)");
         begin
            for I in Match'Range loop
               if Match (I) then
                  Add_Reference (M, Candidates (I).Declaration,
                                 "possible interpretation: " &
                                   Aquarius.Entries.Objects.Object_Entry_Type
                                   (Candidates (I)).Description);
               end if;
            end loop;
            Tree.Attach_Message (M);
         end;
         return null;
      else
         return Found_Match;
      end if;
   end Match_Functions;

end Ada_Plugin.Inference;
