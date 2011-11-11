with Aquarius.Errors;
with Aquarius.Entries.Objects;
with Aquarius.Messages;
with Aquarius.Types.Inference;
with Aquarius.Types.Maps;

package body Ada_Plugin.Ch04 is

   package Type_Inference renames
     Aquarius.Types.Inference;

   function Get_Left (Item : Program_Tree) return Program_Tree;
   function Get_Right (Item : Program_Tree) return Program_Tree;
   function Is_Leaf (Item : Program_Tree) return Boolean;

   function Get_Function_Name (Item : Program_Tree) return String;
   function Get_Argument_Count (Item : Program_Tree) return Natural;
   function Get_Arguments (Item : Program_Tree)
                          return Array_Of_Program_Trees;

   function Get_Possible_Argument_Types
     (Candidates : Aquarius.Entries.Array_Of_Entries;
      Index      : Positive)
     return Aquarius.Types.Possible_Types;

   procedure Check_Expression
     (Expr      : in Aquarius.Programs.Program_Tree;
      Possibles : in Aquarius.Types.Possible_Types);

   procedure Check_Function
     (Expr      : in Aquarius.Programs.Program_Tree;
      Possibles : in Aquarius.Types.Possible_Types);

   procedure Check_Primary
     (Expr      : in Aquarius.Programs.Program_Tree;
      Possibles : in Aquarius.Types.Possible_Types);

   function Match_Functions
     (Tree       : Program_Tree;
      Candidates : Aquarius.Entries.Array_Of_Entries;
      Args       : Array_Of_Program_Trees)
     return Aquarius.Entries.Table_Entry;

   ---------------------------
   -- Binary_Operator_After --
   ---------------------------

   procedure Binary_Operator_After
     (Node : Program_Tree)
   is
   begin
      Node.Set_Property (Plugin.Property_Function);
      Node.Set_Property (Plugin.Property_Expression, Node);
   end Binary_Operator_After;

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
      else
         Check_Function (Expr, Possibles);
      end if;
   end Check_Expression;

   --------------------
   -- Check_Function --
   --------------------

   procedure Check_Function
     (Expr      : in Aquarius.Programs.Program_Tree;
      Possibles : in Aquarius.Types.Possible_Types)
   is
      Candidates : constant Aquarius.Entries.Array_Of_Entries :=
        Type_Inference.Get_Function_Candidates
        (Expr.Symbol_Table, Get_Function_Name (Expr),
         Get_Argument_Count (Expr), Possibles);
   begin

      if Candidates'Length = 0 then
         if Expr.Symbol_Table.Exists (Get_Function_Name (Expr)) then
            Aquarius.Errors.Error (Expr,
                                   "no visible interpretation of " &
                                     Get_Function_Name (Expr) &
                                     " matches expected type " &
                                     Possibles.Name);
         else
            Aquarius.Errors.Error (Expr,
                                   Get_Function_Name (Expr) &
                                     " is undefined");
         end if;
      else

         declare
            Args : constant Array_Of_Program_Trees :=
              Get_Arguments (Expr);
         begin
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
                    (Get_Result_Type (Object_Entry_Type
                     (Found_Match)));
                  Type_Inference.Set_Inferred_Types
                    (Expr,
                     Aquarius.Types.Single_Possible_Type
                       (Expr.Get_Type));
                  if Aquarius.Entries.Objects.Is_Intrinsic (Found_Match) then
                     Expr.Set_Fragment
                       (Aquarius.Entries.Objects.Object_Entry_Intrinsic_Value
                          (Found_Match));
                  end if;
               end if;
            end;
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
      Type_Inference.Set_Possible_Types (Expr, Possibles);
      Type_Inference.Check_Types (Expr);
   end Check_Primary;

   ----------------------
   -- Expression_After --
   ----------------------

   procedure Expression_After
     (Tree : Program_Tree)
   is
      Expr      : Program_Tree;
      Possibles : Aquarius.Types.Possible_Types;
   begin

      Sub_Expression_After (Tree);

      if not Tree.Has_Property (Plugin.Property_Expression) then
         return;
      end if;

      Expr := Program_Tree (Tree.Property (Plugin.Property_Expression));

      if not Type_Inference.Has_Possible_Types (Tree) then
         return;
      end if;

      Possibles :=
        Type_Inference.Get_Possible_Types (Tree);

      Check_Expression (Expr, Possibles);
      if Type_Inference.Has_Inferred_Types (Expr) then
         Type_Inference.Set_Inferred_Types
           (Tree, Type_Inference.Get_Inferred_Types (Expr));
      end if;
   end Expression_After;

   ----------------------
   -- Factor_After_Not --
   ----------------------

   procedure Factor_After_Not
     (Factor    : Program_Tree;
      Not_Token : Program_Tree)
   is
      pragma Unreferenced (Factor);
   begin
      Not_Token.Set_Property (Plugin.Property_Function);
      Not_Token.Set_Property (Plugin.Property_Expression, Not_Token);
   end Factor_After_Not;

   ------------------------
   -- Get_Argument_Count --
   ------------------------

   function Get_Argument_Count (Item : Program_Tree) return Natural is
      pragma Unreferenced (Item);
   begin
      return 2;
   end Get_Argument_Count;

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments (Item : Program_Tree)
                          return Array_Of_Program_Trees
   is
   begin
      return (1 => Get_Left (Item),
              2 => Get_Right (Item));
   end Get_Arguments;

   -----------------------
   -- Get_Function_Name --
   -----------------------

   function Get_Function_Name (Item : Program_Tree) return String is
   begin
      return Item.Text;
   end Get_Function_Name;

   --------------
   -- Get_Left --
   --------------

   function Get_Left (Item : Program_Tree) return Program_Tree is
   begin
      return Program_Tree (Item.Property (Plugin.Property_Left));
   end Get_Left;

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
         Add_Type (Result.all,
                   Aquarius.Types.Maps.Get_Argument_Type
                     (Aquarius.Entries.Objects.Object_Entry_Type
                        (Candidates (I)),
                      Index));
      end loop;
      return Result;
   end Get_Possible_Argument_Types;

   ---------------
   -- Get_Right --
   ---------------

   function Get_Right (Item : Program_Tree) return Program_Tree is
   begin
      return Program_Tree (Item.Property (Plugin.Property_Right));
   end Get_Right;

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
         if Type_Inference.Has_Inferred_Types (Args (I)) then
            Inferred_Types (I) :=
              Inference.Get_Inferred_Types (Args (I));
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

   -------------------
   -- Primary_After --
   -------------------

   procedure Primary_After
     (Node : Program_Tree)
   is
   begin

      Node.Set_Property (Plugin.Property_Expression, Node);
      Node.Set_Property (Plugin.Property_Node);

      if Type_Inference.Has_Inferred_Types (Node.Chosen_Tree) then
         Type_Inference.Set_Inferred_Types (Node,
                                       Type_Inference.Get_Inferred_Types
                                              (Node.Chosen_Tree));
      end if;
   end Primary_After;

   --------------------------
   -- Sub_Expression_After --
   --------------------------

   procedure Sub_Expression_After
     (Node : Program_Tree)
   is
      Children  : constant Array_Of_Program_Trees := Node.Direct_Children;
      Index     : Positive := Children'First + 1;
      Tree      : Program_Tree;
   begin

      for I in Children'Range loop
         if not Children (I).Has_Property (Plugin.Property_Expression) then
            return;
         end if;
      end loop;

      Tree :=
        Program_Tree
        (Children (Children'First).Property (Plugin.Property_Expression));
      while Index <= Children'Last loop
         declare
            Child : constant Program_Tree := Children (Index);
         begin
            if Child.Has_Property (Plugin.Property_Function) then
               --  drop down to the operator
               declare
                  Op : constant Program_Tree :=
                    Program_Tree (Child.First_Leaf);
               begin
                  Op.Set_Property (Plugin.Property_Left, Tree);
                  Tree := Op;
               end;
            elsif not Tree.Has_Property (Plugin.Property_Left) then
               Tree.Set_Property
                 (Plugin.Property_Left,
                  Child.Property (Plugin.Property_Expression));
            else
               Tree.Set_Property
                 (Plugin.Property_Right,
                  Child.Property (Plugin.Property_Expression));
            end if;
         end;
         Index := Index + 1;
      end loop;

      Node.Set_Property (Plugin.Property_Node);
      Node.Set_Property (Plugin.Property_Expression, Tree);

   end Sub_Expression_After;

   --------------------------
   -- Unary_Operator_After --
   --------------------------

   procedure Unary_Operator_After
     (Node : Program_Tree)
   is
   begin
      Node.Set_Property (Plugin.Property_Function);
      Node.Set_Property (Plugin.Property_Expression, Node);
   end Unary_Operator_After;

end Ada_Plugin.Ch04;
