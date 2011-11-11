with Aquarius.Types.Inference;

with Tagatha.Fragments;

package body Ada_Plugin.Ch04.Gen is

   --------------------
   -- Operator_After --
   --------------------

   procedure Operator_After (Item : Program_Tree) is
      Children  : constant Array_Of_Program_Trees := Item.Direct_Children;
   begin
      for I in Children'Range loop
         Item.Append_Fragment (Children (I).Get_Fragment);
      end loop;
   end Operator_After;

   -----------------------------------
   -- Primary_After_Numeric_Literal --
   -----------------------------------

   procedure Primary_After_Numeric_Literal
     (Tree    : Program_Tree;
      Num_Lit : Program_Tree)
   is
      use type Aquarius.Types.Possible_Types;
      Possibles : constant Aquarius.Types.Possible_Types :=
        Aquarius.Types.Inference.Get_Inferred_Types
        (Tree);
   begin
      if Possibles = null then
         return;
      end if;

      declare
         Tree_Type : constant Aquarius.Types.Aquarius_Type :=
           Aquarius.Types.Get_Type (Possibles.all);
         Object_Bits : constant Natural := Tree_Type.Size_Bits;
      begin
         Tree.Set_Fragment
           (Tagatha.Fragments.Integer_Constant
              (Tagatha.Tagatha_Integer'Value (Num_Lit.Text),
               Tagatha.Bits_To_Size (Object_Bits)));

         Tree.Set_Property (Plugin.Has_Scalar_Property);
      end;
   end Primary_After_Numeric_Literal;

   ------------------------------------
   -- Primary_After_Object_Reference --
   ------------------------------------

   procedure Primary_After_Object_Reference
     (Tree             : Program_Tree;
      Object_Reference : Program_Tree)
   is
      use type Aquarius.Types.Possible_Types;
      Possibles : constant Aquarius.Types.Possible_Types :=
        Aquarius.Types.Inference.Get_Inferred_Types
        (Tree);
   begin
      if Possibles = null then
         return;
      end if;

      Tree.Append_Fragment (Object_Reference.Get_Fragment);
   end Primary_After_Object_Reference;

   --------------------------
   -- Sub_Expression_After --
   --------------------------

   procedure Sub_Expression_After (Item : Program_Tree) is
      Children  : constant Array_Of_Program_Trees := Item.Direct_Children;
      Index     : Positive := Children'First + 1;
   begin
      Item.Append_Fragment (Children (Children'First).Get_Fragment);
      while Index < Children'Last loop
         Item.Append_Fragment (Children (Index + 1).Get_Fragment);
         Item.Append_Fragment (Children (Index).Get_Fragment);
         Index := Index + 2;
      end loop;
   end Sub_Expression_After;

end Ada_Plugin.Ch04.Gen;
