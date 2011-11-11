with Aquarius.Errors;
with Aquarius.Types.Inference;
with Aquarius.Types.Maps;

package body Ada_Plugin.Ch05 is

   ----------------------
   -- Assignment_After --
   ----------------------

   procedure Assignment_After
     (Assignment : Program_Tree)
   is
      use Aquarius.Types;
      Object_Reference : constant Program_Tree :=
        Assignment.Program_Child ("object_reference");
      Expression       : constant Program_Tree :=
        Assignment.Program_Child ("expression");
      Possibles  : Possible_Types;
   begin
      if Expression /= null and then
        Inference.Has_Inferred_Types (Expression)
      then
         Possibles :=
           Inference.Get_Inferred_Types (Expression);
         if Possibles.Count = 1 then
            null;
         elsif Possibles.Count = 0 then
            Aquarius.Errors.Error (Expression, "no candidate types match");
         else
            Aquarius.Errors.Error (Object_Reference, "ambiguous assignment");
         end if;
      end if;
   end Assignment_After;

   ----------------------------------
   -- Assignment_Before_Expression --
   ----------------------------------

   procedure Assignment_Before_Expression
     (Assignment : Program_Tree;
      Expression : Program_Tree)
   is
      use Aquarius.Types;
      Object_Reference : constant Program_Tree :=
        Assignment.Program_Child ("object_reference");
      Inferred_Target_Type : Possible_Types;
   begin

      if Inference.Has_Inferred_Types (Object_Reference) then
         Inferred_Target_Type :=
           Inference.Get_Inferred_Types (Object_Reference);
         Inference.Set_Possible_Types (Expression, Inferred_Target_Type);
      end if;
   end Assignment_Before_Expression;

   -------------------------------
   -- Boolean_Expression_Before --
   -------------------------------

   procedure Boolean_Expression_Before
     (Statement : Program_Tree;
      Condition : Program_Tree)
   is
      pragma Unreferenced (Statement);
      use Aquarius.Types;
   begin
      Inference.Set_Possible_Types
        (Condition,
         Single_Possible_Type (Plugin.Root_Boolean_Type));
   end Boolean_Expression_Before;

   --------------------------------------------
   -- Procedure_Call_Before_Object_Reference --
   --------------------------------------------

   procedure Procedure_Call_Before_Object_Reference
     (Call : Program_Tree;
      Ref  : Program_Tree)
   is
      use Aquarius.Types, Aquarius.Types.Maps;
      pragma Unreferenced (Call);
   begin
      Inference.Set_Possible_Types
        (Ref,
         Single_Possible_Type (Get_Void_Type));
   end Procedure_Call_Before_Object_Reference;

end Ada_Plugin.Ch05;
