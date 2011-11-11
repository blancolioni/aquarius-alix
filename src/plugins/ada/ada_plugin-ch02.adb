with Aquarius.Programs;                 use Aquarius.Programs;
with Aquarius.Types.Inference;
with Aquarius.Types.Integral;

package body Ada_Plugin.Ch02 is

   ---------------------------
   -- Numeric_Literal_After --
   ---------------------------

   procedure Numeric_Literal_After
     (Tree : Program_Tree)
   is
      use Aquarius.Types, Aquarius.Types.Integral;
   begin
      Aquarius.Types.Inference.Set_Inferred_Types
        (Tree, Single_Possible_Type (Universal_Integer_Type));
   end Numeric_Literal_After;

end Ada_Plugin.Ch02;
