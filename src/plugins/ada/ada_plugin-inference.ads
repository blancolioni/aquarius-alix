with Aquarius.Programs;
with Aquarius.Types;

package Ada_Plugin.Inference is

   procedure Check_Expression
     (Expr          : in Aquarius.Programs.Program_Tree;
      Possibles     : in Aquarius.Types.Possible_Types);

   procedure Check_Primary
     (Expr          : in Aquarius.Programs.Program_Tree;
      Possibles     : in Aquarius.Types.Possible_Types);

   procedure Check_Function
     (Expr          : in Aquarius.Programs.Program_Tree;
      Possibles     : in Aquarius.Types.Possible_Types;
      Function_Name : in Aquarius.Programs.Program_Tree;
      Args          : in Aquarius.Programs.Array_Of_Program_Trees);

end Ada_Plugin.Inference;
