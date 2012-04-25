with Aquarius.Grammars;
with Aquarius.Programs;

package Aquarius.Fragments.Programs is

   function Create_Program
     (Width, Height : Positive;
      Grammar       : Aquarius.Grammars.Aquarius_Grammar;
      Program       : Aquarius.Programs.Program_Tree)
      return Aquarius_Fragment;

end Aquarius.Fragments.Programs;
