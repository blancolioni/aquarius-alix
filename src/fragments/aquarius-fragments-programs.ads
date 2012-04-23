with Aquarius.Fragments.Text;
with Aquarius.Programs;

package Aquarius.Fragments.Programs is

   type Program_Fragment is
     new Aquarius.Fragments.Text.Text_Fragment with private;

   overriding
   procedure Render (Fragment : in out Program_Fragment);

   function Create_Program (Width, Height : Positive;
                            Program       : Aquarius.Programs.Program_Tree)
                            return Aquarius_Fragment;

private

   type Program_Fragment is
     new Aquarius.Fragments.Text.Text_Fragment with
      record
         Program : Aquarius.Programs.Program_Tree;
      end record;

end Aquarius.Fragments.Programs;
