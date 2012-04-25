private with Ada.Strings.Unbounded;

with Aquarius.Fragments.Text;

package Aquarius.Fragments.Notes is

   type Note_Fragment is
     new Aquarius.Fragments.Text.Text_Fragment with private;

   overriding
   procedure Render (Fragment : in out Note_Fragment);

   function Create_Note (Width, Height : Positive;
                         Text          : String)
                         return Aquarius_Fragment;

   overriding
   function On_Key_Press (Fragment : in out Note_Fragment;
                          Position : in     Aquarius.Layout.Position;
                          Key      : in     Aquarius.Keys.Aquarius_Key)
                          return Boolean;

private

   type Note_Fragment is
     new Aquarius.Fragments.Text.Text_Fragment with
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Aquarius.Fragments.Notes;
