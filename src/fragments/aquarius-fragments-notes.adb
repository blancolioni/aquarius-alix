package body Aquarius.Fragments.Notes is

   -----------------
   -- Create_Note --
   -----------------

   function Create_Note
     (Width, Height : Positive;
      Text          : String)
      return Aquarius_Fragment
   is
      use Ada.Strings.Unbounded;
      use Aquarius.Fragments.Text;
      Result : constant Aquarius_Fragment :=
                 new Note_Fragment'(Text_Fragment with
                                    Text => To_Unbounded_String (Text));
   begin
      Result.Initialise (Width, Height);
      return Result;
   end Create_Note;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Fragment : in out Note_Fragment)
   is
      R : constant Aquarius.Rendering.Aquarius_Renderer := Fragment.Renderer;
   begin
      R.Begin_Render;
      R.Set_Text
        (Position => (1, 1),
         Class    => "normal",
         Text     => Ada.Strings.Unbounded.To_String (Fragment.Text));
      R.End_Render;
   end Render;

end Aquarius.Fragments.Notes;
