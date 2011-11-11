package body Aquarius.Bubbles.Buffers is

   type Buffer_Bubble is new Root_Aquarius_Bubble with
      record
         Buffer : Aquarius.Buffers.Aquarius_Buffer;
      end record;

   procedure Render (Bubble : not null access Buffer_Bubble);

   --------------------------
   -- Create_Buffer_Bubble --
   --------------------------

   function Create_Buffer_Bubble
     (Buffer : in Aquarius.Buffers.Aquarius_Buffer)
     return Aquarius_Bubble
   is
      Result : constant Buffer_Bubble :=
        (Current_Width      => 60,
         Current_Renderer   => null,
         Current_Background =>
           Aquarius.Fonts.Parse_Colour ("purple"),
         Buffer             => Buffer);
   begin
      return new Buffer_Bubble'(Result);
   end Create_Buffer_Bubble;

   ------------
   -- Render --
   ------------

   procedure Render (Bubble : not null access Buffer_Bubble) is
   begin
      Bubble.Buffer.Render (Bubble.Current_Renderer);
   end Render;

end Aquarius.Bubbles.Buffers;
