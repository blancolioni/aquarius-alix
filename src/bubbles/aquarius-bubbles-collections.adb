package body Aquarius.Bubbles.Collections is

   ----------------
   -- Add_Bubble --
   ----------------

   procedure Add_Bubble (To      : in out Aquarius_Bubble_Collection;
                         Bubble  : in     Aquarius_Bubble)
   is
   begin
      To.Bubbles.Append (Bubble);
   end Add_Bubble;

   -----------
   -- Count --
   -----------

   function Count (Collection : Aquarius_Bubble_Collection)
                   return Natural
   is
   begin
      return Collection.Bubbles.Last_Index;
   end Count;

   ----------------
   -- Get_Bubble --
   ----------------

   function Get_Bubble (Collection : Aquarius_Bubble_Collection;
                        Index      : Positive)
                        return Aquarius_Bubble
   is
   begin
      return Collection.Bubbles.Element (Index);
   end Get_Bubble;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position (Collection : Aquarius_Bubble_Collection;
                          Index      : Positive)
                          return Position
   is
   begin
      return Collection.Bubbles.Element (Index).Get_Position;
   end Get_Position;

end Aquarius.Bubbles.Collections;
