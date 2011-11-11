with Ada.Containers.Vectors;

package Aquarius.Bubbles.Collections is

   type Aquarius_Bubble_Collection is private;

   procedure Add_Bubble (To             : in out Aquarius_Bubble_Collection;
                         Bubble         : in     Aquarius_Bubble);

   function Count (Collection : Aquarius_Bubble_Collection)
                  return Natural;

   function Get_Bubble (Collection : Aquarius_Bubble_Collection;
                        Index      : Positive)
                       return Aquarius_Bubble;

   function Get_Position (Collection : Aquarius_Bubble_Collection;
                          Index      : Positive)
                         return Position;

private

   package Bubble_Vector is
      new Ada.Containers.Vectors (Positive, Aquarius_Bubble, "=");

   type Aquarius_Bubble_Collection is
      record
         Bubbles : Bubble_Vector.Vector;
      end record;

end Aquarius.Bubbles.Collections;
