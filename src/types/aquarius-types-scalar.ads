with Aquarius.Types.Elementary;

package Aquarius.Types.Scalar is

   type Root_Scalar_Type is
     abstract new Aquarius.Types.Elementary.Root_Elementary_Type with private;

private

   type Root_Scalar_Type is
     abstract new Aquarius.Types.Elementary.Root_Elementary_Type
     with null record;

end Aquarius.Types.Scalar;
