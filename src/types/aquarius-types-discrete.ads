with Aquarius.Types.Scalar;

package Aquarius.Types.Discrete is

   type Root_Discrete_Type is
     abstract new Aquarius.Types.Scalar.Root_Scalar_Type with private;

private

   type Root_Discrete_Type is
     abstract new Aquarius.Types.Scalar.Root_Scalar_Type with null record;

end Aquarius.Types.Discrete;
