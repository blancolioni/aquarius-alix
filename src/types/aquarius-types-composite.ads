package Aquarius.Types.Composite is

   type Root_Composite_Type is
     abstract new Root_Aquarius_Type with private;

private

   type Root_Composite_Type is
     abstract new Root_Aquarius_Type with null record;

end Aquarius.Types.Composite;
