with Tagatha.Units;

package Ack.Generate.Intrinsics is

   procedure Generate_Intrinsic
     (Unit      : in out Tagatha.Units.Tagatha_Unit;
      Name      : String;
      Arg_Count : Natural;
      Push      : not null access
        procedure (Argument_Index : Positive));

   type Intrinsic_Generator is access
     function (Unit : in out Tagatha.Units.Tagatha_Unit;
               Push : not null access
                 procedure (Argument_Index : Positive))
               return Boolean;

   procedure Add_Intrinsic
     (Name      : String;
      Generator : Intrinsic_Generator);

end Ack.Generate.Intrinsics;
