package Aquarius.Buffers.Commands is

   type Root_Argument_Type is tagged private;

   No_Argument : constant Root_Argument_Type;

   type Buffer_Command is access
     function (Buffer : access Aquarius_Buffer_Record'Class;
               Arg    : in     Root_Argument_Type'Class := No_Argument)
              return Boolean;

private

   type Root_Argument_Type is tagged null record;
   No_Argument : constant Root_Argument_Type := (null record);

end Aquarius.Buffers.Commands;


