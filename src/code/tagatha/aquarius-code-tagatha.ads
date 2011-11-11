package Aquarius.Code.Tagatha is

   type Tagatha_Generator is new Generator with private;

   procedure Push (Gen  : in out Tagatha_Generator;
                   Item : in     Stack_Argument);


   procedure Pop (Gen   : in out Tagatha_Generator;
                  Item  : in     Stack_Argument);

end Aquarius.Code.Tagatha;
