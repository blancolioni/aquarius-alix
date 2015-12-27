package Aquarius.Plugins.Macro_32 is

   type Macro_32_Plugin is new Aquarius_Plugin_Type with private;

   overriding
   function Name (Plugin : Macro_32_Plugin) return String;

   overriding
   function Version (Plugin : Macro_32_Plugin) return String;

   overriding
   procedure Load (Plugin  : not null access Macro_32_Plugin;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar);

private

   type Macro_32_Plugin is new Aquarius_Plugin_Type with
      record
         Assembly : Aquarius.Properties.Property_Type;
      end record;

   function Global_Plugin return access Macro_32_Plugin'Class;

end Aquarius.Plugins.Macro_32;
