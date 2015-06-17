package Aquarius.Plugins.Macro_11 is

   type Macro_11_Plugin is new Aquarius_Plugin_Type with private;

   overriding
   function Name (Plugin : Macro_11_Plugin) return String;

   overriding
   function Version (Plugin : Macro_11_Plugin) return String;

   overriding
   procedure Load (Plugin  : not null access Macro_11_Plugin;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar);

private

   type Macro_11_Plugin is new Aquarius_Plugin_Type with
      record
         Assembly : Aquarius.Properties.Property_Type;
      end record;

   function Global_Plugin return access Macro_11_Plugin'Class;

end Aquarius.Plugins.Macro_11;
