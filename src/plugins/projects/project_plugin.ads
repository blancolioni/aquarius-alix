with Aquarius.Grammars;
with Aquarius.Plugins;
private with Aquarius.Properties;

package Project_Plugin is

   type Project_Plugin_Type is
     new Aquarius.Plugins.Aquarius_Plugin_Type with private;

   overriding
   function Name (Plugin : Project_Plugin_Type)
                 return String;

   overriding
   function Version (Plugin : Project_Plugin_Type)
                    return String;

   overriding
   procedure Load (Plugin  : not null access Project_Plugin_Type;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar);

private

   subtype Property_Type is Aquarius.Properties.Property_Type;

   type Project_Plugin_Type is new Aquarius.Plugins.Aquarius_Plugin_Type with
      record
         Package_Property           : Property_Type;
      end record;

   type Project_Plugin_Access is access all Project_Plugin_Type'Class;

   function Plugin return Project_Plugin_Access;

end Project_Plugin;
