private with Aquarius.Programs.Parser;

package Aquarius.Plugins.Plugin is

   type Plugin_Plugin is new Aquarius_Plugin_Type with private;

   overriding
   function Name (Plugin : Plugin_Plugin) return String;

   overriding
   function Version (Plugin : Plugin_Plugin) return String;

   overriding
   procedure Load (Plugin  : not null access Plugin_Plugin;
                   Grammar : Aquarius.Grammars.Aquarius_Grammar);

private

   type Plugin_Plugin is new Aquarius_Plugin_Type with
      record
         Ada_Grammar : Aquarius.Grammars.Aquarius_Grammar;
         Current     : Aquarius.Programs.Parser.Parse_Context;

         Property_Ada_Spec_Tree  : Aquarius.Properties.Property_Type;
         Property_Ada_Body_Tree  : Aquarius.Properties.Property_Type;
         Property_Package_Name   : Aquarius.Properties.Property_Type;
         Property_Internal_Package_Name : Aquarius.Properties.Property_Type;
         Property_Project_Name   : Aquarius.Properties.Property_Type;
         Property_Plugin_Name    : Aquarius.Properties.Property_Type;
         Property_Dependent      : Aquarius.Properties.Property_Type;

         Property_Action_Group   : Aquarius.Properties.Property_Type;
         Property_Package_Group  : Aquarius.Properties.Property_Type;
         Property_Action_Trigger : Aquarius.Properties.Property_Type;
         Property_Action_Binder  : Aquarius.Properties.Property_Type;
      end record;

   type Plugin_Plugin_Access is access all Plugin_Plugin'Class;

   function Plugin return Plugin_Plugin_Access;

end Aquarius.Plugins.Plugin;
