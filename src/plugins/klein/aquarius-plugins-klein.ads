with Aquarius.Properties;
with Aquarius.Types;

package Aquarius.Plugins.Klein is

   type Klein_Plugin is new Aquarius_Plugin_Type with private;

   overriding
   function Name (Plugin : Klein_Plugin) return String;

   overriding
   function Version (Plugin : Klein_Plugin) return String;

   overriding
   procedure Load (Plugin  : not null access Klein_Plugin;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar);

private

   type Klein_Plugin is new Aquarius_Plugin_Type with
      record
         Error_Type              : Aquarius.Types.Aquarius_Type;

         --  Properties for arranging expressions
         Property_Left           : Aquarius.Properties.Property_Type;
         Property_Right          : Aquarius.Properties.Property_Type;
         Property_Function       : Aquarius.Properties.Property_Type;
         Property_Node           : Aquarius.Properties.Property_Type;
         Property_Expression     : Aquarius.Properties.Property_Type;

         --  Properties for type inference
         Property_Possible_Types : Aquarius.Properties.Property_Type;
         Property_Inferred_Types : Aquarius.Properties.Property_Type;

         --  properties for tagging names
         Object_Property         : Aquarius.Properties.Property_Type;
         Package_Property        : Aquarius.Properties.Property_Type;
         Record_Property         : Aquarius.Properties.Property_Type;
         Procedure_Property      : Aquarius.Properties.Property_Type;
         Package_Spec_Property   : Aquarius.Properties.Property_Type;
         Package_Body_Property   : Aquarius.Properties.Property_Type;

         --  error tags
         Type_Error_Tag          : Aquarius.Properties.Property_Type;
      end record;

   procedure Load_Package_Standard (Plugin : in out Klein_Plugin);

   type Klein_Plugin_Access is access all Klein_Plugin'Class;

   function Plugin return Klein_Plugin_Access;

end Aquarius.Plugins.Klein;
