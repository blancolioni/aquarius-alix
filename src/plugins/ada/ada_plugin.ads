with Aquarius.Entries;
with Aquarius.Grammars;
with Aquarius.Plugins;
with Aquarius.Programs;
with Aquarius.Projects;
with Aquarius.Properties;
with Aquarius.Types;

package Ada_Plugin is

   type Ada_Plugin_Type is
     new Aquarius.Plugins.Aquarius_Plugin_Type
     with private;

   overriding
   function Name (Plugin : Ada_Plugin_Type) return String;

   overriding
   function Version (Plugin : Ada_Plugin_Type) return String;

   overriding
   procedure Load (Plugin  : not null access Ada_Plugin_Type;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar);

private

   subtype Property_Type is Aquarius.Properties.Property_Type;

   type Ada_Plugin_Type is
     new Aquarius.Plugins.Aquarius_Plugin_Type
     and Aquarius.Watcher
     with
      record
         Error_Type                 : Aquarius.Types.Aquarius_Type;
         Root_Boolean_Type          : Aquarius.Types.Aquarius_Type;

         --  Properties for arranging expressions
         Property_Left              : Property_Type;
         Property_Right             : Property_Type;
         Property_Function          : Property_Type;
         Property_Node              : Property_Type;
         Property_Expression        : Property_Type;
         Property_Object_Reference  : Property_Type;

         --  properties for tagging names
         Object_Property            : Property_Type;
         Package_Property           : Property_Type;
         Record_Property            : Property_Type;
         Procedure_Property         : Property_Type;
         Package_Spec_Property      : Property_Type;
         Package_Body_Property      : Property_Type;
         Defining_Instance_Property : Property_Type;
         Attribute_Property         : Property_Type;
         Last_Identifier_Property   : Property_Type;
         Defined_Name_Property      : Property_Type;

         --  properties for referring to other parts of the tree
         Object_Reference_Property  : Property_Type;
         Compilation_Unit_Property  : Property_Type;
         Type_Declaration_Property  : Property_Type;

         --  properties for arranging labels for loops etc
         Top_Label_Property         : Property_Type;
         Out_Label_Property         : Property_Type;

         --  properties for tagging subtrees
         Private_Section_Property   : Property_Type;

         --  properties for tagging information about storage
         Has_Address_Property       : Property_Type;
         Has_Scalar_Property        : Property_Type;

         --  error tags
         Type_Error_Tag             : Property_Type;
      end record;

   type Ada_Plugin_Access is access all Ada_Plugin_Type'Class;

   function Plugin return Ada_Plugin_Access;

   function Load_Child_Package (Project : Aquarius.Projects.Aquarius_Project;
                                Tree     : Aquarius.Programs.Program_Tree;
                                Parent  : Aquarius.Entries.Table_Entry;
                                Child   : String)
                               return Aquarius.Entries.Table_Entry;

end Ada_Plugin;
