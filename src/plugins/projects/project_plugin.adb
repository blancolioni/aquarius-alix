with Aquarius.Actions;

with Project_Plugin.Actions;

package body Project_Plugin is

   Global_Project_Plugin : Project_Plugin_Access;

   procedure Create_Properties
     (Plugin  : in out Project_Plugin_Type'Class;
      Grammar : in     Aquarius.Grammars.Aquarius_Grammar);

   procedure Create_Parse_Actions
      (Plugin       : in out Project_Plugin_Type'Class;
       Action_Group : in     Aquarius.Actions.Action_Group);

   --------------------------
   -- Create_Parse_Actions --
   --------------------------

   procedure Create_Parse_Actions
      (Plugin       : in out Project_Plugin_Type'Class;
       Action_Group : in     Aquarius.Actions.Action_Group)
   is
   begin
      Project_Plugin.Actions.Bind_Parse_Actions (Plugin'Access, Action_Group);
   end Create_Parse_Actions;

   -----------------------
   -- Create_Properties --
   -----------------------

   procedure Create_Properties
     (Plugin  : in out Project_Plugin_Type'Class;
      Grammar : in     Aquarius.Grammars.Aquarius_Grammar)
   is
   begin
      Grammar.Create_Property (Plugin.Package_Property,
                               "property-package-tree",
                               Inherited => True,
                               Has_Value => True);
      Grammar.Create_Property (Plugin.Top_Project_Property,
                               "property-top-project",
                               Inherited => True,
                               Has_Value => True);
   end Create_Properties;

   ----------
   -- Load --
   ----------

   overriding
   procedure Load (Plugin  : not null access Project_Plugin_Type;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar)
   is
      Parser : Aquarius.Actions.Action_Group;
   begin

      Aquarius.Plugins.Load
        (Aquarius.Plugins.Aquarius_Plugin_Type (Plugin.all)'Access, Grammar);

      Grammar.Add_Action_Group ("parser",
                                Aquarius.Actions.Parse_Trigger,
                                Parser);

      Create_Properties (Plugin.all, Grammar);
      Create_Parse_Actions (Plugin.all, Parser);
      Global_Project_Plugin := Project_Plugin_Access (Plugin);

   end Load;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Plugin : Project_Plugin_Type)
                 return String
   is
      pragma Unreferenced (Plugin);
   begin
      return "Aquarius Project";
   end Name;

   ------------
   -- Plugin --
   ------------

   function Plugin return Project_Plugin_Access is
   begin
      return Global_Project_Plugin;
   end Plugin;

   -------------
   -- Version --
   -------------

   overriding
   function Version
     (Plugin : Project_Plugin_Type)
     return String
   is
      pragma Unreferenced (Plugin);
   begin
      return "0.1";
   end Version;

end Project_Plugin;
