with Aquarius.Plugins.Macro_32.Assemble;

package body Aquarius.Plugins.Macro_32 is

   Global_Macro_32_Plugin : Aquarius_Plugin;

   -------------------
   -- Global_Plugin --
   -------------------

   function Global_Plugin return access Macro_32_Plugin'Class is
   begin
      return Macro_32_Plugin (Global_Macro_32_Plugin.all)'Access;
   end Global_Plugin;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Plugin  : not null access Macro_32_Plugin;
      Grammar : in     Aquarius.Grammars.Aquarius_Grammar)
   is
      Assemble : Aquarius.Actions.Action_Group;
   begin
      Load (Aquarius_Plugin_Type (Plugin.all)'Access, Grammar);

      Aquarius.Properties.Create_Property
        (Pool      => Grammar.all,
         Prop      => Plugin.Assembly,
         Name      => "assembly",
         Inherited => True,
         Has_Value => True);

      Grammar.Add_Action_Group ("assemble",
                                Aquarius.Actions.Semantic_Trigger,
                                Assemble);

      Plugin.Register_Action
        ("source_file", Assemble, Before,
         Plugins.Macro_32.Assemble.Before_Source_File'Access);

      Plugin.Register_Action
        ("source_file", Assemble, After,
         Plugins.Macro_32.Assemble.After_Source_File'Access);

      Plugin.Register_Action
        ("source_element", "label", Assemble, After,
         Plugins.Macro_32.Assemble.Source_Element_After_Label'Access);

      Plugin.Register_Action
        ("declaration", Assemble, After,
         Plugins.Macro_32.Assemble.After_Declaration'Access);

      Plugin.Register_Action
        ("directive", Assemble, After,
         Plugins.Macro_32.Assemble.After_Directive'Access);

      Plugin.Register_Action
        ("triple_operand", Assemble, After,
         Plugins.Macro_32.Assemble.After_Triple_Operand'Access);

      Plugin.Register_Action
        ("double_operand", Assemble, After,
         Plugins.Macro_32.Assemble.After_Double_Operand'Access);

      Plugin.Register_Action
        ("single_operand", Assemble, After,
         Plugins.Macro_32.Assemble.After_Single_Operand'Access);

      Plugin.Register_Action
        ("no_operand", Assemble, After,
         Plugins.Macro_32.Assemble.After_No_Operand'Access);

      Plugin.Register_Action
        ("property", Assemble, After,
         Plugins.Macro_32.Assemble.After_Property'Access);

      Plugin.Register_Action
        ("iterator", Assemble, After,
         Plugins.Macro_32.Assemble.After_Iterator'Access);

      Plugin.Register_Action
        ("branch", Assemble, After,
         Plugins.Macro_32.Assemble.After_Branch'Access);

      Plugin.Register_Action
        ("jump", Assemble, After,
         Plugins.Macro_32.Assemble.After_Jump'Access);

      Plugin.Register_Action
        ("trap", Assemble, After,
         Plugins.Macro_32.Assemble.After_Trap'Access);

      Global_Macro_32_Plugin := Aquarius_Plugin (Plugin);
   end Load;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Plugin : Macro_32_Plugin)
      return String
   is
      pragma Unreferenced (Plugin);
   begin
      return "macro-32";
   end Name;

   -------------
   -- Version --
   -------------

   overriding function Version
     (Plugin : Macro_32_Plugin)
      return String
   is
      pragma Unreferenced (Plugin);
   begin
      return "1.0";
   end Version;

end Aquarius.Plugins.Macro_32;
