with Aquarius.Actions;
with Aquarius.Programs;
with Aquarius.Plugins.Script_Plugin.Bindings;
package body Aquarius.Plugins.Script_Plugin.Generated is
   package Bindings is
      pragma Style_Checks (Off);
      procedure Actionable_plugin_declaration_Before_list_of_declarations (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                           Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_property_declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_group_declaration_Before_list_of_actions (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_group_declaration_After_list_of_actions (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                    Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_group_declaration_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_group_declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_action_Before_action_definition (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                            Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_action_After_action_time (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_action_After_action_context (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                        Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_action_After_action_definition (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                           Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_let_expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_let_expression_After_binding (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                         Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_call_expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_call_expression_After_name (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                       Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_arguments_After_expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                       Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_call_expression_After_arguments (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                            Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_binding_After_expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_binding_After_identifier (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class);
      procedure Actionable_literal_expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class);
   end Bindings;
   package body Bindings is
      procedure Actionable_plugin_declaration_Before_list_of_declarations (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                           Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.plugin_declaration_Before_list_of_declarations (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_plugin_declaration_Before_list_of_declarations;
      procedure Actionable_property_declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.property_declaration_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_property_declaration_After;
      procedure Actionable_group_declaration_Before_list_of_actions (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.group_declaration_Before_list_of_actions (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_group_declaration_Before_list_of_actions;
      procedure Actionable_group_declaration_After_list_of_actions (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                                    Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.group_declaration_After_list_of_actions (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_group_declaration_After_list_of_actions;
      procedure Actionable_group_declaration_Before
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.group_declaration_Before (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_group_declaration_Before;
      procedure Actionable_group_declaration_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.group_declaration_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_group_declaration_After;
      procedure Actionable_action_Before_action_definition (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                            Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.action_Before_action_definition (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_action_Before_action_definition;
      procedure Actionable_action_After_action_time (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.action_After_action_time (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_action_After_action_time;
      procedure Actionable_action_After_action_context (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                        Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.action_After_action_context (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_action_After_action_context;
      procedure Actionable_action_After_action_definition (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                           Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.action_After_action_definition (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_action_After_action_definition;
      procedure Actionable_expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_expression_After;
      procedure Actionable_let_expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.let_expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_let_expression_After;
      procedure Actionable_let_expression_After_binding (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                         Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.let_expression_After_binding (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_let_expression_After_binding;
      procedure Actionable_call_expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.call_expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_call_expression_After;
      procedure Actionable_call_expression_After_name (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                       Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.call_expression_After_name (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_call_expression_After_name;
      procedure Actionable_arguments_After_expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                       Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.arguments_After_expression (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_arguments_After_expression;
      procedure Actionable_call_expression_After_arguments (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                            Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.call_expression_After_arguments (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_call_expression_After_arguments;
      procedure Actionable_binding_After_expression (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.binding_After_expression (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_binding_After_expression;
      procedure Actionable_binding_After_identifier (Target_Actionable : not null access Aquarius.Actions.Actionable'Class;
                                                     Child_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.binding_After_identifier (Aquarius.Programs.Program_Tree
           (Target_Actionable), Aquarius.Programs.Program_Tree
           (Child_Actionable));
      end Actionable_binding_After_identifier;
      procedure Actionable_literal_expression_After
        (Target_Actionable : not null access Aquarius.Actions.Actionable'Class) is
      begin
         Aquarius.Plugins.Script_Plugin.Bindings.literal_expression_After (Aquarius.Programs.Program_Tree
           (Target_Actionable));
      end Actionable_literal_expression_After;
   end Bindings;
   procedure Bind_Actions (Plugin : in out Script_Plugin_Type;
                           Grammar : in Aquarius.Grammars.Aquarius_Grammar) is
      Parser : Aquarius.Actions.Action_Group;
   begin
      Grammar.Add_Action_Group ("parser",
                                Aquarius.Actions.Parse_Trigger, Parser);
      Plugin.Register_Action ("plugin_declaration",
                              "list_of_declarations", Parser,
                              Aquarius.Before,
                              Bindings.Actionable_plugin_declaration_Before_list_of_declarations'access);
      Plugin.Register_Action ("property_declaration", Parser,
                              Aquarius.After,
                              Bindings.Actionable_property_declaration_After'access);
      Plugin.Register_Action ("group_declaration", "list_of_actions",
                              Parser, Aquarius.Before,
                              Bindings.Actionable_group_declaration_Before_list_of_actions'access);
      Plugin.Register_Action ("group_declaration", "list_of_actions",
                              Parser, Aquarius.After,
                              Bindings.Actionable_group_declaration_After_list_of_actions'access);
      Plugin.Register_Action ("group_declaration", Parser,
                              Aquarius.Before,
                              Bindings.Actionable_group_declaration_Before'access);
      Plugin.Register_Action ("group_declaration", Parser,
                              Aquarius.After,
                              Bindings.Actionable_group_declaration_After'access);
      Plugin.Register_Action ("action", "action_definition", Parser,
                              Aquarius.Before,
                              Bindings.Actionable_action_Before_action_definition'access);
      Plugin.Register_Action ("action", "action_time", Parser,
                              Aquarius.After,
                              Bindings.Actionable_action_After_action_time'access);
      Plugin.Register_Action ("action", "action_context", Parser,
                              Aquarius.After,
                              Bindings.Actionable_action_After_action_context'access);
      Plugin.Register_Action ("action", "action_definition", Parser,
                              Aquarius.After,
                              Bindings.Actionable_action_After_action_definition'access);
      Plugin.Register_Action ("expression", Parser, Aquarius.After,
                              Bindings.Actionable_expression_After'access);
      Plugin.Register_Action ("let_expression", Parser, Aquarius.After,
                              Bindings.Actionable_let_expression_After'access);
      Plugin.Register_Action ("let_expression", "binding", Parser,
                              Aquarius.After,
                              Bindings.Actionable_let_expression_After_binding'access);
      Plugin.Register_Action ("call_expression", Parser,
                              Aquarius.After,
                              Bindings.Actionable_call_expression_After'access);
      Plugin.Register_Action ("call_expression", "name", Parser,
                              Aquarius.After,
                              Bindings.Actionable_call_expression_After_name'access);
      Plugin.Register_Action ("arguments", "expression", Parser,
                              Aquarius.After,
                              Bindings.Actionable_arguments_After_expression'access);
      Plugin.Register_Action ("call_expression", "arguments", Parser,
                              Aquarius.After,
                              Bindings.Actionable_call_expression_After_arguments'access);
      Plugin.Register_Action ("binding", "expression", Parser,
                              Aquarius.After,
                              Bindings.Actionable_binding_After_expression'access);
      Plugin.Register_Action ("binding", "identifier", Parser,
                              Aquarius.After,
                              Bindings.Actionable_binding_After_identifier'access);
      Plugin.Register_Action ("literal_expression", Parser,
                              Aquarius.After,
                              Bindings.Actionable_literal_expression_After'access);
   end Bind_Actions;
end Aquarius.Plugins.Script_Plugin.Generated;
