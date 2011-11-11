package Aquarius.Plugins.Klein.Expressions is

   procedure Expression_After
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure Sub_Expression_After
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure Primary_After
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure Binary_Operator_After
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure Unary_Operator_After
     (Target : not null access Aquarius.Actions.Actionable'Class);

end Aquarius.Plugins.Klein.Expressions;
