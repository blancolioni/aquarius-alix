with Aquarius.Actions;

package Aquarius.Plugins.Klein.Names is

   procedure Name_After
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure Direct_Name_After
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure Expanded_Name_After
     (Expanded_Name_Actionable : not null access Actions.Actionable'Class);

   procedure Component_Name_After
     (Target : not null access Aquarius.Actions.Actionable'Class);

--     procedure Name_Qualifier_After
--       (Target : not null access Aquarius.Actions.Actionable'Class);

end Aquarius.Plugins.Klein.Names;
