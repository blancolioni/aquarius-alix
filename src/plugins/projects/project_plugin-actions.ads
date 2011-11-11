with Aquarius.Actions;

package Project_Plugin.Actions is

  procedure Bind_Parse_Actions
    (Plugin    : not null access Project_Plugin_Type'Class;
     Parser    : Aquarius.Actions.Action_Group);

end Project_Plugin.Actions;
