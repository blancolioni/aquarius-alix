with Aquarius.Actions;

package Aquarius.Plugins.Macro_32.Assemble is

   procedure Before_Source_File
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Source_File
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure Source_Element_After_Label
     (Parent : not null access Aquarius.Actions.Actionable'Class;
      Child  : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Declaration
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Directive
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Double_Operand
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Extended_Double_Operand
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Single_Operand
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Branch
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Jump
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Jump_Subroutine
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Return
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure After_Trap
     (Target : not null access Aquarius.Actions.Actionable'Class);

end Aquarius.Plugins.Macro_32.Assemble;
