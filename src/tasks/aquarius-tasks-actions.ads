with Aquarius.Actions;
with Aquarius.Grammars;
with Aquarius.Programs;

package Aquarius.Tasks.Actions is

   type Action_Task_Type is
     new Root_Task_Type with private;

   overriding
   procedure Execute (Item : Action_Task_Type);

   procedure Create (Item    : in out Action_Task_Type;
                     Grammar : Aquarius.Grammars.Aquarius_Grammar;
                     Trigger : Aquarius.Actions.Action_Execution_Trigger;
                     Start   : Aquarius.Programs.Program_Tree);

private

   type Action_Task_Type is
     new Root_Task_Type with
      record
         Grammar : Aquarius.Grammars.Aquarius_Grammar;
         Trigger : Aquarius.Actions.Action_Execution_Trigger;
         Start   : Aquarius.Programs.Program_Tree;
      end record;

end Aquarius.Tasks.Actions;
