package body Aquarius.Tasks.Actions is

   ------------
   -- Create --
   ------------

   procedure Create (Item    : in out Action_Task_Type;
                     Grammar : Aquarius.Grammars.Aquarius_Grammar;
                     Trigger : Aquarius.Actions.Action_Execution_Trigger;
                     Start   : Aquarius.Programs.Program_Tree)
   is
   begin
      Item.Grammar := Grammar;
      Item.Trigger := Trigger;
      Item.Start   := Start;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Item : Action_Task_Type) is
   begin
      Item.Grammar.Run_Action_Trigger (Item.Start, Item.Trigger);
   end Execute;

end Aquarius.Tasks.Actions;
