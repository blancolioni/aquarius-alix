with Aquarius.Programs;

package Aquarius.Actions.Interpreter is

   procedure Interpret_Action
     (Action    : Aquarius.Programs.Program_Tree;
      Target    : Aquarius.Programs.Program_Tree);
   --  Action is a tree with the action grammar.  Target is
   --  a tree to which Action should be applied.  The action
   --  tree is interpreted in the context of Target (and Target's
   --  plugin and grammar).

end Aquarius.Actions.Interpreter;
