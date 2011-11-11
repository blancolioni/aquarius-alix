private with Aquarius.Names;
private with Ada.Containers.Vectors;

with Aquarius.Script;

package Aquarius.Actions is

   --  Action_Execution_Trigger: a number of action groups may be
   --  associated with a grammar (see Add_Action_Group, below).
   --  These actions can be executed manually (via command line flag
   --  or menu drop-down), or if certain triggers are met.  The four
   --  predefined triggers are definied here:

   --  Parse_Trigger: the action is executed while the tree is being
   --  built (normally while the parser is running).  The exact time
   --  of execution is slightly complicated (see below).

   --  Changed_Trigger: actions performed when changes are made to
   --  a node, e.g. renaming an identifier

   --  Semantic_Trigger: the action is executed when the tree is
   --  being checked.  This normally happens after the parse is
   --  finished, or when new nodes are added.

   --  Code_Trigger: the action is executed in order to generate
   --  code for the tree.

   --  Manual_Trigger: the action can only be executed manually.

   --  More on Parse_Trigger actions: these actions are generally
   --  used to collect semantic information about the tree being
   --  built in order to help the parser disambiguate.  Like other
   --  actions, they are set to run at a particular position in the
   --  tree; either before or after a given node, or before/after a
   --  particular child of a node.  However, instead of being
   --  initiated from a depth-first scan of the tree, they are
   --  run when the parse process does certain things; in particular:

   --  Move_To_Left_Of_First_Child causes the Before_Node action
   --  of the current tree (before moving) to execute.

   --  Move_To_Right_Of_Parent causes the After_Node action of the
   --  parent tree to execute.

   --  (others tbd)

   type Action_Execution_Trigger is
     (Parse_Trigger,
      Changed_Trigger,
      Semantic_Trigger,
      Code_Trigger,
      Manual_Trigger);

   type Action_Position is private;

   function Before_Node return Action_Position;
   function After_Node return Action_Position;
   function Show (Position : Action_Position) return String;

   type Action_Group is private;

   function Action_Group_Name (Group : Action_Group) return String;
   function Action_Group_Trigger (Group : Action_Group)
                                 return Action_Execution_Trigger;

   --  Actionable: something that actions can apply to
   type Actionable is interface;

   function Name (Item : Actionable) return String
      is abstract;

   function Image (Item : Actionable) return String
      is abstract;

   type Node_Action is access
     procedure (Item : not null access Actionable'Class);

   type Parent_Action is access
     procedure (Parent : not null access Actionable'Class;
                Child  : not null access Actionable'Class);

   --  Action_Source: something that has actions
   type Action_Source is interface;

   type Action_Instance_List is private;

   function Get_Action_List (Source : not null access Action_Source)
                            return Action_Instance_List
      is abstract;

   procedure Set_Action_List (Source : not null access Action_Source;
                              List   : Action_Instance_List)
      is abstract;

   function Parent_Actionable (Child    : not null access Actionable;
                               Parent   : not null access Action_Source'Class)
                              return access Actionable'Class
      is abstract;

   --  Every actionable object has a corresponding action source
   function Actionable_Source (Item : Actionable)
                              return access Action_Source'Class
      is abstract;

   function Name (Source : Action_Source) return String
      is abstract;

   function Before_Child (Child : not null access Action_Source'Class)
                         return Action_Position;

   function After_Child (Child : not null access Action_Source'Class)
                        return Action_Position;

   procedure Set_Action (Source   : not null access Action_Source'Class;
                         Group    : in     Action_Group;
                         Position : in     Rule_Position;
                         Action   : in     Node_Action);

   procedure Set_Action (Source   : not null access Action_Source'Class;
                         Child    : not null access Action_Source'Class;
                         Group    : in     Action_Group;
                         Position : in     Rule_Position;
                         Action   : in     Parent_Action);

   procedure Set_Action (Source   : not null access Action_Source'Class;
                         Group    : in     Action_Group;
                         Position : in     Rule_Position;
                         Action   : in     Aquarius.Script.Aquarius_Script);

   procedure Set_Action (Source   : not null access Action_Source'Class;
                         Child    : not null access Action_Source'Class;
                         Group    : in     Action_Group;
                         Position : in     Rule_Position;
                         Action   : in     Aquarius.Script.Aquarius_Script);

   procedure Execute (Source   : in out Action_Source'Class;
                      Target   : in out Actionable'Class;
                      Group    : in     Action_Group;
                      Position : in     Rule_Position);

   type Action_Group_List is private;

   function Empty_Action_Group_List return Action_Group_List;

   procedure Create_Action_Group
     (List       : in out Action_Group_List;
      Group_Name : in     String;
      Trigger    : in     Action_Execution_Trigger;
      Group      :    out Action_Group);

   function Get_Group_Count (List : Action_Group_List)
                            return Natural;
   function Get_Group (List  : Action_Group_List;
                       Index : Positive)
                      return Action_Group;
   function Get_Group (List        : Action_Group_List;
                       Group_Name  : String)
                      return Action_Group;

   type Action_Context (<>) is private;

private

   type Action_Anchor_Type is (Node_Relative, Child_Relative);

   type Action_Position is
      record
         Pos_Type : Rule_Position;
         Anchor   : Action_Anchor_Type;
         Child    : access Action_Source'Class;
      end record;

   type Action_Group_Record is
      record
         Index         : Positive;
         Group_Name    : Aquarius.Names.Aquarius_Name;
         Group_Trigger : Action_Execution_Trigger;
      end record;

   type Action_Group is access Action_Group_Record;

   package Action_Group_Vector is
      new Ada.Containers.Vectors (Positive, Action_Group);

   type Action_Group_List is
      record
         Groups : Action_Group_Vector.Vector;
      end record;

   type Action_Instance is
      record
         Group         : Action_Group;
         Parent        : access Action_Source'Class;
         Position      : Rule_Position;
         Parent_Act    : Parent_Action;
         Node_Act      : Node_Action;
         Parent_Script : Aquarius.Script.Aquarius_Script;
         Node_Script   : Aquarius.Script.Aquarius_Script;

      end record;

   package Action_Instance_Vector is
     new Ada.Containers.Vectors (Positive, Action_Instance);

   type Action_Instance_List is access Action_Instance_Vector.Vector;

   type Action_Source_Access is access all Action_Source'Class;

   type Action_Context is array (Positive range <>) of Action_Source_Access;

end Aquarius.Actions;
