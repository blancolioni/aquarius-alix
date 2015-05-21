with Ada.Exceptions;
with Ada.Text_IO;

package body Aquarius.Actions is

   function New_Instance_List return Action_Instance_List;

   -----------------------
   -- Action_Group_Name --
   -----------------------

   function Action_Group_Name (Group : Action_Group) return String is
   begin
      return Aquarius.Names.To_String (Group.Group_Name);
   end Action_Group_Name;

   --------------------------
   -- Action_Group_Trigger --
   --------------------------

   function Action_Group_Trigger (Group : Action_Group)
                                 return Action_Execution_Trigger
   is
   begin
      return Group.Group_Trigger;
   end Action_Group_Trigger;

   -----------------
   -- After_Child --
   -----------------

   function After_Child (Child : not null access Action_Source'Class)
                        return Action_Position
   is
   begin
      return (After, Child_Relative, Child);
   end After_Child;

   ----------------
   -- After_Node --
   ----------------

   function After_Node return Action_Position is
   begin
      return (After, Node_Relative, null);
   end After_Node;

   ------------------
   -- Before_Child --
   ------------------

   function Before_Child (Child : not null access Action_Source'Class)
                         return Action_Position
   is
   begin
      return (Before, Child_Relative, Child);
   end Before_Child;

   ----------------
   -- Before_Node --
   ----------------

   function Before_Node return Action_Position is
   begin
      return (Before, Node_Relative, null);
   end Before_Node;

   -------------------------
   -- Create_Action_Group --
   -------------------------

   procedure Create_Action_Group
     (List       : in out Action_Group_List;
      Group_Name : in     String;
      Trigger    : in     Action_Execution_Trigger;
      Group      :    out Action_Group)
   is
   begin
      Group := new Action_Group_Record'
        (Index         => List.Groups.Last_Index + 1,
         Group_Name    => Aquarius.Names.To_Aquarius_Name (Group_Name),
         Group_Trigger => Trigger);
      List.Groups.Append (Group);
   end Create_Action_Group;

   -----------------------------
   -- Empty_Action_Group_List --
   -----------------------------

   function Empty_Action_Group_List return Action_Group_List is
      Result : Action_Group_List;
   begin
      return Result;
   end Empty_Action_Group_List;

   -------------
   -- Execute --
   -------------

   procedure Execute (Source   : in out Action_Source'Class;
                      Target   : in out Actionable'Class;
                      Group    : in     Action_Group;
                      Position : in     Rule_Position)
   is
      Instances : constant Action_Instance_List :=
        Source.Get_Action_List;

      --  if Parent_Actions is True, only execute parent
      --  actions.  Otherwise, only execute node actions.
      procedure Exec (Parent_Actions : Boolean);

      ----------
      -- Exec --
      ----------

      procedure Exec (Parent_Actions : Boolean) is
         use type Aquarius.Script.Aquarius_Script;
      begin

         for I in 1 .. Instances.Last_Index loop

            declare
               Instance : Action_Instance renames Instances.Element (I);
            begin
               if Instance.Position = Position and then
                 Instance.Group = Group
               then
                  if Parent_Actions and then
                    Instance.Parent_Act /= null
                  then
                     declare
                        Parent : constant access Actionable'Class :=
                          Target.Parent_Actionable (Instance.Parent);
                     begin
                        if Parent /= null then
                           Instance.Parent_Act (Parent, Target'Access);
                        end if;
                     end;
                  end if;
                  if Parent_Actions and then
                    Instance.Parent_Script /= null
                  then
                     declare
                        Parent : constant access Actionable'Class :=
                          Target.Parent_Actionable (Instance.Parent);
                     begin
                        if Parent /= null then
                           Instance.Parent_Script.Execute
                             (Target'Access, Parent);
                        end if;
                     end;
                  end if;
                  if not Parent_Actions and then
                    Instance.Node_Act /= null
                  then
                     begin
                        Instance.Node_Act (Target'Access);
                     exception
                        when E : others =>
                           Ada.Text_IO.Put_Line
                             (Ada.Text_IO.Standard_Error,
                              "caught exception while running actions");
                           Ada.Text_IO.Put_Line
                             (Ada.Text_IO.Standard_Error,
                              "  target: " & Target.Image);
                           Ada.Text_IO.Put_Line
                             (Ada.Text_IO.Standard_Error,
                              "   exception message: " &
                              Ada.Exceptions.Exception_Message (E));
                           raise;
                     end;

                  end if;
                  if not Parent_Actions and then
                    Instance.Node_Script /= null
                  then
                     Instance.Node_Script.Execute (Target'Access, null);
                  end if;
               end if;
            end;
         end loop;
      end Exec;

   begin

      if Instances = null then
         return;
      end if;

      --  if position is before, then parent actions
      --  have priority, otherwise node actions do

      if Position = Before then
         Exec (True);
         Exec (False);
      else
         Exec (False);
         Exec (True);
      end if;

   end Execute;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group (List  : Action_Group_List;
                       Index : Positive)
                      return Action_Group
   is
   begin
      return List.Groups.Element (Index);
   end Get_Group;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group (List        : Action_Group_List;
                       Group_Name  : String)
                       return Action_Group
   is
      use type Aquarius.Names.Aquarius_Name;
   begin
      for I in 1 .. List.Groups.Last_Index loop
         if List.Groups.Element (I).Group_Name = Group_Name then
            return List.Groups.Element (I);
         end if;
      end loop;
      raise Constraint_Error with
        "expected to find a group called '" & Group_Name & "'";
   end Get_Group;

   ---------------------
   -- Get_Group_Count --
   ---------------------

   function Get_Group_Count (List : Action_Group_List)
                            return Natural
   is
   begin
      return List.Groups.Last_Index;
   end Get_Group_Count;

   -----------------------
   -- New_Instance_List --
   -----------------------

   function New_Instance_List return Action_Instance_List is
   begin
      return new Action_Instance_Vector.Vector;
   end New_Instance_List;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action (Source   : not null access Action_Source'Class;
                         Group    : in     Action_Group;
                         Position : in     Rule_Position;
                         Action   : in     Node_Action)
   is
      use type Aquarius.Script.Aquarius_Script;
      Instances : Action_Instance_List := Source.Get_Action_List;
   begin
      if Instances = null then
         Instances := New_Instance_List;
         Source.Set_Action_List (Instances);
      end if;

      for I in 1 .. Instances.Last_Index loop
         declare
            Instance : Action_Instance := Instances.Element (I);
         begin
            if Instance.Group = Group
              and then Instance.Position = Position
              and then Instance.Node_Act = null
              and then Instance.Node_Script = null
            then
               Instance.Node_Act := Action;
               Instances.Replace_Element (I, Instance);
               return;
            end if;
         end;
      end loop;
      Instances.Append ((Group, null, Position, null, Action, null, null));
   end Set_Action;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action (Source   : not null access Action_Source'Class;
                         Group    : in     Action_Group;
                         Position : in     Rule_Position;
                         Action   : in     Aquarius.Script.Aquarius_Script)
   is
      use type Aquarius.Script.Aquarius_Script;
      Instances : Action_Instance_List := Source.Get_Action_List;
   begin
      if Instances = null then
         Instances := New_Instance_List;
         Source.Set_Action_List (Instances);
      end if;

      for I in 1 .. Instances.Last_Index loop
         declare
            Instance : Action_Instance := Instances.Element (I);
         begin
            if Instance.Group = Group and then
              Instance.Position = Position and then
              Instance.Node_Act = null and then
              Instance.Node_Script = null
            then
               Instance.Node_Script := Action;
               Instances.Replace_Element (I, Instance);
               return;
            end if;
         end;
      end loop;
      Instances.Append ((Group, null, Position, null, null, null, Action));
   end Set_Action;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action (Source   : not null access Action_Source'Class;
                         Child    : not null access Action_Source'Class;
                         Group    : in     Action_Group;
                         Position : in     Rule_Position;
                         Action   : in     Parent_Action)
   is
      use type Aquarius.Script.Aquarius_Script;
      Instances : Action_Instance_List :=
        Child.Get_Action_List;
   begin
      if Instances = null then
         Instances := New_Instance_List;
         Child.Set_Action_List (Instances);
      end if;

      for I in 1 .. Instances.Last_Index loop
         declare
            Instance : Action_Instance := Instances.Element (I);
         begin
            if Instance.Group = Group
              and then Instance.Position = Position
              and then Instance.Parent_Act = null
              and then Instance.Parent_Script = null
            then
               Instance.Parent_Act := Action;
               Instance.Parent     := Source;
               Instances.Replace_Element (I, Instance);
               return;
            end if;
         end;
      end loop;
      Instances.Append ((Group         => Group,
                         Parent        => Source,
                         Position      => Position,
                         Parent_Act    => Action,
                         Node_Act      => null,
                         Parent_Script => null,
                         Node_Script   => null));
   end Set_Action;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action (Source   : not null access Action_Source'Class;
                         Child    : not null access Action_Source'Class;
                         Group    : in     Action_Group;
                         Position : in     Rule_Position;
                         Action   : in     Aquarius.Script.Aquarius_Script)
   is
      use type Aquarius.Script.Aquarius_Script;
      Instances : Action_Instance_List :=
        Child.Get_Action_List;
   begin
      if Instances = null then
         Instances := New_Instance_List;
         Child.Set_Action_List (Instances);
      end if;

      for I in 1 .. Instances.Last_Index loop
         declare
            Instance : Action_Instance := Instances.Element (I);
         begin
            if Instance.Group = Group
              and then Instance.Position = Position
              and then Instance.Parent_Act = null
              and then Instance.Parent_Script = null
            then
               Instance.Parent_Script := Action;
               Instance.Parent        := Source;
               Instances.Replace_Element (I, Instance);
               return;
            end if;
         end;
      end loop;
      Instances.Append ((Group         => Group,
                         Parent        => Source,
                         Position      => Position,
                         Parent_Act    => null,
                         Node_Act      => null,
                         Parent_Script => Action,
                         Node_Script   => null));
   end Set_Action;

   ----------
   -- Show --
   ----------

   function Show (Position : Action_Position) return String is
   begin
      case Position.Anchor is
         when Node_Relative =>
            case Position.Pos_Type is
               when Before =>
                  return "before node";
               when After =>
                  return "after node";
            end case;
         when Child_Relative =>
            case Position.Pos_Type is
               when Before =>
                  return "before child '" & Position.Child.Name & "'";
               when After =>
                  return "after child '" & Position.Child.Name & "'";
            end case;
      end case;
   end Show;

end Aquarius.Actions;
