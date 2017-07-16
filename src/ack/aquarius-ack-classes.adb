with Ada.Containers.Ordered_Sets;

with Aquarius.Ack.Primitives;

package body Aquarius.Ack.Classes is

   package Entity_Id_Sets is
     new Ada.Containers.Ordered_Sets (Entity_Id);

   ---------------
   -- Add_Class --
   ---------------

   procedure Add_Class
     (Context : Entity_Id;
      Name    : Name_Id;
      Class   : Node_Id)
   is
      Entity : constant Entity_Id :=
                 New_Entity (Name, Class_Entity, Context, Class, No_Entity);
   begin
      Entity_Table (Entity).Entity_Type := Entity;
      Node_Table (Class).Entity := Entity;
   end Add_Class;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class
     (Context : Node_Id;
      Name    : Name_Id)
      return Node_Id
   is (No_Node);

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From
     (Ancestor   : Entity_Id;
      Descendent : Entity_Id)
      return Boolean
   is
      Tried : Entity_Id_Sets.Set;

      function Try (Class : Entity_Id) return Boolean;

      ---------
      -- Try --
      ---------

      function Try (Class : Entity_Id) return Boolean is
      begin
         if Class = Ancestor then
            return True;
         elsif Tried.Contains (Class) then
            return False;
         elsif Get_Kind (Class) = Generic_Argument_Entity then
            return False;
         else
            declare
               Declaration  : constant Node_Id :=
                                Get_Declaration (Class);
               Inherit_Node : constant Node_Id :=
                                (if Declaration = No_Node then No_Node
                                 else Inheritance (Declaration));
               Inherit_List : constant List_Id :=
                                (if Inherit_Node = No_Node
                                 then No_List
                                 else Inherits (Inherit_Node));
            begin
               Tried.Insert (Class);
               if Inherit_List /= No_List then
                  for Parent_Node of List_Table (Inherit_List).List loop
                     if Try (Get_Entity (Parent_Node)) then
                        return True;
                     end if;
                  end loop;
               end if;
               return False;
            end;
         end if;
      end Try;

   begin
      return Ancestor = Descendent
        or else Ancestor = Aquarius.Ack.Primitives.Any_Class
        or else Descendent = Aquarius.Ack.Primitives.None_Class
        or else (Get_Kind (Ancestor) = Class_Entity
                 and then Try (Descendent));
   end Is_Derived_From;

end Aquarius.Ack.Classes;
