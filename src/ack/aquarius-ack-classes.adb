with Ada.Containers.Ordered_Sets;

with Aquarius.Ack.Primitives;

package body Aquarius.Ack.Classes is

   package Entity_Id_Sets is
     new Ada.Containers.Ordered_Sets (Entity_Id);

   function Get_Local_Entity
     (Context : Entity_Id;
      Entity  : Entity_Id)
      return Entity_Id;

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

   ----------------------
   -- Get_Local_Entity --
   ----------------------

   function Get_Local_Entity
     (Context : Entity_Id;
      Entity  : Entity_Id)
      return Entity_Id
   is
   begin
      case Get_Kind (Entity) is
         when Generic_Argument_Entity =>
            if Get_Context (Entity) = Context then
               return Entity;
            else
               declare
                  Name : constant String :=
                           Get_Link_Name (Get_Context (Entity))
                           & "--formal--"
                           & Get_Link_Name (Entity);
                  Value : constant Entity_Id :=
                            Find_Local_Entity (Context, Get_Name_Id (Name));
               begin
                  if Value = Undeclared_Entity then
                     raise Constraint_Error with
                       "expected to find an instantiation of "
                       & Get_Description (Entity)
                       & " in " & Get_Description (Context);
                  end if;
                  return Value;
               end;
            end if;

         when others =>
            return Entity;
      end case;
   end Get_Local_Entity;

   -----------------------
   -- Instantiate_Class --
   -----------------------

   function Instantiate_Class
     (Class   : Entity_Id;
      Context : Entity_Id;
      Actuals : Node_Id)
      return Entity_Id
   is
      function Generic_Actuals_Name
        (Current : List_Of_Nodes.Cursor)
         return String;

      --------------------------
      -- Generic_Actuals_Name --
      --------------------------

      function Generic_Actuals_Name
        (Current : List_Of_Nodes.Cursor)
         return String
      is
      begin
         if not List_Of_Nodes.Has_Element (Current) then
            return "";
         end if;

         declare
            Entity : constant Entity_Id :=
                       Get_Entity (List_Of_Nodes.Element (Current));
            Name   : constant String := To_String (Get_Name (Entity));
            Rest   : constant String :=
                       Generic_Actuals_Name
                         (List_Of_Nodes.Next (Current));
         begin
            return "--" & Name & Rest;
         end;
      end Generic_Actuals_Name;

--        Formals_Node : constant Node_Id :=
--                         Formal_Generics
--                           (Class_Header
--                              (Get_Declaration (Class)));
      List         : constant List_Id := Actual_Generics_List (Actuals);
      Name         : constant String :=
                       Get_Link_Name (Class)
                       & Generic_Actuals_Name (List_Table (List).List.First);
      Id           : constant Name_Id := Get_Name_Id (Name);
      Result       : Entity_Id :=
                       Find_Local_Entity (Context, Id);
   begin
      if Result = Undeclared_Entity then
         Result := New_Entity
           (Name        => Get_Name_Id (Name),
            Kind        => Instantiated_Class_Entity,
            Context     => Context,
            Declaration => Actuals,
            Entity_Type => Class);

         declare
            use List_Of_Nodes;
            Formal_List : constant List_Id :=
                            Formal_Generics_List
                              (Formal_Generics
                                 (Class_Header
                                    (Get_Declaration (Class))));
            Actual_List : constant List_Id :=
                            Actual_Generics_List (Actuals);
            Formal      : Cursor := List_Table (Formal_List).List.First;
            Actual      : Cursor := List_Table (Actual_List).List.First;
         begin
            while Has_Element (Formal) and then Has_Element (Actual) loop
               declare
                  Actual_Entity : constant Entity_Id :=
                                    Get_Entity (Element (Actual));
                  Formal_Entity : constant Entity_Id :=
                                    Get_Entity (Element (Formal));
               begin
                  Instantiate_Entity
                    (Generic_Class  => Class,
                     Concrete_Class => Context,
                     Formal_Entity  => Formal_Entity,
                     Actual_Entity  => Actual_Entity,
                     Declaration    => Element (Actual));
               end;
               Next (Formal);
               Next (Actual);
            end loop;
         end;

--           declare
--              procedure Add_Retyped_Child
--                (Child : Entity_Id);
--
--              -----------------------
--              -- Add_Retyped_Child --
--              -----------------------
--
--              procedure Add_Retyped_Child
--                (Child : Entity_Id)
--              is
--              begin
--                 Ada.Text_IO.Put_Line
--                   (Name & ": instantiating: "
--                    & Get_Description (Child));
--                 if Get_Kind (Child) in Feature_Entity_Kind then
--                    Entity_Table (Result).Children.Append (Child)
--                      (Instantiate_Feature
--                         (Child, Formals_Node, Actuals));
--                 end if;
--              end Add_Retyped_Child;
--
--           begin
--              Scan_Children (Class, Add_Retyped_Child'Access);
--           end;

         Entity_Table (Result).Entity_Type := Result;
         Entity_Table (Result).Instantiated_From := Class;
      end if;
      return Result;
   end Instantiate_Class;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From
     (Context    : Entity_Id;
      Ancestor   : Entity_Id;
      Descendent : Entity_Id)
      return Boolean
   is
      Tried : Entity_Id_Sets.Set;

      function Try (Class : Entity_Id) return Boolean
        with Pre => Get_Kind (Class) in Class_Entity_Kind;

      Local_Ancestor   : constant Entity_Id :=
                           Get_Local_Entity (Context, Ancestor);
      Local_Descendent : constant Entity_Id :=
                           Get_Local_Entity (Context, Descendent);

      ---------
      -- Try --
      ---------

      function Try (Class : Entity_Id) return Boolean is
      begin
         if Class = Local_Ancestor then
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
        or else (Get_Kind (Local_Ancestor) in Class_Entity_Kind
                 and then Try (Local_Descendent));
   end Is_Derived_From;

end Aquarius.Ack.Classes;
