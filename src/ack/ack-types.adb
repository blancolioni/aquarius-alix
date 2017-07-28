with WL.String_Maps;

with Ack.Classes;

package body Ack.Types is

   package Class_Type_Maps is
     new WL.String_Maps (Type_Entity);

   Class_Type_Map : Class_Type_Maps.Map;

   ---------------------
   -- Aliased_Feature --
   ---------------------

   function Aliased_Feature
     (Typ   : Type_Entity_Record'Class;
      Alias : Name_Id)
      return Ack.Features.Feature_Entity
   is
   begin
      return Typ.Class.Aliased_Feature (Alias);
   end Aliased_Feature;

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Typ  : Type_Entity_Record;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      Typ.Class.Allocate (Unit);
   end Allocate;

   -----------------
   -- Conforms_To --
   -----------------

   overriding function Conforms_To
     (Conformer : not null access constant Type_Entity_Record;
      Other     : not null access constant Root_Entity_Type'Class)
      return Boolean
   is
   begin
      if Constant_Type_Entity (Conformer) = Constant_Type_Entity (Other) then
         return True;
      elsif Conformer.Generic_Formal then
         if Conformer.Constraints.Is_Empty then
            return True;
         else
            for Constraint of Conformer.Constraints loop
               if Constraint.Conforms_To (Other) then
                  return True;
               end if;
            end loop;
            return False;
         end if;
      else
         return Conformer.Class.Conforms_To (Other);
      end if;
   end Conforms_To;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Typ       : Type_Entity_Record;
      Name      : String;
      Recursive : Boolean := True)
      return Boolean
   is
   begin
      return Root_Entity_Type (Typ).Contains (Name, Recursive)
        or else (Recursive and then Typ.Class.Contains (Name));
   end Contains;

   -----------------
   -- Description --
   -----------------

   overriding function Description
     (Typ       : Type_Entity_Record)
      return String
   is
   begin
      if Typ.Generic_Formal then
         return Typ.Declared_Name & " (a generic argument of "
           & Typ.Class.Description & ")";
      else
         return Typ.Full_Name & " (" & Typ.Class.Description & ")";
      end if;
   end Description;

   -------------
   -- Feature --
   -------------

   function Feature
     (Typ   : Type_Entity_Record'Class;
      Name  : Name_Id)
      return Ack.Features.Feature_Entity
   is
   begin
      return Ack.Features.Feature_Entity (Typ.Get (Name));
   end Feature;

   ---------------
   -- Full_Name --
   ---------------

   overriding function Full_Name
     (Typ       : Type_Entity_Record)
      return String
   is
      function Generic_Bindings_Image return String;

      ----------------------------
      -- Generic_Bindings_Image --
      ----------------------------

      function Generic_Bindings_Image return String is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;
      begin
         for Binding of Typ.Generic_Bindings loop
            if Result /= Null_Unbounded_String then
               Result := Result & ",";
            end if;
            Result := Result & Binding.Actual.Qualified_Name;
         end loop;
         return To_String (Result);
      end Generic_Bindings_Image;

      Detachable : constant String :=
                     (if Typ.Detachable
                      then "detachable " else "");

   begin
      if Typ.Generic_Bindings.Is_Empty then
         return Detachable
           & Type_Entity_Record'Class (Typ).Qualified_Name;
      else
         return Detachable
           & Type_Entity_Record'Class (Typ).Qualified_Name
           & "[" & Generic_Bindings_Image & "]";
      end if;
   end Full_Name;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Typ  : not null access constant Type_Entity_Record;
      Name : String)
      return Entity_Type
   is

      Ancestor_Type : Constant_Type_Entity;

      function Instantiate_Entity
        (Entity : Entity_Type)
         return Entity_Type;

      ------------------------
      -- Instantiate_Entity --
      ------------------------

      function Instantiate_Entity
        (Entity : Entity_Type)
         return Entity_Type
      is
      begin
         for Binding of Ancestor_Type.Generic_Bindings loop
            if Constant_Type_Entity (Entity) = Binding.Formal then
               return Entity_Type (Binding.Actual);
            end if;
         end loop;
         return Entity;
      end Instantiate_Entity;

   begin
      if Root_Entity_Type (Typ.all).Contains (Name) then
         return Root_Entity_Type (Typ.all).Get (Name);
      else
         declare
            use type Ack.Classes.Constant_Class_Entity;
            Feature          : constant Ack.Features.Feature_Entity :=
                                 Typ.Class.Feature
                                   (Get_Name_Id (Name));
            Definition_Class : constant Ack.Classes.Constant_Class_Entity :=
                                 Feature.Definition_Class;
         begin
            Ancestor_Type :=
              (if Definition_Class = Typ.Class
               then Constant_Type_Entity (Typ)
               else Typ.Class.Get_Ancestor_Type
                 (Definition_Class));

            if Ancestor_Type.Generic_Bindings.Is_Empty then
               Typ.Insert (Feature);
               return Entity_Type (Feature);
            else
               declare
                  Instantiated_Entity : constant Entity_Type :=
                                          Feature.Instantiate
                                            (Instantiate_Entity'Access);
               begin
                  Typ.Insert (Instantiated_Entity);
                  return Instantiated_Entity;
               end;
            end if;
         end;
      end if;
   end Get;

   --------------------
   -- Get_Class_Type --
   --------------------

   function Get_Class_Type
     (Class : not null access Ack.Classes.Class_Entity_Record'Class)
      return Type_Entity
   is
      Key : constant String := Class.Full_Name;
   begin
      if not Class_Type_Map.Contains (Key) then
         Class_Type_Map.Insert
           (Key, New_Class_Type (Class.Declaration_Node, Class, False));
      end if;
      return Class_Type_Map.Element (Key);
   end Get_Class_Type;

   ------------------------
   -- Get_Top_Level_Type --
   ------------------------

   function Get_Top_Level_Type
     (Name : String)
      return Type_Entity
   is
   begin
      return Get_Class_Type (Ack.Classes.Get_Top_Level_Class (Name));
   end Get_Top_Level_Type;

   ---------------------
   -- Get_Type_Entity --
   ---------------------

   function Get_Type_Entity
     (Node : Node_Id)
      return Type_Entity
   is
   begin
      return Type_Entity (Get_Entity (Node));
   end Get_Type_Entity;

   -------------------------
   -- Has_Aliased_Feature --
   -------------------------

   function Has_Aliased_Feature
     (Typ   : Type_Entity_Record'Class;
      Alias : Name_Id)
      return Boolean
   is
   begin
      return Typ.Class.Has_Aliased_Feature (Alias);
   end Has_Aliased_Feature;

   -----------------
   -- Has_Feature --
   -----------------

   function Has_Feature
     (Typ   : Type_Entity_Record'Class;
      Name  : Name_Id)
      return Boolean
   is
   begin
      return Typ.Class.Has_Feature (Name);
   end Has_Feature;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Entity             : not null access Type_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type
   is
   begin
      return Type_Instantiation (Entity_Type (Entity));
   end Instantiate;

   ------------------------------
   -- Instantiate_Generic_Type --
   ------------------------------

   function Instantiate_Generic_Class
     (Node            : Node_Id;
      Generic_Class   : not null access Ack.Classes.Class_Entity_Record'Class;
      Generic_Actuals : Array_Of_Types)
      return Type_Entity
   is
   begin
      return Result : constant Type_Entity := new Type_Entity_Record'
        (Root_Entity_Type with
         Class               => Generic_Class,
         Generic_Bindings    => <>,
         Constraints         => <>,
         Generic_Formal      => False,
         Detachable          => False,
         Anchored            => False)
      do
         Result.Create
           (Get_Name_Id (Generic_Class.Declared_Name), Node, Table => True);
         for I in Generic_Actuals'Range loop
            Result.Generic_Bindings.Append
              (Generic_Argument_Binding'
                 (Formal =>
                      Constant_Type_Entity (Generic_Class.Generic_Formal (I)),
                  Actual => Generic_Actuals (I)));
         end loop;

      end return;

   end Instantiate_Generic_Class;

   ---------------
   -- Link_Name --
   ---------------

   overriding function Link_Name
     (Typ       : Type_Entity_Record)
      return String
   is
   begin
      return Typ.Class.Link_Name;
   end Link_Name;

   --------------------
   -- New_Class_Type --
   --------------------

   function New_Class_Type
     (Node            : Node_Id;
      Class           : not null access
        Ack.Classes.Class_Entity_Record'Class;
      Detachable      : Boolean)
      return Type_Entity
   is
   begin
      return Result : constant Type_Entity := new Type_Entity_Record'
        (Root_Entity_Type with
         Class               => Class,
         Generic_Bindings    => <>,
         Constraints         => <>,
         Generic_Formal      => False,
         Detachable          => Detachable,
         Anchored            => False)
      do
         Result.Create (Get_Name_Id (Class.Declared_Name), Node,
                        Table => True);
      end return;
   end New_Class_Type;

   -----------------------------
   -- New_Generic_Formal_Type --
   -----------------------------

   function New_Generic_Formal_Type
     (Name          : Name_Id;
      Node          : Node_Id;
      Generic_Class : not null access Ack.Classes.Class_Entity_Record'Class;
      Constraints   : Array_Of_Types := Empty_Type_Array)
      return Type_Entity
   is
   begin
      return Result : constant Type_Entity := new Type_Entity_Record'
        (Root_Entity_Type with
         Class               => null,
         Generic_Bindings    => <>,
         Constraints         => <>,
         Generic_Formal      => False,
         Detachable          => False,
         Anchored            => False)
      do
         Result.Create (Name, Node, Table => False);
         Result.Class := Generic_Class;
         Result.Generic_Formal := True;

         for Constraint of Constraints loop
            Result.Constraints.Append (Constraint);
         end loop;

      end return;
   end New_Generic_Formal_Type;

end Ack.Types;
