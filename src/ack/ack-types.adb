with Ack.Classes;

package body Ack.Types is

   -----------------
   -- Conforms_To --
   -----------------

   overriding function Conforms_To
     (Conformer : not null access constant Type_Entity_Record;
      Other     : not null access constant Root_Entity_Type'Class)
      return Boolean
   is
   begin
      return Conformer.Class.Conforms_To
        (Constant_Type_Entity (Other).Class);
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
        or else Typ.Class.Contains (Name);
   end Contains;

   -------------
   -- Feature --
   -------------

   function Feature
     (Typ   : Type_Entity_Record'Class;
      Name  : Name_Id)
      return Ack.Features.Feature_Entity
   is
   begin
      return Typ.Class.Feature (Name);
   end Feature;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Typ  : Type_Entity_Record;
      Name : String)
      return Entity_Type
   is
   begin
      if Root_Entity_Type (Typ).Contains (Name) then
         return Root_Entity_Type (Typ).Get (Name);
      else
         return Typ.Class.Get (Name);
      end if;
   end Get;

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
         Detachable          => False,
         Anchored            => False)
      do
         Result.Create (Get_Name_Id (Generic_Class.Declared_Name), Node);
         for I in Generic_Actuals'Range loop
            Result.Generic_Bindings.Append
              (Generic_Argument_Binding'
                 (Formal =>
                      Constant_Type_Entity (Generic_Class.Generic_Formal (I)),
                  Actual => Generic_Actuals (I)));
         end loop;

      end return;

   end Instantiate_Generic_Class;

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
         Detachable          => Detachable,
         Anchored            => False)
      do
         Result.Create (Get_Name_Id (Class.Declared_Name), Node);
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
         Detachable          => False,
         Anchored            => False)
      do
         Result.Create (Name, Node);
         Result.Class := Generic_Class;

         for Constraint of Constraints loop
            Result.Constraints.Append (Constraint);
         end loop;

      end return;
   end New_Generic_Formal_Type;

end Ack.Types;
