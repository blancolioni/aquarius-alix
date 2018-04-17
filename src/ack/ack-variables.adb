with Ack.Classes;
with Ack.Features;
with Ack.Types;

package body Ack.Variables is

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Entity             : not null access Variable_Entity_Record;
      Type_Instantiation : not null access
        function (Generic_Type : Entity_Type) return Entity_Type)
      return Entity_Type
   is
      Result : constant Variable_Entity :=
                 new Variable_Entity_Record'(Entity.all);
   begin
      Result.Value_Type := Result.Value_Type.Instantiate (Type_Instantiation);
      return Entity_Type (Result);
   end Instantiate;

   -------------------------
   -- New_Argument_Entity --
   -------------------------

   function New_Argument_Entity
     (Name          : Name_Id;
      Node          : Node_Id;
      Argument_Type : not null access Root_Entity_Type'Class)
      return Variable_Entity
   is
   begin
      return Result : constant Variable_Entity :=
        new Variable_Entity_Record
      do
         Result.Create (Name, Node, Table => False);
         Result.Kind := Argument;
         Result.Value_Type := Entity_Type (Argument_Type);
         if not Argument_Type.Detachable then
            Result.Set_Attached;
         end if;
      end return;
   end New_Argument_Entity;

   -------------------------
   -- New_Iterator_Entity --
   -------------------------

   function New_Iterator_Entity
     (Name           : Name_Id;
      Node           : Node_Id;
      Iteration_Type : not null access Root_Entity_Type'Class;
      Local_Type     : not null access Root_Entity_Type'Class)
      return Variable_Entity
   is
   begin
      return Result : constant Variable_Entity :=
        new Variable_Entity_Record
      do
         Result.Create (Name, Node, Table => False);
         Result.Kind := Local;
         Result.Value_Type := Entity_Type (Local_Type);
         Result.Iterator := True;
         Result.Iteration := Entity_Type (Iteration_Type);
      end return;
   end New_Iterator_Entity;

   ----------------------
   -- New_Local_Entity --
   ----------------------

   function New_Local_Entity
     (Name       : Name_Id;
      Node       : Node_Id;
      Local_Type : not null access Root_Entity_Type'Class)
      return Variable_Entity
   is
   begin
      return Result : constant Variable_Entity :=
        new Variable_Entity_Record
      do
         Result.Create (Name, Node, Table => False);
         Result.Kind := Local;
         Result.Value_Type := Entity_Type (Local_Type);
      end return;
   end New_Local_Entity;

   ----------------
   -- Pop_Entity --
   ----------------

   overriding procedure Pop_Entity
     (Variable : Variable_Entity_Record;
      Unit     : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      case Variable.Kind is
         when Local =>
            Unit.Pop_Local
              (Tagatha.Local_Offset (Variable.Offset),
               Tagatha.Default_Size);

         when Argument =>
            Unit.Pop_Argument
              (Tagatha.Argument_Offset (Variable.Offset),
               Tagatha.Default_Size);
      end case;
   end Pop_Entity;

   -----------------
   -- Push_Entity --
   -----------------

   overriding procedure Push_Entity
     (Variable      : Variable_Entity_Record;
      Have_Current  : Boolean;
      Context       : not null access constant Root_Entity_Type'Class;
      Unit          : in out Tagatha.Units.Tagatha_Unit)
   is
      pragma Unreferenced (Have_Current);
      pragma Unreferenced (Context);
   begin
      case Variable.Kind is
         when Local =>
            Unit.Push_Local
              (Tagatha.Local_Offset (Variable.Offset),
               Tagatha.Default_Size);

            if Variable.Iterator then
               declare
                  Class : constant Ack.Classes.Class_Entity :=
                            Ack.Classes.Class_Entity
                              (Ack.Types.Type_Entity
                                 (Variable.Iteration).Class);
                  Feature : constant Ack.Features.Feature_Entity :=
                              Class.Feature
                                (Get_Name_Id ("element"));
               begin
                  Feature.Push_Entity
                    (Have_Current => True,
                     Context      => Class,
                     Unit         => Unit);
               end;
            end if;

         when Argument =>
            Unit.Push_Argument
              (Tagatha.Argument_Offset (Variable.Offset),
               Tagatha.Default_Size);
      end case;
   end Push_Entity;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset
     (Variable : in out Variable_Entity_Record'Class;
      Offset   : Positive)
   is
   begin
      Variable.Offset := Offset;
   end Set_Offset;

end Ack.Variables;
