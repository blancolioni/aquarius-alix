with Ada.Text_IO;

with Aqua.IO;
with Aqua.Iterators;
with Aqua.Objects;
with Aqua.Primitives;
with Aqua.Words;

package body Aqua.CPU.Traps is

   Trace_Properties : constant Boolean := False;

   -------------------------
   -- Handle_Get_Property --
   -------------------------

   procedure Handle_Get_Property
     (CPU   : in out Aqua_CPU_Type'Class)
   is
      Argument_Count : constant Natural := Natural (Get_Integer (CPU.Pop));
      Arguments      : Array_Of_Words (1 .. Argument_Count);
      Name           : constant Word := CPU.Pop;
      Target         : Word;
      Value          : Word;
   begin

      for I in 1 .. Argument_Count loop
         Arguments (I) := CPU.Pop;
      end loop;

      Target := CPU.Pop;

      if not Is_String_Reference (Name) then
         raise Constraint_Error
           with "property name must be a string: "
           & Aqua.IO.Hex_Image (Name);
      end if;

      if Is_String_Reference (Target) then
         declare
            Prim : constant Subroutine_Reference :=
                     Aqua.Primitives.Get_Primitive
                       ("string__" & CPU.Image.To_String (Name));
         begin
            Value :=
              Aqua.Primitives.Call_Primitive
                (CPU, Prim, Target & Arguments (1 .. Argument_Count));
            if Trace_Properties then
               Ada.Text_IO.Put_Line
                 (CPU.Show (Target) & "." & CPU.Show (Name) & " -> "
                  & CPU.Show (Value));
            end if;
         end;
      elsif not Is_External_Reference (Target) then
         raise Constraint_Error
           with "property "
           & CPU.Show (Name)
           & " defined only on objects; found "
           & CPU.Show (Target);
      else
         declare
            Ext : constant access External_Object_Interface'Class :=
                    CPU.To_External_Object (Target);
         begin
            if Ext.all not in Aqua.Objects.Object_Interface'Class then
               raise Constraint_Error
                 with "property "
                 & CPU.Show (Name)
                 & " defined only on objects; found "
                 & "[" & Aqua.IO.Hex_Image (Target) & "] "
                 & CPU.Show (Target);
            end if;
         end;

         declare
            use Aqua.Objects;
            Target_Object : constant access Object_Interface'Class :=
                              Object_Interface'Class
                                (CPU.To_External_Object
                                   (Target).all)'Access;
            Property_Name : constant String :=
                              CPU.Image.To_String (Name);
         begin
            Value := Target_Object.Get_Property (Property_Name);
            if Trace_Properties then
               Ada.Text_IO.Put_Line
                 (Target_Object.Name & "." & CPU.Show (Name) & " -> "
                  & CPU.Show (Value));
            end if;
         end;

         if Aqua.Words.Is_Subroutine_Reference (Value) then
            Value :=
              Aqua.Primitives.Call_Primitive
                (CPU, Aqua.Words.Get_Subroutine_Reference (Value),
                 Target & Arguments (1 .. Argument_Count));
         end if;

      end if;

      CPU.Push (Value);
   end Handle_Get_Property;

   --------------------------
   -- Handle_Iterator_Next --
   --------------------------

   procedure Handle_Iterator_Next
     (CPU   : in out Aqua_CPU_Type'Class)
   is
      use Aqua.Iterators;
      Old_Value     : constant Word := CPU.Pop;
      pragma Unreferenced (Old_Value);
      Iterator_Word : constant Word := CPU.Pop;
      Iterator_Ext  : access External_Object_Interface'Class;
      Iterator      : access Aqua_Iterator_Interface'Class;
   begin
      if not Is_External_Reference (Iterator_Word) then
         raise Constraint_Error
           with "iterator_next: expected an object but found "
           & CPU.Show (Iterator_Word);
      end if;

      Iterator_Ext := CPU.To_External_Object (Iterator_Word);

      if Iterator_Ext.all not in Aqua_Iterator_Interface'Class then
         raise Constraint_Error
           with "iterator_next: cannot iterate over "
           & Iterator_Ext.Name;
      end if;

      Iterator :=
        Aqua_Iterator_Interface'Class (Iterator_Ext.all)'Access;

      if Trace_Properties then
         Ada.Text_IO.Put_Line
           ("Iterator_Next: old current = "
            & CPU.Show (Iterator.Current));
      end if;

      Iterator.Next (CPU.Z);
      if not CPU.Z then
         if Trace_Properties then
            Ada.Text_IO.Put_Line
              ("Iterator_Next: new current = "
               & " [" & Aqua.IO.Hex_Image (Iterator.Current) & "] "
               & CPU.Show (Iterator.Current));
         end if;
         CPU.Push (Iterator_Word);
         CPU.Push (Iterator.Current);
      else
         if Trace_Properties then
            Ada.Text_IO.Put_Line
              ("Iterator_Next: finished");
         end if;
      end if;
   end Handle_Iterator_Next;

   ---------------------------
   -- Handle_Iterator_Start --
   ---------------------------

   procedure Handle_Iterator_Start
     (CPU   : in out Aqua_CPU_Type'Class)
   is
      use Aqua.Iterators;
      Container_Word : constant Word := CPU.Pop;
      Container_Ext  : access External_Object_Interface'Class;
      Container      : access Aqua_Container_Interface'Class;
   begin
      if not Is_External_Reference (Container_Word) then
         raise Constraint_Error
           with "iterator_start: expected an object but found "
           & CPU.Show (Container_Word);
      end if;

      Container_Ext := CPU.To_External_Object (Container_Word);

      if Container_Ext.all not in Aqua_Container_Interface'Class then
         raise Constraint_Error
           with "iterator_start: cannot iterate over "
           & Container_Ext.Name;
      end if;

      Container :=
        Aqua_Container_Interface'Class (Container_Ext.all)'Access;

      if Trace_Properties then
         Ada.Text_IO.Put_Line ("iterating: " & Container.Show);
      end if;

      declare
         It : constant External_Object_Access :=
                new Aqua_Iterator_Interface'Class'
                  (Container.Start);
      begin
         CPU.Push (CPU.To_Word (It));
         CPU.Push (0);
      end;
   end Handle_Iterator_Start;

   -------------------------
   -- Handle_Set_Property --
   -------------------------

   procedure Handle_Set_Property
     (CPU   : in out Aqua_CPU_Type'Class)
   is
      Name   : constant Word := CPU.Pop;
      Target : constant Word := CPU.Pop;
      Value  : constant Word := CPU.Pop;
   begin
      if Trace_Properties then
         Ada.Text_IO.Put_Line
           ("set: "
            & CPU.Name (Target)
            & "."
            & CPU.Show (Name)
            & " <- "
            & CPU.Show (Value));
      end if;

      if not Is_String_Reference (Name) then
         raise Constraint_Error
           with "set: expected a string for property name, but found "
           & CPU.Show (Name);
      end if;

      if not Is_External_Reference (Target) then
         raise Constraint_Error
           with "set: expected an object but found "
           & CPU.Show (Target);
      end if;

      --                 pragma Assert (Is_String_Reference (Name));
      --                 pragma Assert (Is_External_Reference (Target));

      declare
         Ext : constant access External_Object_Interface'Class :=
                 CPU.To_External_Object (Target);
      begin
         if Ext.all not in Aqua.Objects.Object_Interface'Class then
            raise Constraint_Error
              with "property "
              & CPU.Show (Name)
              & " defined only on objects; found "
              & Ext.Name;
         end if;
      end;

      declare
         use Aqua.Objects;
         Target_Object : constant access Object_Interface'Class :=
                           Object_Interface'Class
                             (CPU.To_External_Object
                                (Target).all)'Access;
         Property_Name : constant String :=
                           CPU.Image.To_String (Name);
      begin
         Target_Object.Set_Property (Property_Name, Value);
      end;
   end Handle_Set_Property;

end Aqua.CPU.Traps;