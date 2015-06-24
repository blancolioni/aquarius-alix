with Ada.Strings.Unbounded;

with Aqua.IO;
with Aqua.Primitives;
with Aqua.Words;

package body Aqua.Objects is

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (Object : in out Root_Object_Type;
      Name   : in String)
      return Word
   is
      Result : Word;
   begin
      if Object.Map.Contains (Name) then
         Result := Object.Map.Element (Name);
      else
         declare
            Object_Primitive_Name : constant String :=
                                      "object__" & Name;
            Object_Primitive      : constant Subroutine_Reference :=
                                      Aqua.Primitives.Get_Primitive
                                        (Object_Primitive_Name);
         begin
            if Object_Primitive /= 0 then
               Result := Aqua.Words.To_Subroutine_Word (Object_Primitive);
            else
               Result := 0;
            end if;
         end;
      end if;
      return Result;
   end Get_Property;

   ------------------
   -- Has_Property --
   ------------------

   overriding function Has_Property
     (Object : in Root_Object_Type;
      Name   : in String)
      return Boolean
   is
   begin
      return Object.Map.Contains (Name)
        or else Aqua.Primitives.Get_Primitive
          (Name => "object__" & Name) /= 0;
   end Has_Property;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (It       : in out Root_Object_Iterator;
      Finished :    out Boolean)
   is
   begin
      if Object_Maps.Has_Element (It.Position) then
         It.Current := Object_Maps.Element (It.Position);
         Object_Maps.Next (It.Position);
         Finished := False;
      else
         Finished := True;
      end if;
   end Next;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (Object : in out Root_Object_Type;
      Name   : in     String;
      Value  : in     Word)
   is
   begin
      if Object.Map.Contains (Name) then
         Object.Map.Replace (Name, Value);
      else
         Object.Map.Insert (Name, Value);
      end if;
   end Set_Property;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Object : Root_Object_Type)
      return String
   is
      use Ada.Strings.Unbounded;
      use Object_Maps;
      Result : Unbounded_String;
   begin
      for Position in Object.Map.Iterate loop
         if Result = Null_Unbounded_String then
            Result := To_Unbounded_String ("(");
         else
            Result := Result & ", ";
         end if;
         Result := Result
           & Key (Position) & " => " & Aqua.IO.Hex_Image (Element (Position));
      end loop;
      if Result = Null_Unbounded_String then
         return "()";
      else
         return To_String (Result) & ")";
      end if;
   end Show;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Object : Root_Object_Type)
      return Aqua.Iterators.Aqua_Iterator_Interface'Class
   is
   begin
      return Result : Root_Object_Iterator do
         Result.Position := Object.Map.First;
         Result.Current  := 0;
      end return;
   end Start;

end Aqua.Objects;
