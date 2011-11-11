with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Tags;

package body Aquarius.Types.Maps is

   type From_Type_Entry is
      record
         From_Type : Aquarius_Type;
         Optional  : Boolean;
      end record;

   package Type_Vector is
      new Ada.Containers.Vectors (Positive, From_Type_Entry);

   type Map_Type_Record is new Root_Aquarius_Type with
      record
         From : Type_Vector.Vector;
         To   : Aquarius_Type;
      end record;

   type Map_Type is access all Map_Type_Record'Class;

   overriding
   function Description (Item : Map_Type_Record)
                 return String;

   overriding
   function Create_Derived_Type (Item : Map_Type_Record)
                                return Aquarius_Type;

   overriding
   function Unify (Item      : not null access Map_Type_Record;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type;

   type Void_Type_Record is new Root_Aquarius_Type with null record;

   type Void_Type is access all Void_Type_Record'Class;

   overriding
   function Description (Item : Void_Type_Record)
                 return String;

   overriding
   function Create_Derived_Type (Item : Void_Type_Record)
                                return Aquarius_Type;

   overriding
   function Unify (Item      : not null access Void_Type_Record;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type;

   Local_Void_Type : Void_Type;

   -------------------
   -- Add_From_Type --
   -------------------

   procedure Add_From_Type (To        : access Root_Aquarius_Type'Class;
                            From_Type : access Root_Aquarius_Type'Class;
                            Optional  : in     Boolean)
   is
   begin
      Map_Type_Record (To.all).From.Append
        ((Aquarius_Type (From_Type), Optional));
   end Add_From_Type;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type (Item : Map_Type_Record)
                                return Aquarius_Type
   is
   begin
      return new Map_Type_Record'(Item);
   end Create_Derived_Type;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type (Item : Void_Type_Record)
                                return Aquarius_Type
   is
   begin
      return new Void_Type_Record'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Map_Type_Record)
                        return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      if Item.From.Last_Index > 0 then
         for I in 1 .. Item.From.Last_Index loop
            if I > 1 then
               Result := Result & " x ";
            end if;
            Result := Result & Item.From.Element (I).From_Type.Description;
         end loop;

         Result := Result & " -> ";
      end if;

      if Item.To = null then
         return To_String (Result) & "()";
      else
         return To_String (Result) & Item.To.Description;
      end if;
   end Description;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Void_Type_Record)
                        return String
   is
      pragma Unreferenced (Item);
   begin
      return "()";
   end Description;

   -----------------------
   -- Get_Argument_Type --
   -----------------------

   function Get_Argument_Count (Item  : access Root_Aquarius_Type'Class)
                               return Natural
   is
      V      : Type_Vector.Vector renames Map_Type (Item).From;
   begin
      return V.Last_Index;
   end Get_Argument_Count;

   -----------------------
   -- Get_Argument_Type --
   -----------------------

   function Get_Argument_Type (Item  : Aquarius_Type;
                               Index : Positive)
                              return Aquarius_Type
   is
   begin
      return Map_Type (Item).From.Element (Index).From_Type;
   end Get_Argument_Type;

   ------------------------
   -- Get_Argument_Types --
   ------------------------

   function Get_Argument_Types (Item : Aquarius_Type)
                               return Array_Of_Types
   is
      V      : constant Type_Vector.Vector :=
        Map_Type (Item).From;
      Result : Array_Of_Types (1 .. V.Last_Index);
   begin
      for I in Result'Range loop
         Result (I) := V.Element (I).From_Type;
      end loop;
      return Result;
   end Get_Argument_Types;

   ---------------------
   -- Get_Result_Type --
   ---------------------

   function Get_Result_Type (Item : Aquarius_Type)
                            return Aquarius_Type
   is
   begin
      if not Is_Map_Type (Item) then
         raise Constraint_Error with
           "not a map type: " & Ada.Tags.External_Tag (Item.all'Tag);
      end if;
      return Map_Type (Item).To;
   end Get_Result_Type;

   -------------------
   -- Get_Void_Type --
   -------------------

   function Get_Void_Type return Aquarius_Type is
   begin
      if Local_Void_Type = null then
         Local_Void_Type := new Void_Type_Record;
      end if;
      return Aquarius_Type (Local_Void_Type);
   end Get_Void_Type;

   -----------------
   -- Is_Map_Type --
   -----------------

   function Is_Map_Type (Item : Aquarius_Type) return Boolean is
   begin
      return Item.all in Map_Type_Record'Class;
   end Is_Map_Type;

   ------------------
   -- New_Map_Type --
   ------------------

   function New_Map_Type (From : Array_Of_Types;
                          To   : Aquarius_Type)
                         return Aquarius_Type
   is
      Result : constant Map_Type := new Map_Type_Record;
   begin
      for I in From'Range loop
         Result.From.Append ((From (I), False));
      end loop;
      Result.To := To;
      return Aquarius_Type (Result);
   end New_Map_Type;

   ------------------
   -- New_Map_Type --
   ------------------

   function New_Map_Type (To   : Aquarius_Type)
                         return Aquarius_Type
   is
      From   : Array_Of_Types (1 .. 0);
   begin
      return New_Map_Type (From, To);
   end New_Map_Type;

   ------------------
   -- New_Map_Type --
   ------------------

   function New_Map_Type return Aquarius_Type
   is
      From   : Array_Of_Types (1 .. 0);
   begin
      return New_Map_Type (From, null);
   end New_Map_Type;

   ---------------------
   -- Set_Return_Type --
   ---------------------

   procedure Set_Return_Type (For_Map_Type : access Root_Aquarius_Type'Class;
                              Return_Type  : access Root_Aquarius_Type'Class)
   is
   begin
      Map_Type_Record (For_Map_Type.all).To := Aquarius_Type (Return_Type);
   end Set_Return_Type;

   -----------
   -- Unify --
   -----------

   overriding
   function Unify (Item      : not null access Map_Type_Record;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type
   is
      Right      : Map_Type;
      Result     : Map_Type_Record;
   begin
      if With_Type.all not in Map_Type_Record'Class then
         return null;
      end if;

      Right := Map_Type (With_Type);

      if Item.From.Last_Index /= Right.From.Last_Index then
         return null;
      end if;

      if Item.To = null then
         if Right.To /= null then
            return null;
         else
            Result.To := Unify (Item.To, Right.To);
            if Result.To = null then
               return null;
            end if;
         end if;
      else
         if Right.To /= null then
            return null;
         end if;
      end if;

      for I in 1 .. Item.From.Last_Index loop
         declare
            Unified : constant Aquarius_Type :=
              Unify (Item.From.Element (I).From_Type,
                     Right.From.Element (I).From_Type);
         begin
            if Unified = null then
               return null;
            end if;
         end;
      end loop;

      return new Map_Type_Record'(Result);
   end Unify;

   -----------
   -- Unify --
   -----------

   overriding
   function Unify (Item      : not null access Void_Type_Record;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type
   is
   begin
      if With_Type.all not in Void_Type_Record'Class then
         return null;
      end if;

      return Aquarius_Type (Item);
   end Unify;

end Aquarius.Types.Maps;
