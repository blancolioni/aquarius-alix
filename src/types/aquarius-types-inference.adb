with Aquarius.Errors;
with Aquarius.Properties;

with Aquarius.Entries.Objects;
with Aquarius.Types.Errors;
with Aquarius.Types.Maps;

pragma Elaborate_All (Aquarius.Properties);

package body Aquarius.Types.Inference is

   Inferred_Types_Property : constant Aquarius.Properties.Property_Type :=
     Aquarius.Properties.Inferred_Types_Property;

   Possible_Types_Property : constant Aquarius.Properties.Property_Type :=
     Aquarius.Properties.Possible_Types_Property;

   type Function_Constraint is
     new Aquarius.Entries.Name_Constraint with
      record
         Result_Types : Possible_Types;
         Arg_Count    : Natural;
      end record;

   overriding
   function Match
     (Constraint : Function_Constraint;
      Item       : access Aquarius.Entries.Table_Entry_Record'Class)
     return Boolean;

   procedure Initialise_Constraint
     (Constraint   : in out Function_Constraint'Class;
      Name         : in     String;
      Result_Types : in     Possible_Types;
      Arg_Count    : in     Natural);

   -----------------
   -- Check_Types --
   -----------------

   procedure Check_Types
     (Item : not null access Aquarius.Programs.Program_Tree_Type'Class)
   is
      use Aquarius.Types;
   begin
      if not Has_Possible_Types (Item)
        or else not Has_Inferred_Types (Item)
      then
         return;
      end if;
      declare
         Possible : constant Possible_Types := Get_Possible_Types (Item);
         Inferred : constant Possible_Types := Get_Inferred_Types (Item);
         Result   : Possible_Types;
      begin
         Result := Possible.Unify (Inferred);

         if Result.Count = 0 then
            if not Item.Has_Property
              (Aquarius.Properties.Type_Error_Property)
            then
               Aquarius.Errors.Error (Item, "expected type " & Possible.Name);
               Aquarius.Errors.Error (Item, "found type " & Inferred.Name);
               Item.Set_Property (Aquarius.Properties.Type_Error_Property);
            end if;
         else
            Set_Inferred_Types (Item, Result);
         end if;
      end;
   end Check_Types;

   -----------------------------
   -- Get_Function_Candidates --
   -----------------------------

   function Get_Function_Candidates
     (Table          : Aquarius.Entries.Symbol_Table;
      Name           : String;
      Argument_Count : Natural;
      Result_Type    : Possible_Types)
     return Aquarius.Entries.Array_Of_Entries
   is
      Constraint : Function_Constraint;
   begin
      Initialise_Constraint (Constraint, Name, Result_Type, Argument_Count);
      return Aquarius.Entries.Search (Table, Constraint);
   end Get_Function_Candidates;

   ------------------------
   -- Get_Inferred_Types --
   ------------------------

   function Get_Inferred_Types
     (Tree : not null access Aquarius.Programs.Program_Tree_Type'Class)
     return Possible_Types
   is
      use Aquarius.Types;
   begin
      return Possible_Types (Tree.Property (Inferred_Types_Property));
   end Get_Inferred_Types;

   ------------------------
   -- Get_Possible_Types --
   ------------------------

   function Get_Possible_Types
     (Tree : not null access Aquarius.Programs.Program_Tree_Type'Class)
     return Possible_Types
   is
      use Aquarius.Types;
   begin
      if not Tree.Has_Property (Possible_Types_Property) then
         return null;
--           Tree.Set_Property (Possible_Types_Property,
--                              new Possible_Type_Record);
      end if;

      return Possible_Types (Tree.Property (Possible_Types_Property));
   end Get_Possible_Types;

   ------------------------
   -- Has_Inferred_Types --
   ------------------------

   function Has_Inferred_Types
     (Tree : not null access Aquarius.Programs.Program_Tree_Type'Class)
     return Boolean
   is
      use Aquarius.Types;
   begin
      return Tree.Has_Property (Inferred_Types_Property);
   end Has_Inferred_Types;

   ------------------------
   -- Has_Possible_Types --
   ------------------------

   function Has_Possible_Types
     (Tree : not null access Aquarius.Programs.Program_Tree_Type'Class)
     return Boolean
   is
      use Aquarius.Types;
   begin
      return Tree.Has_Property (Possible_Types_Property);
   end Has_Possible_Types;

   ---------------------------
   -- Initialise_Constraint --
   ---------------------------

   procedure Initialise_Constraint
     (Constraint   : in out Function_Constraint'Class;
      Name         : in     String;
      Result_Types : in     Possible_Types;
      Arg_Count    : in     Natural)
   is
   begin
      Aquarius.Entries.Initialise_Constraint
        (Aquarius.Entries.Name_Constraint (Constraint),
         Name);
      Constraint.Result_Types := Result_Types;
      Constraint.Arg_Count    := Arg_Count;
   end Initialise_Constraint;

   -----------
   -- Match --
   -----------

   overriding
   function Match
     (Constraint : Function_Constraint;
      Item       : access Aquarius.Entries.Table_Entry_Record'Class)
     return Boolean
   is
      use Aquarius.Entries, Aquarius.Entries.Objects;
      use Aquarius.Types, Maps;
   begin
      if not Match (Name_Constraint (Constraint), Item) then
         return False;
      end if;
      if not Is_Object_Entry (Item) then
         return False;
      end if;

      if Aquarius.Types.Errors.Is_Error_Type (Object_Entry_Type (Item)) then
         return False;
      end if;

      declare
         Function_Type : constant Aquarius_Type :=
                           Object_Entry_Type (Item);
      begin
         if not Aquarius.Types.Maps.Is_Map_Type (Function_Type) then
            return False;
         end if;

         if not Unifies (Constraint.Result_Types,
                         Get_Result_Type (Function_Type))
         then
            return False;
         end if;

         if Get_Argument_Count (Object_Entry_Type (Item)) /=
           Constraint.Arg_Count
         then
            return False;
         end if;
      end;

      return True;
   end Match;

   ------------------------
   -- Set_Inferred_Types --
   ------------------------

   procedure Set_Inferred_Types
     (Tree  : not null access Aquarius.Programs.Program_Tree_Type'Class;
      Types : in Possible_Types)
   is
   begin
      Tree.Set_Property (Inferred_Types_Property, Types);
   end Set_Inferred_Types;

   ------------------------
   -- Set_Possible_Types --
   ------------------------

   procedure Set_Possible_Types
     (Tree  : not null access Aquarius.Programs.Program_Tree_Type'Class;
      Types : in Possible_Types)
   is
   begin
      Tree.Set_Property (Possible_Types_Property, Types);
   end Set_Possible_Types;

end Aquarius.Types.Inference;
