package body Aquarius.Plugins.Klein.Types is

   Local_Universal_Integer : Aquarius_Type;
   Local_Universal_Boolean : Aquarius_Type;

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component (To_Record : Aquarius.Types.Aquarius_Type;
                            Component : Aquarius.Entries.Table_Entry)
   is
   begin
      Record_Type (To_Record.all).Components.Insert (Component);
   end Add_Component;

   -----------------
   -- Add_Literal --
   -----------------

   procedure Add_Literal
     (To_Enumeration : in Aquarius.Types.Aquarius_Type;
      Name           : in String;
      Declaration    : in Aquarius.Programs.Program_Tree)
   is
      Enum : Enumeration_Type renames
        Enumeration_Type (To_Enumeration.all);
   begin
      Enum.Literals.Append
        ((Ada.Strings.Unbounded.To_Unbounded_String (Name),
          Enum.Literals.Last_Index,
          Declaration));
      if 2**To_Enumeration.Size_Bits < Enum.Literals.Last_Index then
         To_Enumeration.Set_Default_Size (To_Enumeration.Size_Bits + 1);
      end if;
   end Add_Literal;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type (Item : Root_Klein_Type)
                                return Aquarius.Types.Aquarius_Type
   is
      pragma Unreferenced (Item);
   begin
      return null;
   end Create_Derived_Type;

   -----------------------------
   -- Create_Enumeration_Type --
   -----------------------------

   function Create_Enumeration_Type return Aquarius.Types.Aquarius_Type is
      Result : constant Aquarius.Types.Aquarius_Type :=
        new Enumeration_Type;
   begin
      Result.Set_Default_Size (0);
      return Result;
   end Create_Enumeration_Type;

   -------------------------
   -- Create_Integer_Type --
   -------------------------

   function Create_Integer_Type (Start, Bound : Integer)
                                return Aquarius.Types.Aquarius_Type
   is
      Int_Type : Signed_Integer_Type;
      Result   : Aquarius.Types.Aquarius_Type;
      Size     : Natural := 0;
   begin
      Int_Type.Start     := Start;
      Int_Type.Bound     := Bound;

      if Bound > Start then
         if Start <= Integer'First / 2 or else Bound >= Integer'Last / 2 then
            if Start >= 0 or else Bound < 0 then
               Size := Integer'Size - 1;
            else
               Size := Integer'Size;
            end if;
         else
            declare
               I : Integer := Bound - Start;
            begin
               while I /= 0 loop
                  I := I / 2;
                  Size := Size + 1;
               end loop;
            end;
         end if;
      end if;

      Result := new Signed_Integer_Type'(Int_Type);
      Aquarius.Types.Set_Default_Size (Result.all, Size);
      return Result;
   end Create_Integer_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Access_Type) return String is
   begin
      return "access to " & Item.Access_To.Name;
   end Description;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Enumeration_Type)
                 return String
   is
      pragma Unreferenced (Item);
   begin
      return "an enumeration type";
   end Description;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Universal_Integer_Type)
                 return String
   is
      pragma Unreferenced (Item);
   begin
      return "universal integer";
   end Description;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Signed_Integer_Type)
                 return String
   is
      pragma Unreferenced (Item);
   begin
      return "a signed integer type";
   end Description;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Unsigned_Integer_Type)
                 return String
   is
      pragma Unreferenced (Item);
   begin
      return "an unsigned integer type";
   end Description;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Record_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "a record type";
   end Description;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description (Item : Error_Type)
                 return String
   is
      pragma Unreferenced (Item);
   begin
      return "an error type";
   end Description;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component (From_Record : Aquarius.Types.Aquarius_Type;
                           Name        : String)
                          return Aquarius.Entries.Table_Entry
   is
   begin
      return Record_Type (From_Record.all).Components.Retrieve (Name);
   end Get_Component;

   --------------------
   -- New_Error_Type --
   --------------------

   function New_Error_Type
     (Restrictions : Aquarius.Types.Aquarius_Type)
     return Aquarius.Types.Aquarius_Type
   is
      Result : Error_Type;
   begin
      Result.Restrictions := Restrictions;
      return new Error_Type'(Result);
   end New_Error_Type;

   ---------------------
   -- New_Record_Type --
   ---------------------

   function New_Record_Type return Aquarius.Types.Aquarius_Type is
      Result : Record_Type;
   begin
      Result.Components :=
        Aquarius.Entries.New_Symbol_Table ("record components");
      return new Record_Type'(Result);
   end New_Record_Type;

   -----------------------
   -- Set_Literal_Value --
   -----------------------

   procedure Set_Literal_Value
     (In_Enumeration : in Aquarius.Types.Aquarius_Type;
      Name           : in String;
      Value          : in Natural)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      Enum   : Enumeration_Type renames
        Enumeration_Type (In_Enumeration.all);
   begin
      for I in 1 .. Enum.Literals.Last_Index loop
         if Enum.Literals.Element (I).Name = Name then
            Enum.Literals.Replace_Element
              (I, (Enum.Literals.Element (I).Name,
                   Value,
                   Enum.Literals.Element (I).Declaration));
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "enumeration type does not contain literal '" & Name & "'";
   end Set_Literal_Value;

   -----------
   -- Unify --
   -----------

   overriding
   function Unify (Item      : not null access Universal_Integer_Type;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type
   is
      pragma Unreferenced (Item);
   begin
      if With_Type.all in Integer_Type'Class then
         return Aquarius_Type (With_Type);
      else
         return null;
      end if;
   end Unify;

   -----------
   -- Unify --
   -----------

   overriding
   function Unify (Item      : not null access Error_Type;
                   With_Type : not null access Root_Aquarius_Type'Class)
                  return Aquarius_Type
   is
      pragma Unreferenced (Item);
   begin
      return Aquarius_Type (With_Type);
   end Unify;

   -----------------------
   -- Universal_Boolean --
   -----------------------

   function Universal_Boolean return Aquarius.Types.Aquarius_Type is
   begin
      if Local_Universal_Boolean = null then
         Local_Universal_Boolean := Create_Enumeration_Type;
         Add_Literal (Universal_Boolean, "false", null);
         Add_Literal (Universal_Boolean, "true", null);
         Local_Universal_Boolean :=
           Aquarius.Types.New_Named_Type ("Boolean",
                                          Universal_Boolean);
      end if;

      return Local_Universal_Boolean;
   end Universal_Boolean;

   -----------------------
   -- Universal_Integer --
   -----------------------

   function Universal_Integer return Aquarius.Types.Aquarius_Type is
   begin
      if Local_Universal_Integer = null then
         Local_Universal_Integer := new Universal_Integer_Type;
         Local_Universal_Integer.Set_Universal;
      end if;

      return Local_Universal_Integer;
   end Universal_Integer;

end Aquarius.Plugins.Klein.Types;
