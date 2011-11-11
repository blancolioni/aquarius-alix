with Ada.Containers.Vectors;

with Aquarius.Entries.Objects;
with Aquarius.Types.Discrete;
with Aquarius.Types.Maps;
with Aquarius.Values;

package body Aquarius.Types.Enumeration is

   type Literal_Item is
      record
         Literal_Entry : Aquarius.Entries.Table_Entry;
         Literal_Char  : Character;
      end record;

   package Literal_Vector is
      new Ada.Containers.Vectors (Positive, Literal_Item);

   type Root_Enumeration_Type is
     new Aquarius.Types.Discrete.Root_Discrete_Type with
      record
         Literal_Type : Aquarius_Type;
         Literals     : Literal_Vector.Vector;
      end record;

   overriding
   function Create_Derived_Type
     (Item : Root_Enumeration_Type)
     return Aquarius_Type;

   overriding
   function Description
     (Item : Root_Enumeration_Type)
     return String;

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   overriding
   function Create_Derived_Type
     (Item : Root_Enumeration_Type)
     return Aquarius_Type
   is
   begin
      return new Root_Enumeration_Type'(Item);
   end Create_Derived_Type;

   -----------------
   -- Description --
   -----------------

   overriding
   function Description
     (Item : Root_Enumeration_Type)
     return String
   is
      pragma Unreferenced (Item);
   begin
      return "an enumeration type";
   end Description;

   -----------------
   -- Get_Literal --
   -----------------

   function Get_Literal (From_Enumeration : Aquarius_Type;
                         Name             : String)
                        return Aquarius.Entries.Table_Entry
   is
      T : Root_Enumeration_Type renames
        Root_Enumeration_Type (From_Enumeration.all);
   begin
      for I in 1 .. T.Literals.Last_Index loop
         if not Aquarius.Entries.Is_Null
           (T.Literals.Element (I).Literal_Entry)
           and then T.Literals.Element (I).Literal_Entry.Name = Name
         then
            return T.Literals.Element (I).Literal_Entry;
         end if;
      end loop;
      return null;
   end Get_Literal;

   -----------------------------
   -- New_Enumeration_Literal --
   -----------------------------

   function New_Enumeration_Literal
     (Enumeration : Aquarius_Type;
      Declaration : access Aquarius.Trees.Root_Tree_Type'Class;
      Name        : String)
     return Aquarius.Entries.Table_Entry
   is
      T : Root_Enumeration_Type renames
        Root_Enumeration_Type (Enumeration.all);
      Literal : constant Aquarius.Entries.Table_Entry :=
        Aquarius.Entries.Objects.New_Object_Entry
        (Name           => Name,
         Declaration    => Declaration,
         Entry_Type     => T.Literal_Type,
         Entry_Value    => Aquarius.Values.To_Value (T.Literals.Last_Index),
         Constant_Value => True);
   begin
      T.Literals.Append ((Literal, Character'Val (0)));
      return Literal;
   end New_Enumeration_Literal;

   -----------------------------
   -- New_Enumeration_Literal --
   -----------------------------

   procedure New_Enumeration_Literal
     (Enumeration : in     Aquarius_Type;
      Item        : in     Character)
   is
      T : Root_Enumeration_Type renames
        Root_Enumeration_Type (Enumeration.all);
   begin
      T.Literals.Append ((null, Item));
   end New_Enumeration_Literal;

   --------------------------
   -- New_Enumeration_Type --
   --------------------------

   function New_Enumeration_Type return Aquarius_Type is
      Result : constant Aquarius_Type := new Root_Enumeration_Type;
   begin
      Root_Enumeration_Type (Result.all).Literal_Type :=
        Aquarius.Types.Maps.New_Map_Type (Result);
      return Result;
   end New_Enumeration_Type;

end Aquarius.Types.Enumeration;
