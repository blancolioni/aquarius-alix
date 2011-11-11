with Ada.Strings.Fixed;

package body Aquarius.Names is

   ---------
   -- "=" --
   ---------

   function "=" (Left  : Aquarius_Name;
                 Right : String)
                return Boolean
   is
   begin
      return To_String (Left) = Right;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left  : String;
                 Right : Aquarius_Name)
                return Boolean
   is
   begin
      return Left = To_String (Right);
   end "=";

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (Left, Right : Aquarius_Name) return Boolean is
   begin
      return To_String (Left) = To_String (Right);
   end "=";

   ------------
   -- Append --
   ------------

   procedure Append (To   : in out Name_List;
                     Name : in     String)
   is
   begin
      To.Names.Append (To_Aquarius_Name (Name));
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Item : in out Name_List) is
   begin
      Item.Names.Clear;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains (Item : Name_List;
                      Name : String)
                     return Boolean
   is
   begin
      for I in 1 .. Count (Item) loop
         if Element (Item, I) = Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   -----------
   -- Count --
   -----------

   function Count (Item : Name_List) return Natural is
   begin
      return Item.Names.Last_Index;
   end Count;

   -------------
   -- Element --
   -------------

   function Element (Item  : Name_List;
                     Index : Positive)
                    return String
   is
   begin
      return To_String (Item.Names.Element (Index));
   end Element;

   -----------
   -- Empty --
   -----------

   function Empty return Name_List is
   begin
      return (Names => Name_Vectors.Empty_Vector);
   end Empty;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Aquarius_Name_Value)
                 return String
   is
   begin
      return To_String (Item.Value);
   end Name;

   ----------------
   -- Name_Value --
   ----------------

   function Name_Value (Name : String)
                       return access Aquarius_Name_Value'Class
   is
   begin
      return new Aquarius_Name_Value'(Value => To_Aquarius_Name (Name));
   end Name_Value;

   ------------------------
   -- Null_Aquarius_Name --
   ------------------------

   function Null_Aquarius_Name return Aquarius_Name is
      Result : Aquarius_Name;
   begin
      return Result;
   end Null_Aquarius_Name;

   ----------------------
   -- To_Aquarius_Name --
   ----------------------

   function To_Aquarius_Name (Item : String) return Aquarius_Name is
      use Ada.Strings.Fixed;
   begin
      if Item'Length > Max_Short_Length then
         return (Long, new String'(Item));
      else
         declare
            Text : String (1 .. Max_Short_Length) := (others => ' ');
         begin
            Text (1 .. Item'Length) := Item;
            return (Short, Text);
         end;
      end if;
   end To_Aquarius_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Aquarius_Name) return String is
   begin
      case Item.Length_Class is
         when Short =>
            return Ada.Strings.Fixed.Trim (Item.Text, Ada.Strings.Right);
         when Long =>
            return Item.Long_Text.all;
      end case;
   end To_String;

end Aquarius.Names;
