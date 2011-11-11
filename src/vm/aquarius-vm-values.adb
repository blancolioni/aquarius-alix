with Ada.Strings.Fixed;

with Aquarius.Names;

package body Aquarius.VM.Values is

   use Ada.Strings.Unbounded;

   function Get_Entry_Value
     (Env    : VM_Environment;
      Name   : String;
      Create : Boolean        := False)
     return VM_Value;
   --  Gets the entry value (with properties etc) associated with the
   --  name.  The "real" value is contained within this value.
   --  If Create is True and Name is not found, create a new entry
   --  with no value.

   ----------
   -- Cons --
   ----------

   function Cons (Head, Tail : VM_Value) return VM_Value is
   begin
      return new VM_Value_Record'(Val_Cons, Head, Tail);
   end Cons;

   -----------------
   -- Error_Value --
   -----------------

   function Error_Value (Message : String) return VM_Value is
   begin
      return new VM_Value_Record'(Val_Error, To_Unbounded_String (Message));
   end Error_Value;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Item : VM_Value;
      Env  : VM_Environment)
      return VM_Value
   is

      function Evaluate_List (List : VM_Value) return VM_Value;
      function List_To_Array (List : VM_Value) return Array_Of_Values;

      -------------------
      -- Evaluate_List --
      -------------------

      function Evaluate_List (List : VM_Value) return VM_Value is
         Args : constant Array_Of_Values := List_To_Array (List.Tail);
      begin
         return List.Head.Fn (Env, Args);
      end Evaluate_List;

      -------------------
      -- List_To_Array --
      -------------------

      function List_To_Array (List : VM_Value) return Array_Of_Values is
      begin
         if List = null then
            declare
               Result : Array_Of_Values (1 .. 0);
            begin
               return Result;
            end;
         else
            return Evaluate (List.Head, Env) & List_To_Array (List.Tail);
         end if;
      end List_To_Array;

      Result : VM_Value;
   begin
      case Item.Class is
         when Val_Error =>
            Result := Item;
         when Val_Integer =>
            Result := Item;
         when Val_Property =>
            Result := Item;
         when Val_Cons =>
            Result := Evaluate_List (Item);
         when Val_Entry =>
            Result := Evaluate (Item.Val_Value, Env);
         when Val_Primitive =>
            Result := Item;
      end case;
      return Result;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Item : VM_Value;
      Env  : VM_Environment)
   is
      Result : constant VM_Value := Evaluate (Item, Env);
      pragma Unreferenced (Result);
   begin
      null;
   end Evaluate;

   ---------------------
   -- Get_Entry_Value --
   ---------------------

   function Get_Entry_Value
     (Env  : VM_Environment;
      Name   : String;
      Create : Boolean        := False)
      return VM_Value
   is
      Key : constant Unbounded_String :=
        To_Unbounded_String (Name);
      It  : constant Env_Map.Cursor :=
        Env.Map.Find (Key);
      V   : VM_Value;
   begin
      if Env_Map.Has_Element (It) then
         return Env_Map.Element (It);
      else
         if Env.Parent /= null then
            V := Get_Entry_Value (Env.Parent, Name, False);
            if V /= null then
               return V;
            end if;
         end if;
         if Create then
            V := new VM_Value_Record'(Val_Entry, Key, null, null);
            Env.Map.Insert (Key, V);
            return V;
         end if;
      end if;

      return null;

   end Get_Entry_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Env  : VM_Environment;
      Name : String)
      return VM_Value
   is
      V : constant VM_Value := Get_Entry_Value (Env, Name);
   begin
      if V = null then
         return V;
      elsif V.Class /= Val_Entry then
         return Error_Value ("non-entry value found for " & Name);
      else
         return V.Val_Value;
      end if;
   end Get_Value;

   --------------
   -- Has_Tree --
   --------------

   function Has_Tree (Item : VM_Value) return Boolean is
   begin
      return Item.Class = Val_Property and then
        Item.Prop_Value.all in Aquarius.Trees.Root_Tree_Type'Class;
   end Has_Tree;

   ----------
   -- Head --
   ----------

   function Head (Item : VM_Value) return VM_Value is
   begin
      return Item.Head;
   end Head;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Env   : VM_Environment;
      Name  : String;
      Value : VM_Value)
   is
      Key : constant Unbounded_String := To_Unbounded_String (Name);
      V   : constant VM_Value :=
        new VM_Value_Record'(Val_Entry, Key, null, Value);
   begin
      Env.Map.Insert (Key, V);
   end Insert;

   ---------------
   -- Make_List --
   ---------------

   function Make_List (Items : Array_Of_Values) return VM_Value is
      It : VM_Value := Null_Value;
   begin
      for I in reverse Items'Range loop
         It := Cons (Items (I), It);
      end loop;
      return It;
   end Make_List;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment
     (Parent : VM_Environment)
      return VM_Environment
   is
      Result : constant VM_Environment := new VM_Environment_Record;
   begin
      Result.Parent := Parent;
      return Result;
   end New_Environment;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Env        : VM_Environment;
      Name       : String;
      Prop_Name  : String;
      Prop_Value : VM_Value)
   is
      V : constant VM_Value := Get_Entry_Value (Env, Name,
                                                Create => True);
   begin
      if V.Properties = null then
         V.Properties := New_Environment (Env);
      end if;
      Insert (V.Properties, Prop_Name, Prop_Value);
   end Set_Property;

   ----------
   -- Show --
   ----------

   function Show (Item : VM_Value) return String is
   begin
      if Item = null then
         return "NIL";
      end if;

      case Item.Class is
         when Val_Error =>
            return "Error: " &
            To_String (Item.Message);
         when Val_Integer =>
            return Ada.Strings.Fixed.Trim (Integer'Image (Item.Int_Value),
                                           Ada.Strings.Both);
         when Val_Property =>
            return Item.Prop_Value.Name;
         when Val_Cons =>
            return '(' & Show (Item.Head) & " . " & Show (Item.Tail) & ')';
         when Val_Entry =>
            return '[' & To_String (Item.Val_Name) &
              ": " & Show (Item.Val_Value) & "]";
         when Val_Primitive =>
            return "(primitive)";
      end case;
   end Show;

   ----------
   -- Tail --
   ----------

   function Tail (Item : VM_Value) return VM_Value is
   begin
      return Item.Tail;
   end Tail;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Value : VM_Value) return Boolean is
   begin
      if Value = null then
         return False;
      end if;

      case Value.Class is
         when Val_Error =>
            raise Constraint_Error with Show (Value);
         when Val_Integer =>
            return Value.Int_Value /= 0;
         when Val_Property =>
            return True;
         when Val_Cons =>
            return True;
         when Val_Entry =>
            return To_Boolean (Value.Val_Value);
         when Val_Primitive =>
            raise Constraint_Error with "Can't convert primitive to boolean";
      end case;
   end To_Boolean;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : VM_Value) return Integer is
   begin
      if Value = null then
         return 0;
      end if;

      case Value.Class is
         when Val_Error =>
            raise Constraint_Error with Show (Value);
         when Val_Integer =>
            return Value.Int_Value;
         when others =>
            raise Constraint_Error with
              "cannot convert " & Show (Value) & " to an integer";
      end case;
   end To_Integer;

   -----------------
   -- To_Property --
   -----------------

   function To_Property
     (Value : VM_Value)
      return access Root_Aquarius_Object'Class
   is
   begin
      if Value = null then
         return null;
      elsif Value.Class = Val_Property then
         return Value.Prop_Value;
      else
         raise Constraint_Error with
           "cannot convert " & Show (Value) &
         " to a property";
      end if;
   end To_Property;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : VM_Value) return String is
   begin
      return Show (Value);
   end To_String;

   -------------
   -- To_Tree --
   -------------

   function To_Tree (Item : VM_Value) return Aquarius.Trees.Tree is
   begin
      pragma Assert (Has_Tree (Item));
      return Aquarius.Trees.Tree (Item.Prop_Value);
   end To_Tree;

   --------------
   -- To_Value --
   --------------

   function To_Value (Item : Boolean) return VM_Value is
   begin
      return To_Value (Boolean'Pos (Item));
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value (Item : Integer) return VM_Value is
   begin
      return new VM_Value_Record'(Val_Integer, Item);
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value (Item : String) return VM_Value is
   begin
      return new VM_Value_Record'(Val_Property,
                                  Aquarius.Names.Name_Value (Item));
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value
     (Item : access Root_Aquarius_Object'Class)
      return VM_Value
   is
   begin
      return new VM_Value_Record'(Val_Property, Item);
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value
     (Item     : Evaluator;
      Defaults : Array_Of_Values)
      return VM_Value
   is
      pragma Unreferenced (Defaults);
   begin
      return new VM_Value_Record'(Val_Primitive, Item);
   end To_Value;

end Aquarius.VM.Values;
