with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;
with Ada.Tags;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Aquarius.Names;
--  with Aquarius.Programs;

package body Aquarius.VM is

   use Ada.Strings.Unbounded;

   package Environment_Lists is
     new Ada.Containers.Doubly_Linked_Lists (VM_Environment);

   Active_Environments : Environment_Lists.List;

   package Value_Lists is
     new Ada.Containers.Doubly_Linked_Lists (VM_Value);

   Free_Value_List   : Value_Lists.List;
   Active_Value_List : Value_Lists.List;

   function Get_Entry_Value
     (Env    : VM_Environment;
      Name   : String;
      Create : Boolean        := False)
     return VM_Value;
   --  Gets the entry value (with properties etc) associated with the
   --  name.  The "real" value is contained within this value.
   --  If Create is True and Name is not found, create a new entry
   --  with no value.

   -----------
   -- Apply --
   -----------

   function Apply (Item : VM_Value;
                   Env  : VM_Environment;
                   Args : Array_Of_Values)
                   return VM_Value
   is
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
            Result := Item;
         when Val_Entry =>
            Result := Item;
         when Val_Primitive =>
            Result := Item;
         when Val_Class =>
            Result := Item;
         when Val_Method_Call =>
            declare
               Class : constant String := Class_Name (Item.Val_Object);
               Lib_Name : constant String :=
                            Class & "__" & To_String (Item.Val_Method);
               Lib_Value : constant VM_Value :=
                             Get_Value (Env, Lib_Name);
            begin
               if Lib_Value /= null then
                  Result := Apply (Lib_Value.Val_Value, Env,
                                   Item.Val_Object & Args);
               else
                  raise Constraint_Error with
                    "object " & To_String (Item.Val_Object)
                    & " has no method named '"
                    & To_String (Item.Val_Method);
               end if;
            end;
      end case;
      return Result;
   end Apply;

   ----------------
   -- Class_Name --
   ----------------

   function Class_Name (Item : VM_Value) return String is
   begin
      case Item.Class is
         when Val_Error =>
            return "error";
         when Val_Integer =>
            return "integer";
         when Val_Property =>
            return Ada.Tags.External_Tag (Item.Prop_Value.all'Tag);
         when Val_Cons =>
            return "list";
         when Val_Entry =>
            return "entry";
         when Val_Primitive =>
            return "primitive";
         when Val_Method_Call =>
            return "method-call";
         when Val_Class =>
            return "class";
      end case;
   end Class_Name;

   ----------
   -- Cons --
   ----------

   function Cons (Head, Tail : VM_Value) return VM_Value is
   begin
      return New_Value ((Val_Cons, Head, Tail));
   end Cons;

   -----------------
   -- Error_Value --
   -----------------

   function Error_Value (Message : String) return VM_Value is
   begin
      return New_Value ((Val_Error, To_Unbounded_String (Message)));
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
            if Item.Arg_Count = 0 then
               declare
                  No_Args : Array_Of_Values (1 .. 0);
               begin
                  Result := Item.Fn (Env, No_Args);
               end;
            else
               Result := Item;
            end if;
         when Val_Method_Call =>
            if Has_Tree (Item.Val_Object)
              and then
                To_Tree (Item.Val_Object).Has_Property
                (To_String (Item.Val_Method))
            then
               Result := To_Value
                 (To_Tree (Item.Val_Object).Property
                  (To_String (Item.Val_Method)));
            else
               declare
                  Class     : constant String := Class_Name (Item.Val_Object);
                  Lib_Name  : constant String :=
                                Class & "__" & To_String (Item.Val_Method);
                  Lib_Value : constant VM_Value :=
                                Get_Value (Env, Lib_Name);
               begin
                  if Lib_Value /= null then
                     if Lib_Value.Arg_Count = 1 then
                        Result := Lib_Value.Fn (Env, (1 => Item.Val_Object));
                     else
                        Result := Item;
                     end if;
                  else
                     raise Constraint_Error with
                       "object " & To_String (Item.Val_Object)
                       & " has no method named '"
                       & To_String (Item.Val_Method)
                       & "'";
                  end if;
               end;
            end if;
         when Val_Class =>
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
            V := New_Value ((Val_Entry, Key, null, null));
            Env.Map.Insert (Key, V);
            return V;
         end if;
      end if;

      return null;

   end Get_Entry_Value;

   ----------------
   -- Get_Method --
   ----------------

   function Get_Method
     (From_Value : VM_Value;
      Name       : String)
      return VM_Value
   is
      Method : constant VM_Value_Record :=
                 (Val_Method_Call, From_Value, To_Unbounded_String (Name));
   begin
      return New_Value (Method);
   end Get_Method;

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
      elsif V.Val_Value.Class = Val_Primitive
        and then V.Val_Value.Arg_Count = 0
      then
         return Evaluate (V.Val_Value, Env);
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
        New_Value ((Val_Entry, Key, null, Value));
   begin
      Env.Map.Insert (Key, V);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (Env         : VM_Environment;
                     Class_Name  : String;
                     Method_Name : String;
                     Value       : VM_Value)
   is
   begin
      Insert (Env, Class_Name & "__" & Method_Name, Value);
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
     (Name   : String;
      Parent : VM_Environment)
      return VM_Environment
   is
      Result : constant VM_Environment := new VM_Environment_Record;
   begin
      Result.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Result.Parent := Parent;
      Active_Environments.Append (Result);
      return Result;
   end New_Environment;

   ---------------
   -- New_Value --
   ---------------

   function New_Value
     (Rec : VM_Value_Record)
      return VM_Value
   is
      Result : VM_Value;
   begin
      if Free_Value_List.Is_Empty then
         Result := new VM_Value_Record'(Rec);
      else
         Result := Free_Value_List.First_Element;
         Free_Value_List.Delete_First;
         Result.all := Rec;
      end if;
      Active_Value_List.Append (Result);
      return Result;
   end New_Value;

   -------------------------
   -- Release_Environment --
   -------------------------

   procedure Release_Environment
     (Env : in out VM_Environment)
   is
      use Environment_Lists;
      procedure Free is
        new Ada.Unchecked_Deallocation
          (VM_Environment_Record, VM_Environment);
      Position : Cursor := Active_Environments.Find (Env);
   begin
      Active_Environments.Delete (Position);
      Free (Env);
   end Release_Environment;

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
         V.Properties :=
           New_Environment (To_String (V) & " properties", Env);
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
            if Item.Prop_Value = null then
               return "/nil/";
--              elsif Item.Prop_Value.all in
--                Aquarius.Programs.Program_Tree_Type'Class
--              then
--                 return Aquarius.Programs.Program_Tree
--                   (Item.Prop_Value).Concatenate_Children;
            else
               return Item.Prop_Value.Name;
            end if;
         when Val_Cons =>
            return '(' & Show (Item.Head) & " . " & Show (Item.Tail) & ')';
         when Val_Entry =>
            return '[' & To_String (Item.Val_Name) &
              ": " & Show (Item.Val_Value) & "]";
         when Val_Primitive =>
            return "(primitive" & Item.Arg_Count'Img & ")";
         when Val_Method_Call =>
            return Show (Item.Val_Object) & "."
              & To_String (Item.Val_Method);
         when Val_Class =>
            return "[" & To_String (Item.Class_Name) & "]";
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

      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "To_Boolean: " & To_String (Value));

      if Value = null then
         return False;
      end if;

      case Value.Class is
         when Val_Error =>
            raise Constraint_Error with Show (Value);
         when Val_Integer =>
            return Value.Int_Value /= 0;
         when Val_Property =>
            return Value.Prop_Value /= null;
         when Val_Cons =>
            return True;
         when Val_Entry =>
            return To_Boolean (Value.Val_Value);
         when Val_Method_Call =>
            return False;
         when Val_Primitive =>
            raise Constraint_Error with "Can't convert primitive to boolean";
         when Val_Class =>
            return True;
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
      return New_Value ((Val_Integer, Item));
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value (Item : String) return VM_Value is
   begin
      return New_Value ((Val_Property,
                        Aquarius.Names.Name_Value (Item)));
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value
     (Item : access Root_Aquarius_Object'Class)
      return VM_Value
   is
   begin
      return New_Value ((Val_Property, Item));
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value
     (Item      : Evaluator;
      Arg_Count : Natural)
      return VM_Value
   is
   begin
      return New_Value ((Val_Primitive, Item, Arg_Count));
   end To_Value;

   --------------
   -- To_Value --
   --------------

   function To_Value (Item     : Evaluator;
                      Default_Values : Array_Of_Values)
                      return VM_Value
   is
   begin
      return To_Value (Item, Default_Values'Length);
   end To_Value;

end Aquarius.VM;
