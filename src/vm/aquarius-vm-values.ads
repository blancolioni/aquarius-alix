private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Hash;

with Aquarius.Trees;

package Aquarius.VM.Values is

   type VM_Value is private;

   type VM_Environment is private;

   Null_Environment : constant VM_Environment;

   type Array_Of_Values is array (Positive range <>) of VM_Value;

   Null_Value : constant VM_Value;

   function Show (Item : VM_Value) return String;

   function Evaluate (Item : VM_Value;
                      Env  : VM_Environment)
                      return VM_Value;

   procedure Evaluate (Item : VM_Value;
                       Env  : VM_Environment);

   function Error_Value (Message : String) return VM_Value;

   function To_Boolean (Value : VM_Value) return Boolean;
   function To_Value (Item : Boolean) return VM_Value;

   function To_Integer (Value : VM_Value) return Integer;
   function To_Value (Item : Integer) return VM_Value;

   function To_String (Value : VM_Value) return String;
   function To_Value (Item : String) return VM_Value;

   function To_Property (Value : VM_Value)
                        return access Root_Aquarius_Object'Class;
   function To_Value (Item : access Root_Aquarius_Object'Class)
                      return VM_Value;

   function Has_Tree (Item : VM_Value) return Boolean;
   function To_Tree (Item : VM_Value) return Aquarius.Trees.Tree;

   type Evaluator is access function (Env  : VM_Environment;
                                      Args : Array_Of_Values) return VM_Value;

   function To_Value (Item     : Evaluator;
                      Defaults : Array_Of_Values)
                     return VM_Value;

   function Cons (Head, Tail : VM_Value) return VM_Value;
   function Head (Item : VM_Value) return VM_Value;
   function Tail (Item : VM_Value) return VM_Value;

   function Make_List (Items : Array_Of_Values) return VM_Value;

   function New_Environment (Parent : VM_Environment)
                            return VM_Environment;

   function Get_Value (Env  : VM_Environment;
                       Name : String)
                      return VM_Value;

   procedure Insert (Env   : VM_Environment;
                     Name  : String;
                     Value : VM_Value);

   procedure Set_Property (Env        : VM_Environment;
                           Name       : String;
                           Prop_Name  : String;
                           Prop_Value : VM_Value);

private

   type Value_Class is
     (Val_Error, Val_Integer, Val_Property, Val_Cons, Val_Entry,
      Val_Primitive);

   type VM_Value_Record (Class : Value_Class);

   type VM_Value is access VM_Value_Record;

   type VM_Environment_Record;
   type VM_Environment is access VM_Environment_Record;
   Null_Environment : constant VM_Environment := null;

   type VM_Value_Record (Class : Value_Class) is
      record
         case Class is
            when Val_Error =>
               Message : Ada.Strings.Unbounded.Unbounded_String;
            when Val_Integer =>
               Int_Value : Integer;
            when Val_Property =>
               Prop_Value : not null access Root_Aquarius_Object'Class;
            when Val_Cons =>
               Head, Tail : VM_Value;
            when Val_Entry =>
               Val_Name     : Ada.Strings.Unbounded.Unbounded_String;
               Properties   : VM_Environment;
               Val_Value    : VM_Value;
            when Val_Primitive =>
               Fn : Evaluator;
         end case;
      end record;

   Null_Value : constant VM_Value := null;

   package Env_Map is
      new Ada.Containers.Hashed_Maps
     (Ada.Strings.Unbounded.Unbounded_String,
      VM_Value,
      Ada.Strings.Unbounded.Hash,
      Ada.Strings.Unbounded."=");

   type VM_Environment_Record is
      record
         Parent : VM_Environment;
         Map    : Env_Map.Map;
      end record;

end Aquarius.VM.Values;
