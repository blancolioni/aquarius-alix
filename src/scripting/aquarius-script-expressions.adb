with Aquarius.Names;
with Aquarius.Script.Library;

package body Aquarius.Script.Expressions is

   type Let_Expression_Element is
     new Expression_Element with
      record
         Bound_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Bound_Value : Script_Element;
      end record;

   overriding
   function Name (Item : Let_Expression_Element) return String;

   overriding
   procedure Execute (Item        : in     Let_Expression_Element;
                      Environment : in out Script_Environment);

   overriding
   function Evaluate (Item        : Let_Expression_Element;
                      Environment : Script_Environment)
                     return access Root_Aquarius_Object'Class;

   type Call_Expression_Element is
     new Expression_Element with
      record
         Call_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Call_Args  : access Array_Of_Expressions;
      end record;

   overriding
   function Name (Item : Call_Expression_Element) return String;

   overriding
   function Evaluate (Item        : Call_Expression_Element;
                      Environment : Script_Environment)
                     return access Root_Aquarius_Object'Class;

   type String_Expression_Element is
     new Expression_Element with
      record
         Text  : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding
   function Name (Item : String_Expression_Element) return String;

   overriding
   function Evaluate (Item        : String_Expression_Element;
                      Environment : Script_Environment)
                     return access Root_Aquarius_Object'Class;

   ---------------------
   -- Call_Expression --
   ---------------------

   function Call_Expression (Callee    : String;
                             Arguments : Array_Of_Expressions)
                             return Expression_Access
   is
      use Ada.Strings.Unbounded;
   begin
      return new Call_Expression_Element'
        (Expression_Element with
           Call_Name => To_Unbounded_String (Callee),
           Call_Args => new Array_Of_Expressions'(Arguments));
   end Call_Expression;

   --------------
   -- Evaluate --
   --------------

   overriding
   function Evaluate (Item        : Let_Expression_Element;
                      Environment : Script_Environment)
                     return access Root_Aquarius_Object'Class
   is
      pragma Unreferenced (Environment);
   begin
      return Item.Bound_Value;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding
   function Evaluate (Item        : Call_Expression_Element;
                      Environment : Script_Environment)
                     return access Root_Aquarius_Object'Class
   is
      Fn : constant Environment_Entry :=
             Find (Environment,
                   Ada.Strings.Unbounded.To_String (Item.Call_Name));
   begin
      if Item.Call_Args /= null
        and then Item.Call_Args'Length > 0
      then
         return Aquarius.Script.Library.Execute (Fn.Object_Value,
                                                 Environment,
                                                 Item.Call_Args.all);
      else
         return null;
      end if;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding
   function Evaluate (Item        : String_Expression_Element;
                      Environment : Script_Environment)
     return access Root_Aquarius_Object'Class
   is
      pragma Unreferenced (Environment);
   begin
      return Aquarius.Names.Name_Value
        (Ada.Strings.Unbounded.To_String (Item.Text));
   end Evaluate;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Item        : in     Expression_Element;
                      Environment : in out Script_Environment)
   is
      Result : constant access Root_Aquarius_Object'Class :=
        Evaluate (Expression_Element'Class (Item), Environment);
      Current : constant Environment_Entry :=
                  Find (Environment, "$");
   begin
      if Current.Entry_Type = Null_Entry then
         Insert (Environment, "$", (Aquarius_Object_Entry, Result));
      else
         Replace (Environment, "$", (Aquarius_Object_Entry, Result));
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Item        : in     Let_Expression_Element;
                      Environment : in out Script_Environment)
   is
      use Ada.Strings.Unbounded;
      New_Entry : constant Environment_Entry :=
                    (Aquarius_Object_Entry, Item.Bound_Value);
   begin
      Replace (Environment, To_String (Item.Bound_Name), New_Entry);
   end Execute;

   ---------------------------
   -- Identifier_Expression --
   ---------------------------

   function Identifier_Expression (Name : String) return Expression_Access is
      use Ada.Strings.Unbounded;
   begin
      return new Call_Expression_Element'
        (Expression_Element with
           Call_Name => To_Unbounded_String (Name),
           Call_Args => null);
   end Identifier_Expression;

   --------------------
   -- Let_Expression --
   --------------------

   function Let_Expression
     (Name    : String;
      Binding : not null access Root_Script_Element'Class)
      return Expression_Access
   is
      Result : Let_Expression_Element;
   begin
      Result.Bound_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Result.Bound_Value := Script_Element (Binding);
      return new Let_Expression_Element'(Result);
   end Let_Expression;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Let_Expression_Element) return String is
   begin
      return "let " & Ada.Strings.Unbounded.To_String (Item.Bound_Name);
   end Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Call_Expression_Element) return String is
   begin
      return "call " & Ada.Strings.Unbounded.To_String (Item.Call_Name);
   end Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : String_Expression_Element) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Text);
   end Name;

   -----------------------
   -- String_Expression --
   -----------------------

   function String_Expression (Value : String) return Expression_Access is
      use Ada.Strings.Unbounded;
   begin
      return new String_Expression_Element'
        (Expression_Element with
           Text => To_Unbounded_String (Value));
   end String_Expression;

   ---------------------
   -- With_Expression --
   ---------------------

   function With_Expression
     (Withed_Item : String;
      Withed_Body : not null access Root_Script_Element'Class)
      return Expression_Access
   is
      pragma Unreferenced (Withed_Item);
      pragma Unreferenced (Withed_Body);
   begin
      return null;
   end With_Expression;

end Aquarius.Script.Expressions;
