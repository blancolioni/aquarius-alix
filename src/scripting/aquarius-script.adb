with Aquarius.Script.Library;

package body Aquarius.Script is

   ------------
   -- Append --
   ------------

   procedure Append (Script : in out Root_Aquarius_Script'Class;
                     Item   : in     Root_Script_Element'Class)
   is
   begin
      Script.Elements.Append (Item);
   end Append;

   -----------------
   -- Bind_Object --
   -----------------

   function Bind_Object (Name   : String;
                         Item   : not null access Root_Aquarius_Object'Class)
                        return Environment_Entry
   is
   begin
      return new Bound_Entry'
        (Root_Aquarius_Object with
           Bound_Name => Ada.Strings.Unbounded.To_Unbounded_String (Name),
           Bound_Value => Item);
   end Bind_Object;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Script  : in Root_Aquarius_Script'Class;
      Node    : not null access Aquarius.Actions.Actionable'Class;
      Parent  : access Aquarius.Actions.Actionable'Class)
   is
      Env : Script_Environment;
   begin
      Insert (Env, Environment_Entry (Parent));
      Insert (Env, Environment_Entry (Node));
      for Element of Script.Elements loop
         Element.Execute (Env);
      end loop;

   end Execute;

   ----------
   -- Find --
   ----------

   function Find (Env  : Script_Environment;
                  Name : String)
                 return Environment_Entry
   is
      Key : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   begin
      if Env.Map.Contains (Key) then
         return Env.Map.Element (Key);
      else
         declare
            Std : constant Script_Environment :=
                    Aquarius.Script.Library.Standard;
         begin
            if Std.Map.Contains (Key) then
               return Std.Map.Element (Key);
            else
               return null;
            end if;
         end;
      end if;
   end Find;

   ------------
   -- Insert --
   ------------

   procedure Insert (Env  : in out Script_Environment;
                     Item : in     Environment_Entry)
   is
   begin
      Env.Map.Insert (Ada.Strings.Unbounded.To_Unbounded_String (Item.Name),
                      Item);
   end Insert;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Root_Aquarius_Script) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Name;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Item : Bound_Entry) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Bound_Name);
   end Name;

   ----------------
   -- New_Script --
   ----------------

   function New_Script (Name : String) return Aquarius_Script is
      Result : constant Aquarius_Script := new Root_Aquarius_Script;
   begin
      Result.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      return Result;
   end New_Script;

   -------------
   -- Replace --
   -------------

   procedure Replace (Env  : in out Script_Environment;
                      Item : in     Environment_Entry)
   is
      Key : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String (Item.Name);
      Element : constant Environment_Maps.Cursor :=
        Env.Map.Find (Key);
   begin
      if Environment_Maps.Has_Element (Element) then
         Env.Map.Replace_Element (Element, Item);
      else
         Env.Map.Insert (Key, Item);
      end if;
   end Replace;

end Aquarius.Script;
