with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package body Aquarius.UI.Menus is

   package Menu_Vector is
     new Ada.Containers.Vectors (Positive, Aquarius_Menu, "=");

   type Aquarius_Menu_Record is
      record
         Menu_Title    : Ada.Strings.Unbounded.Unbounded_String;
         Menu_Children : Menu_Vector.Vector;
         Command       : Menu_Command;
      end record;

   --------------
   -- Activate --
   --------------

   procedure Activate (Item : Aquarius_Menu) is
   begin
      Item.Command.Command_Result := VM.Null_Value;
      Item.Command.Execute;
   end Activate;

   -----------------------
   -- Activation_Result --
   -----------------------

   function Activation_Result (Item : Aquarius_Menu)
                              return Aquarius.VM.VM_Value
   is
   begin
      return Item.Command.Command_Result;
   end Activation_Result;

   -----------------
   -- Add_Submenu --
   -----------------

   procedure Add_Submenu
     (To_Menu : Aquarius_Menu;
      Item    : Aquarius_Menu)
   is
   begin
      To_Menu.Menu_Children.Append (Item);
   end Add_Submenu;

   -----------
   -- Child --
   -----------

   function Child
     (Item   : Aquarius_Menu;
      Index  : Positive)
      return Aquarius_Menu
   is
   begin
      return Item.Menu_Children.Element (Index);
   end Child;

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count (Item : Aquarius_Menu) return Natural is
   begin
      return Item.Menu_Children.Last_Index;
   end Child_Count;

   ----------------
   -- Command_UI --
   ----------------

   function Command_UI (Item : not null access Root_Menu_Command'Class)
                       return access Aquarius_UI'Class
   is
   begin
      return Item.Menu_UI;
   end Command_UI;

   --------------
   -- New_Menu --
   --------------

   function New_Menu (Text    : String;
                      Command : not null access Root_Menu_Command'Class)
                      return Aquarius_Menu
   is
      Result : constant Aquarius_Menu :=
                 new Aquarius_Menu_Record'
                   (Menu_Title    =>
                          Ada.Strings.Unbounded.To_Unbounded_String (Text),
                    Menu_Children => Menu_Vector.Empty_Vector,
                    Command       => Menu_Command (Command));
   begin
      return Result;
   end New_Menu;

   --------------
   -- New_Menu --
   --------------

   function New_Menu (Text    : String)
                      return Aquarius_Menu
   is
   begin
      return new Aquarius_Menu_Record'
        (Menu_Title    => Ada.Strings.Unbounded.To_Unbounded_String (Text),
         Menu_Children => Menu_Vector.Empty_Vector,
         Command       => null);
   end New_Menu;

   ----------------
   -- Set_Result --
   ----------------

   procedure Set_Result
     (Item   : not null access Root_Menu_Command'Class;
      Result : Aquarius.VM.VM_Value)
   is
   begin
      Item.Command_Result := Result;
   end Set_Result;

   ---------------
   -- Show_Menu --
   ---------------

   procedure Show_Menu (Menu      : Aquarius_Menu;
                        UI        : not null access Aquarius_UI'Class)
   is
   begin
      if Menu.Command /= null then
         Menu.Command.Menu_UI := UI;
      end if;
      for I in 1 .. Menu.Menu_Children.Last_Index loop
         Show_Menu (Menu.Menu_Children.Element (I), UI);
      end loop;
   end Show_Menu;

   ----------
   -- Text --
   ----------

   function Text (Item : Aquarius_Menu) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Menu_Title);
   end Text;

end Aquarius.UI.Menus;
