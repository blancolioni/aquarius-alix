with Aquarius.VM.Values;

package Aquarius.UI.Menus is

   type Aquarius_Menu is private;

   type Root_Menu_Command is abstract tagged private;

   procedure Execute (Item : not null access Root_Menu_Command)
   is abstract;

   procedure Set_Result
     (Item   : not null access Root_Menu_Command'Class;
      Result : Aquarius.VM.Values.VM_Value);

   function Command_UI (Item : not null access Root_Menu_Command'Class)
                       return access Aquarius_UI'Class;

   type Menu_Command is access all Root_Menu_Command'Class;

   function Text (Item : Aquarius_Menu) return String;

   function Child_Count (Item : Aquarius_Menu) return Natural;
   function Child (Item   : Aquarius_Menu;
                   Index  : Positive)
                   return Aquarius_Menu;

   function New_Menu (Text    : String;
                      Command : not null access Root_Menu_Command'Class)
                      return Aquarius_Menu;

   function New_Menu (Text    : String)
                      return Aquarius_Menu;

   procedure Show_Menu (Menu      : Aquarius_Menu;
                        UI        : not null access Aquarius_UI'Class);

   procedure Add_Submenu (To_Menu : Aquarius_Menu;
                          Item    : Aquarius_Menu);

   procedure Activate (Item : Aquarius_Menu);
   function Activation_Result (Item : Aquarius_Menu)
     return Aquarius.VM.Values.VM_Value;

private

   type Aquarius_Menu_Record;

   type Aquarius_Menu is access Aquarius_Menu_Record;

   type Root_Menu_Command is abstract tagged
      record
         Menu_UI        : access Aquarius_UI'Class;
         Command_Result : Aquarius.VM.Values.VM_Value;
      end record;

end Aquarius.UI.Menus;
