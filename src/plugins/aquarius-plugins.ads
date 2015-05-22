with Aquarius.Actions;
with Aquarius.Formats;
with Aquarius.Grammars;
with Aquarius.Programs;
with Aquarius.Properties;

with Aquarius.Entries;
with Aquarius.Types;

with Aquarius.VM;

private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Fixed.Hash;

package Aquarius.Plugins is

   type Aquarius_Plugin_Type is
     abstract new Root_Aquarius_Object and Watcher
     with private;

   type Aquarius_Plugin is access all Aquarius_Plugin_Type'Class;

   not overriding
   function Version (Plugin : Aquarius_Plugin_Type)
                    return String
      is abstract;

   procedure Load (Plugin  : not null access Aquarius_Plugin_Type;
                   Grammar : in     Aquarius.Grammars.Aquarius_Grammar);

   function Get_Standard_Entry (Plugin : access Aquarius_Plugin_Type;
                                Name   : in     String)
                               return Aquarius.Entries.Table_Entry;

   function Get_Standard_Type (Plugin : access Aquarius_Plugin_Type;
                               Name   : in     String)
                              return Aquarius.Types.Aquarius_Type;

   procedure Create_Action_Group
     (Plugin     : in out Aquarius_Plugin_Type;
      Group_Name : in     String;
      Trigger    : in     Aquarius.Actions.Action_Execution_Trigger;
      Group      :    out Aquarius.Actions.Action_Group);

   procedure Add_Action_Group (Plugin : in out Aquarius_Plugin_Type;
                               Group  : Aquarius.Actions.Action_Group)
   with Pre => not Plugin.Have_Action_Group
     (Aquarius.Actions.Action_Group_Name (Group));

   function Have_Action_Group (Plugin : Aquarius_Plugin_Type;
                               Name   : String)
                              return Boolean;
   function Get_Action_Group (Plugin : Aquarius_Plugin_Type;
                              Name   : String)
                             return Aquarius.Actions.Action_Group
     with Pre => Plugin.Have_Action_Group (Name);

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : in     String;
      Rule        : in     Aquarius.Formats.Aquarius_Format);

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : in     String;
      Rule        : in     Aquarius.Formats.Format_Rule);

   procedure Register_Rule
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : in     String;
      Rule_1      : in     Aquarius.Formats.Format_Rule;
      Rule_2      : in     Aquarius.Formats.Format_Rule);

   procedure Register_Action
     (Plugin      : not null access Aquarius_Plugin_Type;
      Syntax_Name : in     String;
      Group       : in     Aquarius.Actions.Action_Group;
      Position    : in     Rule_Position;
      Action      : in     Aquarius.Actions.Node_Action);

   procedure Register_Action
     (Plugin      : not null access Aquarius_Plugin_Type;
      Parent_Name : in     String;
      Child_Name  : in     String;
      Group       : in     Aquarius.Actions.Action_Group;
      Position    : in     Rule_Position;
      Action      : in     Aquarius.Actions.Parent_Action);

   type Change_Handler is access
     procedure (Item   : not null access
                  Aquarius.Programs.Program_Tree_Type'Class;
                Object : not null access Root_Aquarius_Object'Class);

   procedure Register_Change_Handler
     (Plugin      : not null access Aquarius_Plugin_Type'Class;
      Syntax_Name : in              String;
      Handler     : in              Change_Handler);

   procedure On_Entry_Change
     (Plugin   : not null access Aquarius_Plugin_Type;
      Node     : in              Aquarius.Programs.Program_Tree)
   is null;

   procedure Add_Standard_Entry
     (Plugin : not null access Aquarius_Plugin_Type;
      Item   : in     Aquarius.Entries.Table_Entry);

   procedure Add_Standard_Type
     (Plugin : not null access Aquarius_Plugin_Type;
      Name   : in     String;
      Item   : in     Aquarius.Types.Aquarius_Type)
   is null;

   function Environment
     (Plugin : not null access Aquarius_Plugin_Type'Class)
     return Aquarius.VM.VM_Environment;

   procedure New_Command
     (Plugin        : not null access Aquarius_Plugin_Type'Class;
      Internal_Name : String;
      External_Name : String;
      Menu_Path     : String;
      Description   : String;
      Definition    : Aquarius.VM.VM_Value);

private

   subtype Plugin_Map_Name is String (1 .. 20);

   package Change_Handler_Map is
      new Ada.Containers.Hashed_Maps
     (Key_Type        => Plugin_Map_Name,
      Element_Type    => Change_Handler,
      Hash            => Ada.Strings.Fixed.Hash,
      Equivalent_Keys => "=");

   subtype Plugin_Group_Name is String (1 .. 20);

   package Group_Map is
      new Ada.Containers.Hashed_Maps
     (Key_Type        => Plugin_Group_Name,
      Element_Type    => Aquarius.Actions.Action_Group,
      Hash            => Ada.Strings.Fixed.Hash,
      Equivalent_Keys => "=",
      "="             => Aquarius.Actions."=");

   type Aquarius_Plugin_Type is
     abstract new Root_Aquarius_Object and Watcher with
      record
         Grammar         : Aquarius.Grammars.Aquarius_Grammar;
         Standard        : Aquarius.Entries.Symbol_Table;
         VM_Env          : Aquarius.VM.VM_Environment;
         Change_Handlers : Change_Handler_Map.Map;
         Action_Groups   : Group_Map.Map;
         Group_List      : Aquarius.Actions.Action_Group_List;
         Change_Flag     : Aquarius.Properties.Property_Type;
         Have_Menu       : Boolean := False;
      end record;

   overriding
   procedure Object_Changed
     (W       : in out Aquarius_Plugin_Type;
      Item    : not null access Aquarius.Root_Aquarius_Object'Class;
      Context : not null access Aquarius.Root_Aquarius_Object'Class);

end Aquarius.Plugins;
