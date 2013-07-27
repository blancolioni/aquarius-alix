private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

limited with Aquarius.Actions;

package Aquarius.Script is

   type Root_Aquarius_Script is
     new Root_Aquarius_Object
     with private;

   overriding
   function Name (Item : Root_Aquarius_Script) return String;

   type Aquarius_Script is
     access all Root_Aquarius_Script'Class;

   procedure Execute
     (Script  : in Root_Aquarius_Script'Class;
      Node    : not null access Aquarius.Actions.Actionable'Class;
      Parent  : access Aquarius.Actions.Actionable'Class);

   function New_Script (Name : String;
                        Path : String)
                        return Aquarius_Script;

   type Script_Environment is private;

   type Root_Script_Element is abstract new Root_Aquarius_Object with private;

   procedure Execute (Item        : in     Root_Script_Element;
                      Environment : in out Script_Environment)
      is abstract;

   procedure Append
     (Script : in out Root_Aquarius_Script'Class;
      Item   : not null access Root_Script_Element'Class);

   type Script_Element is access all Root_Script_Element'Class;

private

   type Root_Script_Element is
     abstract new Root_Aquarius_Object with null record;

   package Script_Element_Vectors is
     new Ada.Containers.Vectors (Positive, Script_Element);

   type Env_Entry_Type is (Null_Entry, String_Entry, Aquarius_Object_Entry);

   type Environment_Entry (Entry_Type : Env_Entry_Type := Null_Entry) is
      record
         case Entry_Type is
            when Null_Entry =>
               null;
            when String_Entry =>
               String_Value : Ada.Strings.Unbounded.Unbounded_String;
            when Aquarius_Object_Entry =>
               Object_Value : access Root_Aquarius_Object'Class;
         end case;
      end record;

   package Environment_Maps is
      new Ada.Containers.Hashed_Maps (Ada.Strings.Unbounded.Unbounded_String,
                                      Environment_Entry,
                                      Ada.Strings.Unbounded.Hash,
                                      Ada.Strings.Unbounded."=");

   type Script_Environment is
      record
         Map : Environment_Maps.Map;
      end record;

   procedure Insert (Env  : in out Script_Environment;
                     Name : in     String;
                     Item : in     Environment_Entry);

   procedure Replace (Env  : in out Script_Environment;
                      Name : in     String;
                      Item : in     Environment_Entry);

   function Find (Env  : Script_Environment;
                  Name : String)
                 return Environment_Entry;

   type Root_Aquarius_Script is
     new Root_Aquarius_Object with
      record
         Name     : Ada.Strings.Unbounded.Unbounded_String;
         Path     : Ada.Strings.Unbounded.Unbounded_String;
         Elements : Script_Element_Vectors.Vector;
      end record;

end Aquarius.Script;
