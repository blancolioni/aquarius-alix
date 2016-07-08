private with Ada.Containers.Vectors;

package Aquarius.Messages is

   --  Aquarius.Messages
   --  Support for storing user-readable messages with varying
   --  severity levels (from informational messages to internal errors)

   --  Message levels, lowest severity to highest
   type Message_Level is
     (No_Message,       --  infinitely not severe
      Informational,    --  reports about Aquarius activities
      Style,            --  style criticisms
      Warning,          --  indicate possible problems
      Error,            --  task failure indication
      Fatal_Error,      --  serious failure
      Internal_Error    --  something is wrong with Aquarius itself
     );

   pragma Ordered (Message_Level);

   function Level_Prefix (Level : Message_Level) return String;
   function Level_Name (Level : Message_Level) return String;

   --  Message_Location: something that can have messages attached to it
   type Message_Location is interface;

   --  Message: an Aquarius message, with a severity level, a text
   --  string, an optional references to other message locations and
   --  text

   type Message is private;

   --  New_Message: create a new message with the given level and text
   function New_Message (Level    : in     Message_Level;
                         Location : access Message_Location'Class;
                         Text     : in     String)
                        return Message;

   --  New_Message: create a new message with a reference to another
   --  location.

   function New_Message (Level         : in     Message_Level;
                         Location      : access Message_Location'Class;
                         Text          : in     String;
                         Reference     : access Message_Location'Class;
                         Reference_Msg : in     String)
                        return Message;

   --  Add_Reference: add a reference to an existing message
   procedure Add_Reference (To         : in     Message;
                            Reference  : access Message_Location'Class;
                            Message    : in     String);

   --  Showing messages
   function Show (Item : in Message) return String;
   function Reference_Count (Item : in Message) return Natural;
   function Show_Reference (Item      : in Message;
                            Ref_Index : in Positive)
                           return String;

   --  Getting information about a message
   function Get_Location (Item : Message)
                         return access Message_Location'Class;

   function Get_Message_Text
     (Item : Message)
      return String;

   --  Message_Location abstract interface

   function Show_Location (Location : Message_Location'Class)
                          return String;

   function Location_Name
     (Location : Message_Location)
      return String
      is abstract;

   function Location_Line (Location : Message_Location)
                          return Natural
      is abstract;

   function Location_Column (Location : Message_Location)
                            return Natural
      is abstract;

   --  Attach_Message: add the message to the location
   procedure Attach_Message (To    : in out Message_Location;
                             Item  : in     Message)
      is abstract;

   --  Before: return true if the first location comes before the second
   function Before (Left   : in Message_Location;
                    Right  : not null access Message_Location'Class)
                   return Boolean
      is abstract;

   procedure Clear_Messages (Item : in out Message_Location)
      is abstract;

   --  Message_List: a list of possibly sorted messages.
   type Message_List is private;

   --  Create_Message_List: create a new, empty message list

   function Empty_Message_List (Sorted : Boolean) return Message_List;

   procedure Create_Message_List (List   : in out Message_List;
                                  Sorted : in     Boolean);

   procedure Clear_Message_List (List : in out Message_List);

   procedure Copy_Message_List (From : in     Message_List;
                                To   : in out Message_List);

   procedure Add_Message (To    : in out Message_List;
                          Item  : in     Message);

   function Message_Count (List : in Message_List) return Natural;
   function Get_Message   (List  : in Message_List;
                           Index : in Positive)
                          return Message;

   function Highest_Level (List : in Message_List) return Message_Level;

   procedure Get_Messages (From  : in Message_Location;
                           List  : in out Message_List)
      is abstract;

private

   type Message_Record;
   type Message is access Message_Record;

   type Message_Reference is
      record
         Reference : access Message_Location'Class;
         Text      : access String;
      end record;

   type Array_Of_References is array (Positive range <>) of Message_Reference;

   type Message_Record is
      record
         Level           : Message_Level;
         Location        : access Message_Location'Class;
         Text            : access String;
         Reference_Count : Natural;
         Reference       : Message_Reference;
         References      : access Array_Of_References;
      end record;

   package Message_Vector is new Ada.Containers.Vectors (Positive, Message);

   type Message_List is
      record
         Sorted    : Boolean                := False;
         Messages  : Message_Vector.Vector;
      end record;

end Aquarius.Messages;
