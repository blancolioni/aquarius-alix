with Ada.Strings.Fixed;

package body Aquarius.Messages is

   function Before (Left, Right : Message) return Boolean;

   package Message_Sorting is
      new Message_Vector.Generic_Sorting (Before);

   -----------------
   -- Add_Message --
   -----------------

   procedure Add_Message (To    : in out Message_List;
                          Item  : in     Message)
   is
   begin
      To.Messages.Append (Item);
      if To.Sorted then
         Message_Sorting.Sort (To.Messages);
      end if;
   end Add_Message;

   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference (To         : in     Message;
                            Reference  : access Message_Location'Class;
                            Message    : in     String)
   is
      Ref : constant Message_Reference := (Reference, new String'(Message));
   begin
      if To.Reference_Count = 0 then
         To.Reference_Count := 1;
         To.Reference := Ref;
      else
         To.Reference_Count := To.Reference_Count + 1;
         if To.References = null then
            To.References := new Array_Of_References (2 .. 4);
         end if;
         To.References (To.Reference_Count) := Ref;
      end if;
   end Add_Reference;

   ------------
   -- Before --
   ------------

   function Before (Left, Right : Message) return Boolean is
   begin
      return Left.Location.Before (Right.Location);
   end Before;

   ------------------------
   -- Clear_Message_List --
   ------------------------

   procedure Clear_Message_List (List : in out Message_List) is
   begin
      List.Messages.Clear;
   end Clear_Message_List;

   -----------------------
   -- Copy_Message_List --
   -----------------------

   procedure Copy_Message_List (From : in     Message_List;
                                To   : in out Message_List)
   is
   begin
      To.Messages.Append (From.Messages);
      if To.Sorted then
         Message_Sorting.Sort (To.Messages);
      end if;
   end Copy_Message_List;

   -------------------------
   -- Create_Message_List --
   -------------------------

   procedure Create_Message_List (List   : in out Message_List;
                                  Sorted : in     Boolean)
   is
   begin
      List.Messages.Clear;
      List.Sorted := Sorted;
   end Create_Message_List;

   ------------------------
   -- Empty_Message_List --
   ------------------------

   function Empty_Message_List (Sorted : Boolean) return Message_List is
      Result : Message_List;
   begin
      Result.Sorted := Sorted;
      return Result;
   end Empty_Message_List;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Item : Message)
                         return access Message_Location'Class
   is
   begin
      return Item.Location;
   end Get_Location;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message   (List  : in Message_List;
                           Index : in Positive)
                          return Message
   is
   begin
      return List.Messages.Element (Index);
   end Get_Message;

   ----------------------
   -- Get_Message_Text --
   ----------------------

   function Get_Message_Text
     (Item : Message)
      return String
   is
   begin
      return Item.Text.all;
   end Get_Message_Text;

   -------------------
   -- Highest_Level --
   -------------------

   function Highest_Level (List : in Message_List) return Message_Level is
      Highest : Message_Level := Message_Level'First;
   begin
      for I in 1 .. List.Messages.Last_Index loop
         if List.Messages.Element (I).Level > Highest then
            Highest := List.Messages.Element (I).Level;
            exit when Highest = Message_Level'Last;
         end if;
      end loop;
      return Highest;
   end Highest_Level;

   ----------------
   -- Level_Name --
   ----------------

   function Level_Name (Level : Message_Level) return String is
   begin
      case Level is
         when No_Message =>
            return "";
         when Informational =>
            return "information";
         when Style =>
            return "style";
         when Warning =>
            return "warning";
         when Error =>
            return "error";
         when Fatal_Error =>
            return "fatal error";
         when Internal_Error =>
            return "internal error";
      end case;
   end Level_Name;

   ------------------
   -- Level_Prefix --
   ------------------

   function Level_Prefix (Level : Message_Level) return String is
   begin
      case Level is
         when No_Message =>
            return "";
         when Informational =>
            return "";
         when Style =>
            return "style: ";
         when Warning =>
            return "warning: ";
         when Error =>
            return "";
         when Fatal_Error =>
            return "fatal error: ";
         when Internal_Error =>
            return "internal error: ";
      end case;
   end Level_Prefix;

   -------------------
   -- Message_Count --
   -------------------

   function Message_Count (List : in Message_List) return Natural is
   begin
      return List.Messages.Last_Index;
   end Message_Count;

   -----------------
   -- New_Message --
   -----------------

   function New_Message (Level    : in     Message_Level;
                         Location : access Message_Location'Class;
                         Text     : in     String)
                        return Message
   is
      Result : constant Message := new Message_Record;
   begin
      Result.Level           := Level;
      Result.Location        := Location;
      Result.Text            := new String'(Text);
      Result.Reference_Count := 0;
      return Result;
   end New_Message;

   -----------------
   -- New_Message --
   -----------------

   function New_Message (Level         : in     Message_Level;
                         Location      : access Message_Location'Class;
                         Text          : in     String;
                         Reference     : access Message_Location'Class;
                         Reference_Msg : in     String)
                        return Message
   is
      Result : constant Message := New_Message (Level, Location, Text);
   begin
      if Reference /= null then
         Add_Reference (Result, Reference, Reference_Msg);
      end if;
      return Result;
   end New_Message;

   ---------------------
   -- Reference_Count --
   ---------------------

   function Reference_Count (Item : in Message) return Natural is
   begin
      return Item.Reference_Count;
   end Reference_Count;

   ----------
   -- Show --
   ----------

   function Show (Item : in Message) return String is
   begin
      if Item.Location /= null then
         return Item.Location.Show_Location & ": " &
           Level_Prefix (Item.Level) &
           Item.Text.all;
      else
         return Level_Prefix (Item.Level) & Item.Text.all;
      end if;
   end Show;

   -------------------
   -- Show_Location --
   -------------------

   function Show_Location (Location : Message_Location'Class)
                          return String
   is
      Line_Img : constant String :=
        Ada.Strings.Fixed.Trim (Positive'Image (Location.Location_Line),
                                Ada.Strings.Left);
      Col_Img : constant String :=
        Ada.Strings.Fixed.Trim (Positive'Image (Location.Location_Column),
                                Ada.Strings.Left);
   begin
      return Location_Name (Location) & ":" & Line_Img & ":" & Col_Img;
   end Show_Location;

   --------------------
   -- Show_Reference --
   --------------------

   function Show_Reference (Item      : in Message;
                            Ref_Index : in Positive)
                           return String
   is
      Ref : Message_Reference := Item.Reference;
   begin
      if Ref_Index > Item.Reference_Count then
         raise Constraint_Error with
           "Message has no such reference" & Ref_Index'Img &
           " (" & Show (Item) & ")";
      end if;
      if Ref_Index > 1 then
         Ref := Item.References (Ref_Index);
      end if;
      if Ref.Reference = null then
         return "(built-in)" &
           ": " & Level_Prefix (Item.Level) &
           Ref.Text.all;
      else
         return Ref.Reference.Show_Location &
           ": " & Level_Prefix (Item.Level) &
           Ref.Text.all;
      end if;
   end Show_Reference;

end Aquarius.Messages;
