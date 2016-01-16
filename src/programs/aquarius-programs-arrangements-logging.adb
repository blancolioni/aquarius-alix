with Aquarius.Messages;

package body Aquarius.Programs.Arrangements.Logging is

   ---------
   -- Log --
   ---------

   procedure Log
     (Context  : in out Contexts.Arrangement_Context;
      Program  : Aquarius.Programs.Program_Tree;
      Text     : String)
   is
      Current : constant String :=
                  (if Program.Name = ""
                   then "(" & Program.Parent.Name & ")"
                   else Program.Name
                   & (if Program.Name /= Program.Text
                     then "[" & Program.Text & "]"
                     else ""))
                   & ": ";

      Message : constant Aquarius.Messages.Message :=
                  Aquarius.Messages.New_Message
                    (Level    => Aquarius.Messages.Informational,
                     Location => Program,
                     Text     => Current & Text);
   begin
      Aquarius.Messages.Add_Message (Context.Logging, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Context  : in out Contexts.Arrangement_Context;
      Program  : Program_Tree)
   is
--        use Ada.Characters.Handling;
      use Ada.Strings, Ada.Strings.Fixed;
--        function To_String (B : Boolean) return String
--        is (To_Lower (Boolean'Image (B)));
      function To_String (I : Integer) return String
      is (Trim (Integer'Image (I), Left));

--           Need_New_Line     : Boolean             := False;
--           New_Line_Priority : Rule_Priority       := 1;
--           Got_New_Line      : Boolean             := False;
--           First_On_Line     : Boolean             := True;
--           First_Terminal    : Program_Tree;
--           Previous_Terminal : Program_Tree;
--           Need_Space        : Boolean             := False;
--           No_Space          : Boolean             := False;
--           Cancel_Indent     : Boolean             := False;
--           Message_Level     : Aquarius.Messages.Message_Level :=
--             Aquarius.Messages.No_Message;
--           Space_Priority    : Rule_Priority       := 1;
--           Current_Line      : Count               := 1;
--           Current_Column    : Count               := 1;
--           Current_Position  : Count               := 0;
--           Current_Indent    : Count               := 1;
--           Right_Margin      : Positive_Count      := 72;
--           Overflow          : Boolean             := False;
--           Overflow_Line     : Count               := 0;
--           Rearranging       : Boolean             := False;
--           User_Text_Length  : Count               := 0;
--           User_Cursor       : Aquarius.Trees.Cursors.Cursor;
--           Logging           : Aquarius.Messages.Message_List;
   begin
      Logging.Log (Context, Program,
           (if Context.Need_New_Line then "[need-nl]" else "")
           & (if Context.Got_New_Line then "[got-nl]" else "")
           & (if Context.First_On_Line then "[first]" else "")
           & (if Context.Rearranging then "[rearranging]" else "")
          & ","
           & "line="
           & To_String (Integer (Context.Current_Line))
           & ","
           & "col="
           & To_String (Integer (Context.Current_Column))
           & ","
           & "indent="
           & To_String (Integer (Context.Current_Indent))
          );
   end Log;

end Aquarius.Programs.Arrangements.Logging;
