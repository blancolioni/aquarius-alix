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
      use Ada.Strings, Ada.Strings.Fixed;
      function To_String (I : Integer) return String
      is (Trim (Integer'Image (I), Left));

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
