with Ada.Text_IO;

package body Komnenos.Logging is

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, Append_File, "trace.txt");
      Put_Line (File, Message);
      Close (File);
   end Log;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, "trace.txt");
      Close (File);
   end Start_Logging;

end Komnenos.Logging;
