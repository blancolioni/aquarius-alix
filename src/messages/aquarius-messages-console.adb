with Ada.Text_IO;

package body Aquarius.Messages.Console is

   -------------------
   -- Show_Messages --
   -------------------

   procedure Show_Messages (List : in Message_List) is
   begin
      for I in 1 .. Message_Count (List) loop
         declare
            Msg : constant Message := Get_Message (List, I);
         begin
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  Show (Msg));
            for J in 1 .. Reference_Count (Msg) loop
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     Show_Reference (Msg, J));
            end loop;
         end;
      end loop;
   end Show_Messages;

end Aquarius.Messages.Console;
