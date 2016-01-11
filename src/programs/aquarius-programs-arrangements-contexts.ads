with Aquarius.Layout;                  use Aquarius.Layout;
with Aquarius.Formats;                 use Aquarius.Formats;

private package Aquarius.Programs.Arrangements.Contexts is

   type Arrangement_Context is
      record
         Need_New_Line      : Boolean             := False;
         Need_Soft_New_Line : Boolean             := False;
         Vertical_Gap       : Count               := 0;
         New_Line_Priority  : Rule_Priority       := 1;
         Got_New_Line       : Boolean             := False;
         First_On_Line      : Boolean             := True;
         First_Terminal     : Program_Tree;
         Previous_Terminal  : Program_Tree;
         Previous_Indent    : Count               := 1;
         Need_Space         : Boolean             := False;
         No_Space           : Boolean             := False;
         Cancel_Indent      : Boolean             := False;
         Message_Level      : Aquarius.Messages.Message_Level :=
                                Aquarius.Messages.No_Message;
         Space_Priority     : Rule_Priority       := 1;
         Current_Line       : Count               := 1;
         Current_Column     : Count               := 1;
         Current_Position   : Count               := 0;
         Current_Indent     : Positive_Count      := 1;
         Right_Margin       : Positive_Count      := 72;
         Rearranging        : Boolean             := False;
         User_Text_Length   : Count               := 0;
         User_Cursor        : Aquarius.Trees.Cursors.Cursor;
         Logging            : Aquarius.Messages.Message_List;
         Stop_Tree          : Program_Tree        := null;
         Stopped            : Boolean             := False;
      end record;

end Aquarius.Programs.Arrangements.Contexts;
