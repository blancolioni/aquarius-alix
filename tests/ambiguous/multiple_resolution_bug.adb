procedure Multiple_Resolution_Bug is
   X : array (1 .. 10) of Integer;
   Index : Positive := 3;
begin
   --  The error occurs on the next two lines.
   --  It appears to be something to do with resolving
   --  more than one ambiguity at the same time, which
   --  leaves the 'start' and the 'stop' nodes set
   --  incorrectly, so that the stop node cannot be
   --  found from the start node.
   --  In this case, the start node is the X in X'Range,
   --  and the stop node is := in the assignment statement.
   --  The parse action execution assumes that the stop node
   --  can be found by going right and up.
   if Index in X'Range then
      Index := Index + 1;
   end if;
end Multiple_Resolution_Bug;
