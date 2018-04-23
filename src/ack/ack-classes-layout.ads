private package Ack.Classes.Layout is

   function Create_Virtual_Table_Layout
     (Class : not null access Class_Entity_Record'Class)
      return Virtual_Table_Layout;

   function Create_Object_Layout
     (Class : not null access constant Class_Entity_Record'Class)
      return Object_Layout;

end Ack.Classes.Layout;
