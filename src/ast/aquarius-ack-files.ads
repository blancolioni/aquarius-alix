package Aquarius.Ack.Files is

   function Find_Class_File
     (Referrer : Aquarius.Programs.Program_Tree;
      Parent   : Entity_Id;
      Name     : Name_Id)
      return String;

end Aquarius.Ack.Files;
