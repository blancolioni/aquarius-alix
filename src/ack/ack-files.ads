package Ack.Files is

   function Base_File_Name (Class : Entity_Id) return String;

   function Find_Class_File
     (Referrer : Aquarius.Programs.Program_Tree;
      Parent   : Entity_Id;
      Name     : Name_Id)
      return String;

end Ack.Files;
