package Aquarius.Ack.Parser is

   function Import
     (Program : Aquarius.Programs.Program_Tree)
     return Node_Id;

private

   function Import_Class_Name
     (From : Aquarius.Programs.Program_Tree)
      return Node_Id
     with Pre => From.Name = "class_name";

end Aquarius.Ack.Parser;
