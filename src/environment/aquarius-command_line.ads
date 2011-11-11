package Aquarius.Command_Line is

   function Version return Boolean;
   function Help    return Boolean;

   function Filter  return Boolean;

   function Action       return String;
   function Grammar      return String;
   function Renderer     return String;
   function Style        return String;
   function Target       return String;
   function Input_File   return String;
   function Output_File  return String;
   function Enable_Debug return String;
   function Project_Name return String;

   function Extra_Arguments return String;

end Aquarius.Command_Line;
