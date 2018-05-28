with Ack;

package Aquarius.Command_Line is

   function Version return Boolean;
   function Help    return Boolean;

   function Clear_Cache return Boolean;

   function Filter  return Boolean;

   function Action       return String;
   function Grammar      return String;
   function Renderer     return String;
   function Theme        return String;
   function Target       return String;
   function Input_File   return String;
   function Output_File  return String;
   function Enable_Debug return String;
   function Project_Name return String;
   function Plugin_Name  return String;
   function Plugin_Path  return String;
   function Session_File return String;
   function Edit_Plugin return String;

   function Line_Length  return Positive;
   function Enable_Plugins return Boolean;

   function Assertion_Monitoring return Ack.Assertion_Monitoring_Level;

   function Ack_Write_Listing return Boolean;
   function Ack_Write_Tables return Boolean;
   function Ack_Execute_Root return String;
   function Ack_Trace_Class_Analysis return Boolean;
   function Ack_Stack_Check return Boolean;

   function Aqua_Trace_Code return Boolean;
   function Aqua_Trace_Link return Boolean;

   function Extra_Arguments return String;

end Aquarius.Command_Line;
