with Ada.Command_Line;
with Ada.Strings.Fixed;

package body Aquarius.Command_Line is

   function Get_Flag (Short_Name : String;
                      Long_Name  : String)
                     return Boolean;

   function Get_Argument (Short_Name : String;
                          Long_Name  : String)
                         return String;

   ------------
   -- Action --
   ------------

   function Action      return String is
   begin
      return Get_Argument ("a", "action");
   end Action;

   ------------------
   -- Enable_Debug --
   ------------------

   function Enable_Debug return String is
   begin
      return Get_Argument ("d", "debug");
   end Enable_Debug;

   ---------------------
   -- Extra_Arguments --
   ---------------------

   function Extra_Arguments return String is
      use Ada.Command_Line;
      Skip : Boolean := False;
   begin
      for I in 1 .. Argument_Count loop
         if not Skip then
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg (Arg'First .. Arg'First + 1) = "--" then
                  null;
               elsif Arg (Arg'First) = '-' then
                  Skip := True;
               else
                  return Arg;
               end if;
            end;
         else
            Skip := False;
         end if;
      end loop;
      return "";
   end Extra_Arguments;

   ------------
   -- Filter --
   ------------

   function Filter  return Boolean is
   begin
      return Get_Flag ("f", "filter");
   end Filter;

   ------------------
   -- Get_Argument --
   ------------------

   function Get_Argument (Short_Name : String;
                          Long_Name  : String)
                         return String
   is
      Short_Argument : constant String := '-' & Short_Name;
      Long_Argument  : constant String := "--" & Long_Name & "=";
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String := Ada.Command_Line.Argument (I);
         begin
            exit when Argument = "--";
            if I < Ada.Command_Line.Argument_Count and then
              Argument = Short_Argument
            then
               return Ada.Command_Line.Argument (I + 1);
            elsif Ada.Strings.Fixed.Head (Argument, Long_Argument'Length)
              = Long_Argument
            then
               return Ada.Strings.Fixed.Tail (Argument,
                                              Argument'Length -
                                                Long_Argument'Length);
            end if;
         end;
      end loop;
      return "";
   end Get_Argument;

   --------------
   -- Get_Flag --
   --------------

   function Get_Flag (Short_Name : String;
                      Long_Name  : String)
                     return Boolean
   is
      Short_Flag : constant String := '-' & Short_Name;
      Long_Flag  : constant String := "--" & Long_Name;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String := Ada.Command_Line.Argument (I);
         begin
            exit when Argument = "--";
            if Argument = Short_Flag or else Argument = Long_Flag then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Get_Flag;

   -------------
   -- Grammar --
   -------------

   function Grammar  return String is
   begin
      return Get_Argument ("g", "grammar");
   end Grammar;

   ----------
   -- Help --
   ----------

   function Help    return Boolean is
   begin
      return Get_Flag ("?", "help");
   end Help;

   ----------------
   -- Input_File --
   ----------------

   function Input_File  return String is
   begin
      return Get_Argument ("i", "input");
   end Input_File;

   -----------------
   -- Output_File --
   -----------------

   function Output_File  return String is
   begin
      return Get_Argument ("o", "output");
   end Output_File;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name  return String is
   begin
      return Get_Argument ("P", "project");
   end Project_Name;

   --------------
   -- Renderer --
   --------------

   function Renderer  return String is
   begin
      return Get_Argument ("r", "render");
   end Renderer;

   -----------
   -- Style --
   -----------

   function Style  return String is
   begin
      return Get_Argument ("s", "style");
   end Style;

   ------------
   -- Target --
   ------------

   function Target  return String is
   begin
      return Get_Argument ("t", "target");
   end Target;

   -------------
   -- Version --
   -------------

   function Version return Boolean is
   begin
      return Get_Flag ("v", "version");
   end Version;

end Aquarius.Command_Line;
