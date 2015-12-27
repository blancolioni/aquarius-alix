with Ada.Containers.Hashed_Maps;
with Ada.Directories;
with Ada.Text_IO;

with Aquarius.Library;

with Aquarius.Plugins.EBNF;
with Aquarius.Plugins.Klein;
with Aquarius.Plugins.Macro_11;
with Aquarius.Plugins.Macro_32;
with Aquarius.Plugins.Plugin;
with Aquarius.Plugins.Script_Plugin;

with Aquarius.Configuration;
with Aquarius.Grammars.Manager;
with Aquarius.Loader;

with Ada_Plugin;
with Project_Plugin;
with Haskell;

package body Aquarius.Plugins.Manager is

   subtype Plugin_Map_Name is String (1 .. 16);

   package Plugin_Map is
      new Ada.Containers.Hashed_Maps (Plugin_Map_Name,
                                      Aquarius_Plugin,
                                      Ada.Strings.Fixed.Hash,
                                      "=");

   Loaded_Plugins : Plugin_Map.Map;

   Local_EBNF_Plugin     : Aquarius_Plugin;
   Local_Klein_Plugin    : Aquarius_Plugin;
   Local_Ada_Plugin      : Aquarius_Plugin;
   Local_Project_Plugin  : Aquarius_Plugin;
   Local_Haskell_Plugin  : Aquarius_Plugin;
   Local_Macro_11_Plugin : Aquarius_Plugin;
   Local_Macro_32_Plugin : Aquarius_Plugin;
   Local_Plugin_Plugin   : Aquarius_Plugin;
   Local_Script_Plugin   : Aquarius_Plugin;

   function To_Plugin_Map_Name
     (Name : String)
     return Plugin_Map_Name;

   ----------------
   -- Get_Plugin --
   ----------------

   function Get_Plugin (Name : String) return Aquarius_Plugin is
   begin
      if Loaded_Plugins.Contains (To_Plugin_Map_Name (Name)) then
         return Loaded_Plugins.Element (To_Plugin_Map_Name (Name));
      end if;
      if Name = "ebnf" then
         return Local_EBNF_Plugin;
      elsif Name = "plugin" then
         return Local_Plugin_Plugin;
      elsif False and then Name = "klein" then
         return Local_Klein_Plugin;
      elsif False and then Name = "ada" then
         return Local_Ada_Plugin;
      elsif Name = "project" then
         return Local_Ada_Plugin;
      elsif Name = "haskell" then
         return Local_Haskell_Plugin;
      elsif Name = "macro11" then
         return Local_Macro_11_Plugin;
      elsif Name = "macro32" then
         return Local_Macro_32_Plugin;
      elsif Name = "script" then
         return Local_Script_Plugin;
      else
         raise Constraint_Error with
           Name & ": no such plugin";
      end if;
   end Get_Plugin;

   ----------
   -- Load --
   ----------

   procedure Load (From_Grammar : Aquarius.Grammars.Aquarius_Grammar) is
      Name   : constant String := From_Grammar.Name;
      Plugin : Aquarius_Plugin;
   begin
      if Loaded_Plugins.Contains (To_Plugin_Map_Name (Name)) then
         return; -- n Loaded_Plugins.Element (To_Plugin_Map_Name (Name));
      end if;

      if not Aquarius.Library.Plugins_Enabled
        and then Name /= "ebnf"
        and then Name /= "project"
      then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "warning: skipping plugin " & Name &
                                 " because plugins are disabled");
         return;
      end if;

      if Name = "ebnf" then
         if Local_EBNF_Plugin = null then
            Local_EBNF_Plugin := new Aquarius.Plugins.EBNF.EBNF_Plugin;
         end if;
         Plugin := Local_EBNF_Plugin;
      elsif Name = "plugin" then
         if Local_Plugin_Plugin = null then
            Local_Plugin_Plugin := new Aquarius.Plugins.Plugin.Plugin_Plugin;
         end if;
         Plugin := Local_Plugin_Plugin;
      elsif False and then Name = "klein" then
         if Local_Klein_Plugin = null then
            Local_Klein_Plugin := new Aquarius.Plugins.Klein.Klein_Plugin;
         end if;
         Plugin := Local_Klein_Plugin;
      elsif False and then Name = "ada" then
         if Local_Ada_Plugin = null then
            Local_Ada_Plugin := new Ada_Plugin.Ada_Plugin_Type;
         end if;
         Plugin := Local_Ada_Plugin;
      elsif Name = "project" then
         if Local_Project_Plugin = null then
            Local_Project_Plugin := new Project_Plugin.Project_Plugin_Type;
         end if;
         Plugin := Local_Project_Plugin;
      elsif Name = "script" then
         if Local_Script_Plugin = null then
            Local_Script_Plugin := new Script_Plugin.Script_Plugin_Type;
         end if;
         Plugin := Local_Script_Plugin;
      elsif Name = "macro11" then
         if Local_Macro_11_Plugin = null then
            Local_Macro_11_Plugin := new Macro_11.Macro_11_Plugin;
         end if;
         Plugin := Local_Macro_11_Plugin;
      elsif Name = "macro32" then
         if Local_Macro_32_Plugin = null then
            Local_Macro_32_Plugin := new Macro_32.Macro_32_Plugin;
         end if;
         Plugin := Local_Macro_32_Plugin;
      elsif Name = "haskell" then
         if Local_Haskell_Plugin = null then
            Local_Haskell_Plugin := new Haskell.Haskell_Plugin_Type;
         end if;
         Plugin := Local_Haskell_Plugin;
      else
         declare
            Path : constant String :=
              Aquarius.Configuration.Get_Grammar_Path (Name) &
              Name & ".plugin";
         begin
            if not Ada.Directories.Exists (Path) then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     "warning: grammar " & Name &
                                       " has no associated plugin");
               return;
            else
               declare
                  Script_Grammar : constant Grammars.Aquarius_Grammar :=
                    Aquarius.Grammars.Manager.Get_Grammar ("script");
                  Script : Aquarius.Programs.Program_Tree;
               begin
                  Load (Script_Grammar);
                  Script :=
                    Aquarius.Loader.Load_From_File (Script_Grammar, Path);
                  Plugin := Aquarius.Plugins.Script_Plugin.Get_Plugin (Script);
               end;
            end if;
         end;
      end if;

      Ada.Text_IO.Put_Line ("Plugin: " & Plugin.Name & " version " &
                              Plugin.Version);

      Loaded_Plugins.Insert (To_Plugin_Map_Name (Name), Plugin);

      Plugin.Load (From_Grammar);
   end Load;

   --------------------------
   -- Loaded_Plugin_Report --
   --------------------------

   procedure Loaded_Plugin_Report is
   begin
      for Plugin of Loaded_Plugins loop
         Plugin.Report_State;
      end loop;
   end Loaded_Plugin_Report;

   ------------------------
   -- To_Plugin_Map_Name --
   ------------------------

   function To_Plugin_Map_Name
     (Name : String)
     return Plugin_Map_Name
   is
      Result : Plugin_Map_Name;
   begin
      Ada.Strings.Fixed.Move (Name, Result,
                              Drop => Ada.Strings.Right);
      return Result;
   end To_Plugin_Map_Name;

end Aquarius.Plugins.Manager;
