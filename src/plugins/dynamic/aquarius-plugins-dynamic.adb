with Aquarius.Grammars.Manager;
with Aquarius.Programs.Aqua_Driver;

with Aqua.CPU;
with Aqua.Drivers;

package body Aquarius.Plugins.Dynamic is

   --------------
   -- Executor --
   --------------

   function Executor
     (Plugin : Dynamic_Plugin_Type'Class)
      return access Aqua.Execution.Execution_Interface'Class
   is
   begin
      return Plugin.Executor;
   end Executor;

   -----------
   -- Image --
   -----------

   function Image
     (Plugin : Dynamic_Plugin_Type'Class)
      return Aqua.Images.Image_Type
   is
   begin
      return Plugin.Image;
   end Image;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Plugin  : not null access Dynamic_Plugin_Type;
      Grammar : Aquarius.Grammars.Aquarius_Grammar)
   is
   begin
      Aquarius_Plugin_Type (Plugin.all).Load (Grammar);
   end Load;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Plugin : Dynamic_Plugin_Type)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Plugin.Name);
   end Name;

   ------------------------
   -- New_Dynamic_Plugin --
   ------------------------

   function New_Dynamic_Plugin
     (Name    : String;
      Version : String)
     return Aquarius_Plugin
   is
      Result : constant Dynamic_Plugin_Access := new Dynamic_Plugin_Type;
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Manager.Get_Grammar (Name);
   begin
      Result.Name    := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Result.Version := Ada.Strings.Unbounded.To_Unbounded_String (Version);
      Result.Image   := Aqua.Images.New_Image;
      Result.Image.Install_Driver
        (Start  => 16#0100#,
         Driver => Aqua.Drivers.Text_Writer);
      Result.Image.Install_Driver
        (Start  => 16#0110#,
         Driver => Aqua.Drivers.Character_Handling);
      Result.Image.Install_Driver
        (Start  => 16#0400#,
         Driver => Aquarius.Programs.Aqua_Driver.Aquarius_Tree_Driver);
      Result.Executor :=
        new Aqua.CPU.Aqua_CPU_Type (Result.Image);
      Aquarius_Plugin_Type'Class (Result.all).Load (Grammar);
      return Aquarius_Plugin (Result);
   end New_Dynamic_Plugin;

   ------------------
   -- Report_State --
   ------------------

   overriding procedure Report_State
     (Plugin : Dynamic_Plugin_Type)
   is
   begin
      Plugin.Executor.Report;
   end Report_State;

   -------------
   -- Version --
   -------------

   overriding function Version
     (Plugin : Dynamic_Plugin_Type)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Plugin.Version);
   end Version;

end Aquarius.Plugins.Dynamic;
