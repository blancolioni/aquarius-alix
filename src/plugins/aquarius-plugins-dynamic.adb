with Aquarius.Grammars.Manager;

package body Aquarius.Plugins.Dynamic is

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
      Result : aliased Dynamic_Plugin_Type;
      Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
        Aquarius.Grammars.Manager.Get_Grammar (Name);
   begin
      Result.Name    := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Result.Version := Ada.Strings.Unbounded.To_Unbounded_String (Version);
      Aquarius_Plugin_Type'Class (Result).Load (Grammar);
      return new Dynamic_Plugin_Type'(Result);
   end New_Dynamic_Plugin;

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
