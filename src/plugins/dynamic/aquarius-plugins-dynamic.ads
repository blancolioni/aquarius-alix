private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Aqua.Execution;
with Aqua.Images;

package Aquarius.Plugins.Dynamic is

   type Dynamic_Plugin_Type is new Aquarius_Plugin_Type with private;

   overriding
   function Name (Plugin : Dynamic_Plugin_Type) return String;

   overriding
   function Version (Plugin : Dynamic_Plugin_Type) return String;

   overriding
   procedure Load (Plugin  : not null access Dynamic_Plugin_Type;
                   Grammar : Aquarius.Grammars.Aquarius_Grammar);

   overriding procedure Report_State
     (Plugin : Dynamic_Plugin_Type);

   function New_Dynamic_Plugin
     (Name    : String;
      Version : String)
     return Aquarius_Plugin;

   function Image
     (Plugin : Dynamic_Plugin_Type'Class)
      return Aqua.Images.Image_Type;

   function Executor
     (Plugin : Dynamic_Plugin_Type'Class)
      return access Aqua.Execution.Execution_Interface'Class;

   type Dynamic_Plugin is access all Dynamic_Plugin_Type'Class;

private

   package Defined_Property_Vectors is
      new Ada.Containers.Vectors (Positive,
                                  Aquarius.Properties.Property_Type,
                                  Aquarius.Properties."=");

   type Dynamic_Plugin_Access is access all Dynamic_Plugin_Type'Class;

   type Dynamic_Plugin_Type is new Aquarius_Plugin_Type with
      record
         Name       : Ada.Strings.Unbounded.Unbounded_String;
         Version    : Ada.Strings.Unbounded.Unbounded_String;
         Plugin     : Aquarius.Properties.Property_Type;
         Properties : Defined_Property_Vectors.Vector;
         Image      : Aqua.Images.Image_Type;
         Executor   : access Aqua.Execution.Execution_Interface'Class;
      end record;

end Aquarius.Plugins.Dynamic;
