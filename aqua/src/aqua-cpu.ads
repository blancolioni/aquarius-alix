private with Ada.Calendar;
private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash;

with Ada.Finalization;

private with Aqua.Architecture;
private with Aqua.String_Vectors;

with Aqua.Execution;
with Aqua.Images;

package Aqua.CPU is

   Halt_Instruction : exception;
   Runtime_Error    : exception;

   type Aqua_CPU_Type
     (Image : access Aqua.Images.Root_Image_Type'Class;
      Load  : access Aqua.Execution.Loader_Interface'Class) is
   limited new Ada.Finalization.Limited_Controlled
     and Aqua.Execution.Execution_Interface with private;

   overriding procedure Initialize
     (CPU : in out Aqua_CPU_Type);

   overriding procedure Finalize
     (CPU : in out Aqua_CPU_Type);

   overriding procedure Execute
     (CPU       : in out Aqua_CPU_Type;
      Start     : Address;
      Arguments : Array_Of_Words);

   overriding function To_Word
     (CPU  : in out Aqua_CPU_Type;
      Item : not null access External_Object_Interface'Class)
      return Word;

   overriding function To_External_Object
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
      return access External_Object_Interface'Class
     with Pre => Is_External_Reference (Value);

   overriding function To_String
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
      return String;

   overriding function To_Integer
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
      return Aqua_Integer;

   overriding function To_String_Word
     (CPU  : in out Aqua_CPU_Type;
      Text : String)
      return Word;

   overriding function Show
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
      return String;

   function Name
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
      return String;

   overriding procedure Report
     (CPU : Aqua_CPU_Type);

   overriding function Loader
     (CPU : Aqua_CPU_Type)
      return access Aqua.Execution.Loader_Interface'Class
   is (CPU.Load);

private

   type External_Object_Access is
     access all External_Object_Interface'Class;

   package External_Object_Vectors is
      new Ada.Containers.Vectors (Positive, External_Object_Access);

   package String_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Word,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   type Aqua_CPU_Type
     (Image : access Aqua.Images.Root_Image_Type'Class;
      Load : access Aqua.Execution.Loader_Interface'Class) is
   limited new Ada.Finalization.Limited_Controlled
     and Aqua.Execution.Execution_Interface with
      record
--         Image      : Aqua.Images.Image_Type;
         R          : Aqua.Architecture.Registers :=
                        (Architecture.R_PC => 0,
                         Architecture.R_SP => 16#1FFF_FFFC#,
                         others => 16#BAAD_F00D#);
         N, Z, C, V : Boolean := False;
         B          : Boolean := False;
         Ext        : External_Object_Vectors.Vector;
         Str        : Aqua.String_Vectors.Vector;
         Str_Map    : String_Maps.Map;
         Start      : Ada.Calendar.Time;
         Exec_Time  : Duration := 0.0;
      end record;

   overriding function Pop
     (CPU : in out Aqua_CPU_Type)
      return Word;

   overriding procedure Push
     (CPU : in out Aqua_CPU_Type;
      Value : Word);

   procedure Show_Stack
     (CPU : in out Aqua_CPU_Type);

end Aqua.CPU;
