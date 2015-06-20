with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Strings.Fixed.Equal_Case_Insensitive;

package body Komnenos.Session_Objects is

   package Constructor_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Session_Object_Constructor,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   Constructors : Constructor_Maps.Map;

   -----------------
   -- Read_Config --
   -----------------

   function Read_Config
     (Config : Tropos.Configuration)
      return access Session_Object_Interface'Class
   is
      pragma Assert (Constructors.Contains (Config.Config_Name));
      Constructor : constant Session_Object_Constructor :=
                      Constructors (Config.Config_Name);
      Result      : constant access Session_Object_Interface'Class :=
                      Constructor.all;
   begin
      Result.From_Config (Config);
      return Result;
   end Read_Config;

   -----------------------------
   -- Register_Session_Object --
   -----------------------------

   procedure Register_Session_Object
     (Name    : String;
      Reader  : Session_Object_Constructor)
   is
   begin
      Constructors.Insert (Name, Reader);
   end Register_Session_Object;

end Komnenos.Session_Objects;
