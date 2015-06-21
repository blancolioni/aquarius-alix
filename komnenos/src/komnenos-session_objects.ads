with Tropos;

package Komnenos.Session_Objects is

   type Session_Object_Interface is interface;

   function Config_Name
     (Item : Session_Object_Interface)
      return String
      is abstract;

   procedure To_Config
     (Item : Session_Object_Interface;
      Config : in out Tropos.Configuration)
   is abstract
     with Pre'Class =>
       Config.Config_Name = Session_Object_Interface'Class (Item).Config_Name;

   procedure From_Config
     (Item : not null access Session_Object_Interface;
      Config : Tropos.Configuration)
   is abstract;

   function Read_Config
     (Config : Tropos.Configuration)
      return access Session_Object_Interface'Class;

   type Session_Object_Constructor is access
     function return access Session_Object_Interface'Class;

   procedure Register_Session_Object
     (Name    : String;
      Reader  : Session_Object_Constructor);

end Komnenos.Session_Objects;
