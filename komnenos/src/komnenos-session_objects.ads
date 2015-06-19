with Tropos;

package Komnenos.Session_Objects is

   type Session_Object_Interface is interface;

   procedure To_Config
     (Item : Session_Object_Interface;
      Config : in out Tropos.Configuration)
      is abstract;

   procedure From_Config
     (Item : in out Session_Object_Interface;
      Config : Tropos.Configuration)
   is abstract;

end Komnenos.Session_Objects;
