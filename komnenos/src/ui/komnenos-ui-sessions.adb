with Tropos.Reader;
with Tropos.Writer;

package body Komnenos.UI.Sessions is

   ------------------
   -- Load_Session --
   ------------------

   procedure Load_Session
     (UI   : Komnenos_UI;
      Path : String)
   is
      Session_Config : constant Tropos.Configuration :=
                         Tropos.Reader.Read_Config (Path);
   begin
      if Session_Config.Contains ("program_stores") then
         for Config of Session_Config.Child ("program_stores") loop
            declare
               subtype Base is Session_Objects.Session_Object_Interface'Class;
               Item : constant access Base :=
                        Session_Objects.Read_Config (Config);
            begin
               UI.Store :=
                 Entities.Program_Store_Interface'Class
                   (Item.all)'Access;
               UI.Store.Load;
            end;
         end loop;
      end if;

      if Session_Config.Contains ("ui") then
         UI.From_Config (Session_Config.Child ("ui"));
      end if;

   end Load_Session;

   ------------------
   -- Save_Session --
   ------------------

   procedure Save_Session
     (UI   : Komnenos_UI;
      Path : String)
   is
      Config : Tropos.Configuration; -- := UI.To_Config;
   begin
      if UI.Program_Store /= null then
         declare
            Program_Stores : Tropos.Configuration :=
                               Tropos.New_Config ("program_stores");
            Store          : Tropos.Configuration :=
                               Tropos.New_Config
                                 (UI.Program_Store.Config_Name);
         begin
            UI.Program_Store.To_Config (Store);
            Program_Stores.Add (Store);
            Config.Add (Program_Stores);
         end;
      end if;

      declare
         UI_Config : Tropos.Configuration :=
                       Tropos.New_Config ("ui");
      begin
         UI.To_Config (UI_Config);
         Config.Add (UI_Config);
      end;

      Tropos.Writer.Write_Config (Config, Path);

      UI.Store.Save;

   end Save_Session;

end Komnenos.UI.Sessions;
