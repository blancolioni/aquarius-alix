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
      --  UI.From_Config (Session_Config);
      UI.View_Left := Session_Config.Get ("view_left");
      UI.View_Top := Session_Config.Get ("view_top");
      UI.View_Width := Session_Config.Get ("view_width");
      UI.View_Height := Session_Config.Get ("view_height");

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
      --  UI.To_Config (Config);
      Config.Add ("view_left", UI.View_Left);
      Config.Add ("view_top", UI.View_Top);
      Config.Add ("view_width", UI.View_Width);
      Config.Add ("view_height", UI.View_Height);
      declare
         Program_Stores : Tropos.Configuration :=
                            Tropos.New_Config ("program_stores");
         Store          : Tropos.Configuration :=
                            Tropos.New_Config (UI.Program_Store.Config_Name);
      begin
         UI.Program_Store.To_Config (Store);
         Program_Stores.Add (Store);
         Config.Add (Program_Stores);
      end;

      Tropos.Writer.Write_Config (Config, Path);
   end Save_Session;

end Komnenos.UI.Sessions;
