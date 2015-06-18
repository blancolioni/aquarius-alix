with Tropos.Reader;

with Komnenos.Configure.Styles;

package body Komnenos.Configure is

   ------------------------
   -- Read_Configuration --
   ------------------------

   procedure Read_Configuration
     (Config_Folder_Path : String)
   is
   begin
      Komnenos.Configure.Styles.Configure_Styles
        (Config =>
           Tropos.Reader.Read_Config
             (Config_Folder_Path & "/styles.txt"));
   end Read_Configuration;

end Komnenos.Configure;
