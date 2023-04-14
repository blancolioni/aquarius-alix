package As.Paths is

   Config_Path : constant String :=
     "D:\git\aquarius\tools\aqua\as\config";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end As.Paths;
