package Aquarius.Version is

   Version_Major       : constant := 0;
   Version_Minor       : constant := 1;
   Version_Release     : constant := 0;

   Build_Number        : constant := 1;

   Release_Name        : constant String := "carrie";

   Version_String      : constant String := "0.1.0";

   Version_Long_String : constant String :=
     "Aquarius version " & Version_String;

end Aquarius.Version;
