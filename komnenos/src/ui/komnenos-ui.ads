with Tropos;

with Aquarius.UI;

with Komnenos.Entities;
with Komnenos.Fragments;
with Komnenos.Session_Objects;

package Komnenos.UI is

   type Root_Komnenos_UI is
     abstract new Komnenos.Entities.Entity_Table_Interface
     and Komnenos.Session_Objects.Session_Object_Interface
     and Aquarius.UI.Root_UI_Type
   with private;

   overriding function Config_Name (UI : Root_Komnenos_UI) return String
   is ("ui");

   overriding procedure To_Config
     (UI     : Root_Komnenos_UI;
      Config : in out Tropos.Configuration);

   overriding procedure From_Config
     (UI : not null access Root_Komnenos_UI;
      Config : Tropos.Configuration);

   procedure Place_Fragment
     (UI       : in out Root_Komnenos_UI;
      Parent   : access Komnenos.Entities.Entity_Visual'Class;
      Offset   : Natural;
      Fragment : Komnenos.Fragments.Fragment_Type)
   is abstract;

   overriding procedure Add_Entity
     (UI     : in out Root_Komnenos_UI;
      Key    : String;
      Entity : Komnenos.Entities.Entity_Reference);

   overriding procedure Add_Cross_Reference
     (UI           : in out Root_Komnenos_UI;
      Item         : Komnenos.Entities.Entity_Reference;
      File_Name    : String;
      Line, Column : Natural;
      Ref_Type     : String);

   overriding function Cross_References
     (UI           : Root_Komnenos_UI;
      File_Name    : String;
      Line, Column : Positive;
      Enabled      : String := "")
      return Komnenos.Entities.Array_Of_Entities;

   overriding function References
     (UI     : Root_Komnenos_UI;
      Entity : Komnenos.Entities.Entity_Reference)
      return Komnenos.Entities.File_Location_Array;

   overriding function Exists
     (UI  : Root_Komnenos_UI;
      Key : String)
      return Boolean;

   overriding function Get
     (UI  : Root_Komnenos_UI;
      Key : String)
      return Komnenos.Entities.Entity_Reference;

   overriding function Location_File_Name
     (UI       : Root_Komnenos_UI;
      Location : Komnenos.Entities.File_Location)
      return String;

   function Active_Fragment
     (UI : Root_Komnenos_UI)
      return Komnenos.Fragments.Fragment_Type
      is abstract;

   overriding procedure Sort
     (UI     : in out Root_Komnenos_UI);

   overriding procedure Iterate
     (UI             : Root_Komnenos_UI;
      Filter         : in String;
      Process        : not null access
        procedure (Item : Komnenos.Entities.Entity_Reference);
      Top_Level_Only : Boolean := True);

--     function Find_Entity
--       (UI     : Root_Komnenos_UI;
--        Source : String;
--        Line   : Natural;
--        Column : Natural)
--        return Komnenos.Entities.Entity_Reference;

   overriding function Program_Store
     (UI             : Root_Komnenos_UI)
      return access Komnenos.Entities.Program_Store_Interface'Class;

   overriding procedure Set_Program_Store
     (UI    : in out Root_Komnenos_UI;
      Store : access Komnenos.Entities.Program_Store_Interface'Class);

   type Komnenos_UI is access all Root_Komnenos_UI'Class;

   function Create_UI
     (Config_Folder_Path : String)
      return Komnenos_UI;

   function Current_UI return Komnenos_UI;

private

   type Root_Komnenos_UI is
     abstract new Komnenos.Entities.Entity_Table_Interface
     and Komnenos.Session_Objects.Session_Object_Interface
     and Aquarius.UI.Root_UI_Type with
      record
         View_Left   : Integer;
         View_Top    : Integer;
         View_Height : Natural;
         View_Width  : Natural;
         Entities    : Komnenos.Entities.Entity_Table;
         Store       : access Komnenos.Entities.Program_Store_Interface'Class;
      end record;

end Komnenos.UI;
