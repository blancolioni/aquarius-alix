with Komnenos.UI.Gtk_UI;

package body Komnenos.UI is

   Local_Current_UI : Komnenos_UI;

   -------------------------
   -- Add_Cross_Reference --
   -------------------------

   overriding procedure Add_Cross_Reference
     (UI           : in out Root_Komnenos_UI;
      Item         : Komnenos.Entities.Entity_Reference;
      File_Name    : String;
      Line, Column : Natural;
      Ref_Type     : String)
   is
   begin
      UI.Entities.Add_Cross_Reference
        (Item, File_Name, Line, Column, Ref_Type);
   end Add_Cross_Reference;

   --------------------------
   -- Add_Entity_Reference --
   --------------------------

   overriding procedure Add_Entity
     (UI     : in out Root_Komnenos_UI;
      Key    : String;
      Entity : Komnenos.Entities.Entity_Reference)
   is
   begin
      UI.Entities.Add_Entity (Key, Entity);
   end Add_Entity;

   ---------------
   -- Create_UI --
   ---------------

   function Create_UI
     (Config_Folder_Path : String)
      return Komnenos_UI
   is
   begin
      Local_Current_UI := Komnenos.UI.Gtk_UI.Create_UI (Config_Folder_Path);
      return Local_Current_UI;
   end Create_UI;

   ----------------------
   -- Cross_References --
   ----------------------

   overriding function Cross_References
     (UI           : Root_Komnenos_UI;
      File_Name    : String;
      Line, Column : Positive;
      Enabled      : String := "")
      return Komnenos.Entities.Array_Of_Entities
   is
   begin
      return UI.Entities.Cross_References
        (File_Name, Line, Column, Enabled);
   end Cross_References;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI return Komnenos_UI is
   begin
      return Local_Current_UI;
   end Current_UI;

   ------------
   -- Exists --
   ------------

   overriding function Exists
     (UI  : Root_Komnenos_UI;
      Key : String)
      return Boolean
   is
   begin
      return UI.Entities.Exists (Key);
   end Exists;

   ---------
   -- Get --
   ---------

   overriding function Get
     (UI  : Root_Komnenos_UI;
      Key : String)
      return Komnenos.Entities.Entity_Reference
   is
   begin
      return UI.Entities.Get (Key);
   end Get;

   -------------
   -- Iterate --
   -------------

   overriding procedure Iterate
     (UI             : Root_Komnenos_UI;
      Filter         : in String;
      Process        : not null access
        procedure (Item : Komnenos.Entities.Entity_Reference);
      Top_Level_Only : Boolean := True)
   is
   begin
      UI.Entities.Iterate (Filter, Process, Top_Level_Only);
   end Iterate;

   ------------------------
   -- Location_File_Name --
   ------------------------

   overriding function Location_File_Name
     (UI       : Root_Komnenos_UI;
      Location : Komnenos.Entities.File_Location)
      return String
   is
   begin
      return UI.Entities.Location_File_Name (Location);
   end Location_File_Name;

   -------------------
   -- Program_Store --
   -------------------

   overriding function Program_Store
     (UI             : Root_Komnenos_UI)
      return access Komnenos.Entities.Program_Store_Interface'Class
   is
   begin
      return UI.Store;
   end Program_Store;

   ----------------
   -- References --
   ----------------

   overriding function References
     (UI     : Root_Komnenos_UI;
      Entity : Komnenos.Entities.Entity_Reference)
      return Komnenos.Entities.File_Location_Array
   is
   begin
      return UI.Entities.References (Entity);
   end References;

   -----------------------
   -- Set_Program_Store --
   -----------------------

   overriding procedure Set_Program_Store
     (UI    : in out Root_Komnenos_UI;
      Store : access Komnenos.Entities.Program_Store_Interface'Class)
   is
   begin
      UI.Store := Store;
   end Set_Program_Store;

   ----------
   -- Sort --
   ----------

   overriding procedure Sort
     (UI     : in out Root_Komnenos_UI)
   is
   begin
      UI.Entities.Sort;
   end Sort;

end Komnenos.UI;
