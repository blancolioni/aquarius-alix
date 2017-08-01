with Ada.Text_IO;

--  with Tagatha.Operands;

with Ack.Environment;
with Ack.Types;

package body Ack.Classes is

   Trace_Classes : constant Boolean := False;

   type String_Class_Record is
     new Class_Entity_Record with null record;

   overriding procedure Allocate
     (Class : String_Class_Record;
      Unit  : in out Tagatha.Units.Tagatha_Unit);

   -----------------
   -- Add_Feature --
   -----------------

   procedure Add_Feature
     (Class   : in out Class_Entity_Record'Class;
      Feature : not null access Ack.Features.Feature_Entity_Record'Class)
   is
   begin
      Class.Class_Features.Append (Feature);
      Root_Entity_Type (Class).Insert (Feature);
   end Add_Feature;

   ------------------------
   -- Add_Generic_Formal --
   ------------------------

   procedure Add_Generic_Formal
     (Class  : in out Class_Entity_Record'Class;
      Formal : not null access Ack.Types.Type_Entity_Record'Class)
   is
   begin
      Class.Formal_Arguments.Append (Formal);
      Class.Insert (Formal);
      Class.Generic_Class := True;
   end Add_Generic_Formal;

   --------------
   -- Add_Note --
   --------------

   procedure Add_Note
     (Class : in out Class_Entity_Record'Class;
      Name  : String;
      Value : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Class.Qualified_Name & ": " & Name & " = " & Value);
      Class.Notes.Insert (Name, Value);
      if Name = "behaviour" then
         if Value = "normal" then
            Class.Behaviour := Normal;
         elsif Value = "aqua_primitive" then
            Class.Behaviour := Aqua_Primitive;
         else
            raise Constraint_Error with
              "invalid behaviour: " & Value;
         end if;
      end if;
   end Add_Note;

   ---------------------
   -- Aliased_Feature --
   ---------------------

   function Aliased_Feature
     (Class : not null access constant Class_Entity_Record'Class;
      Alias : Name_Id)
      return Ack.Features.Feature_Entity
   is
   begin
      return Class.Find_Aliased_Feature (Alias);
   end Aliased_Feature;

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Class : Class_Entity_Record;
      Unit  : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      Unit.Call
        (Class_Entity_Record'Class (Class).Link_Name & "$allocate");
      Unit.Push_Result;
   end Allocate;

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Class : String_Class_Record;
      Unit  : in out Tagatha.Units.Tagatha_Unit)
   is
      pragma Unreferenced (Class);
   begin
      Unit.Push_Text ("");
   end Allocate;

   ----------
   -- Bind --
   ----------

   overriding procedure Bind
     (Class : in out Class_Entity_Record)
   is
      procedure Scan_Hierarchy (Top : Class_Entity);

      --------------------
      -- Scan_Hierarchy --
      --------------------

      procedure Scan_Hierarchy (Top : Class_Entity) is
      begin
         if not Class.Inherited_List.Contains (Top) then
            Class.Inherited_List.Append (Top);
            for Inherited of Top.Inherited_Types loop
               Scan_Hierarchy (Inherited.Inherited_Type.Class);
            end loop;
         end if;
      end Scan_Hierarchy;

   begin
      if Trace_Classes then
         Ada.Text_IO.Put_Line ("binding: " & Class.Description);
      end if;
      for Inherited of Class.Inherited_Types loop
         Scan_Hierarchy (Inherited.Inherited_Type.Class);
      end loop;
   end Bind;

   -----------------
   -- Conforms_To --
   -----------------

   overriding function Conforms_To
     (Class : not null access constant Class_Entity_Record;
      Other : not null access constant Root_Entity_Type'Class)
      return Boolean
   is
      Ancestor : constant Constant_Class_Entity :=
                   (if Other.all in Class_Entity_Record'Class
                    then Constant_Class_Entity (Other)
                    elsif Other.all in
                      Ack.Types.Type_Entity_Record'Class
                    then Ack.Types.Type_Entity_Record'Class (Other.all).Class
                    else null);

      function Try
        (Current : Constant_Class_Entity)
         return Boolean;

      ---------
      -- Try --
      ---------

      function Try
        (Current : Constant_Class_Entity)
         return Boolean
      is
      begin
         if Current = Ancestor then
            return True;
         else
            for Inherited of Current.Inherited_Types loop
               if Try (Constant_Class_Entity
                       (Inherited.Inherited_Type.Class))
               then
                  return True;
               end if;
            end loop;
            return False;
         end if;
      end Try;

   begin

      if Class.Standard_Name = "none" then
         return True;
      end if;

      if Ancestor = null then
         return False;
      end if;

      if Ancestor.Standard_Name = "any" then
         return True;
      end if;

      return Try (Constant_Class_Entity (Class));
   end Conforms_To;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Class     : Class_Entity_Record;
      Name      : String;
      Recursive : Boolean := True)
      return Boolean
   is
   begin
      if Root_Entity_Type (Class).Contains (Name, False) then
         return True;
      elsif Recursive then
         for Inherited of Class.Inherited_List loop
            if Inherited.Contains (Name, False) then
               return True;
            end if;
         end loop;
         return Ack.Environment.Top_Level.Contains (Name);
      else
         return False;
      end if;
   end Contains;

   -------------
   -- Feature --
   -------------

   function Feature
     (Class : not null access constant Class_Entity_Record'Class;
      Name  : Name_Id)
      return Ack.Features.Feature_Entity
   is
   begin
      return Ack.Features.Feature_Entity (Class.Get (Name));
   end Feature;

   --------------------------
   -- Find_Aliased_Feature --
   --------------------------

   function Find_Aliased_Feature
     (Class   : Class_Entity_Record'Class;
      Alias   : Name_Id)
      return Ack.Features.Feature_Entity
   is
      function Test (Feature : not null access constant
                       Ack.Features.Feature_Entity_Record'Class)
                     return Boolean
      is (Feature.Alias = Alias);
   begin
      return Class.Find_Feature (Test'Access);
   end Find_Aliased_Feature;

   ------------------
   -- Find_Feature --
   ------------------

   function Find_Feature
     (Class   : Class_Entity_Record'Class;
      Test    : not null access
        function (Feature : not null access constant
                    Ack.Features.Feature_Entity_Record'Class)
      return Boolean)
      return Ack.Features.Feature_Entity
   is
   begin
      for Feature of Class.Class_Features loop
         if Test (Feature) then
            return Feature;
         end if;
      end loop;

      for Inherited of Class.Inherited_List loop
         for Feature of Inherited.Class_Features loop
            if Test (Feature) then
               return Feature;
            end if;
         end loop;
      end loop;

      return null;
   end Find_Feature;

   --------------------
   -- Generic_Formal --
   --------------------

   function Generic_Formal
     (Class : Class_Entity_Record'Class;
      Index : Positive)
      return access constant Ack.Types.Type_Entity_Record'Class
   is
      Count : Natural := 0;
   begin
      for Formal of Class.Formal_Arguments loop
         Count := Count + 1;
         if Count = Index then
            return Ack.Types.Type_Entity (Formal);
         end if;
      end loop;
      raise Constraint_Error with
        "violated precondition";
   end Generic_Formal;

   --------------------------
   -- Generic_Formal_Count --
   --------------------------

   function Generic_Formal_Count
     (Class : Class_Entity_Record'Class)
      return Natural
   is
   begin
      return Natural (Class.Formal_Arguments.Length);
   end Generic_Formal_Count;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Class : not null access constant Class_Entity_Record;
      Name  : String)
      return Entity_Type
   is
   begin
      if Root_Entity_Type (Class.all).Contains (Name, False) then
         return Root_Entity_Type (Class.all).Get (Name);
      else
         for Inherited of Class.Inherited_List loop
            if Inherited.Contains (Name, False) then
               return Inherited.Get (Name);
            end if;
         end loop;
         return Ack.Environment.Top_Level.Get (Name);
      end if;
   end Get;

   -----------------------
   -- Get_Ancestor_Type --
   -----------------------

   function Get_Ancestor_Type
     (Descendent_Class : Class_Entity_Record'Class;
      Descendent_Type  : not null access constant
        Ack.Types.Type_Entity_Record'Class;
      Ancestor_Class   : not null access constant Class_Entity_Record'Class)
      return access Ack.Types.Type_Entity_Record'Class
   is
      function Get
        (Current_Type : not null access constant
           Ack.Types.Type_Entity_Record'Class;
         Current_Class : Class_Entity_Record'Class)
         return Ack.Types.Type_Entity;

      ---------
      -- Get --
      ---------

      function Get
        (Current_Type  : not null access constant
           Ack.Types.Type_Entity_Record'Class;
         Current_Class : Class_Entity_Record'Class)
         return Ack.Types.Type_Entity
      is
         use type Ack.Types.Type_Entity;
         Result : Ack.Types.Type_Entity := null;
      begin
         for Inherited of Current_Class.Inherited_Types loop
            if Inherited.Inherited_Type.Class = Ancestor_Class then
               Result := Ack.Types.Type_Entity (Inherited.Inherited_Type);
               exit;
            end if;
         end loop;

         if Result = null then
            for Inherited of Current_Class.Inherited_Types loop
               declare
                  Ancestor : constant access
                    Ack.Types.Type_Entity_Record'Class :=
                      Get (Inherited.Inherited_Type,
                           Inherited.Inherited_Type.Class.all);
               begin
                  if Ancestor /= null then
                     Result := Ack.Types.Type_Entity (Ancestor);
                     exit;
                  end if;
               end;
            end loop;
         end if;

         if Result /= null then
            Result :=
              Ack.Types.Update_Type_Instantiation
                (Result, Current_Type);
         end if;
         return Result;
      end Get;

   begin
      return Get (Descendent_Type, Descendent_Class);
   end Get_Ancestor_Type;

   ----------------------
   -- Get_Class_Entity --
   ----------------------

   function Get_Class_Entity
     (Node : Node_Id)
      return Class_Entity
   is
   begin
      return Class_Entity (Get_Entity (Node));
   end Get_Class_Entity;

   -------------------------
   -- Get_Top_Level_Class --
   -------------------------

   function Get_Top_Level_Class
     (Name : String)
      return Class_Entity
   is
   begin
      if Ack.Environment.Top_Level.Contains (Name) then
         return Class_Entity (Ack.Environment.Top_Level.Get (Name));
      else
         return null;
      end if;
   end Get_Top_Level_Class;

   -------------------------
   -- Has_Aliased_Feature --
   -------------------------

   function Has_Aliased_Feature
     (Class : not null access constant Class_Entity_Record'Class;
      Alias : Name_Id)
      return Boolean
   is
      use type Ack.Features.Feature_Entity;
   begin
      return Class.Find_Aliased_Feature (Alias) /= null;
   end Has_Aliased_Feature;

   -------------
   -- Inherit --
   -------------

   procedure Inherit
     (Class           : in out Class_Entity_Record'Class;
      Inherited_Type  : not null access Ack.Types.Type_Entity_Record'Class)
   is
   begin
      Class.Inherited_Types.Append
        ((Inherited_Type => Ack.Types.Type_Entity (Inherited_Type),
          others          => <>));
   end Inherit;

   ---------------------
   -- Is_Redefinition --
   ---------------------

   function Is_Redefinition
     (Class        : Class_Entity_Record'Class;
      Feature_Name : Name_Id)
      return Boolean
   is
   begin
      for Inherited of Class.Inherited_Types loop
         declare
            Local_Name : Name_Id := Feature_Name;
         begin
            for Rename of Inherited.Renamed_Features loop
               if Rename.New_Name = Feature_Name then
                  Local_Name := Rename.Old_Name;
                  exit;
               end if;
            end loop;

            for Feature of Inherited.Redefined_Features loop
               if Get_Name_Id (Feature.Standard_Name) = Local_Name then
                  return True;
               end if;
            end loop;
         end;
      end loop;
      return False;
   end Is_Redefinition;

   ---------------
   -- Is_Rename --
   ---------------

   function Is_Rename
     (Class        : Class_Entity_Record'Class;
      Feature_Name : Name_Id)
      return Boolean
   is
   begin
      for Inherited of Class.Inherited_Types loop
         for Rename of Inherited.Renamed_Features loop
            if Rename.New_Name = Feature_Name then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Is_Rename;

   ---------------
   -- New_Class --
   ---------------

   function New_Class
     (Name        : Name_Id;
      Context     : Class_Entity;
      Declaration : Node_Id)
      return Class_Entity
   is
      Class_Name : constant String := To_Standard_String (Name);
      Result : constant Class_Entity :=
                     (if Class_Name = "string"
                      then new String_Class_Record
                      else new Class_Entity_Record);
   begin
      Result.Create
        (Name, Declaration,
         Table              => True,
         Parent_Environment => Ack.Environment.Top_Level,
         Context            => Context);
      return Result;
   end New_Class;

   --------------
   -- Redefine --
   --------------

--     procedure Redefine
--       (Class           : in out Class_Entity_Record'Class;
--        Inherited_Class : not null access Class_Entity_Record'Class;
--        Feature_Name    : Name_Id)
--     is
--     begin
--        for Inherited of Class.Inherited_Classes loop
--           if Inherited.Inherited_Class = Inherited_Class then
--              Inherited.Redefined_Features.Append
--                (Inherited_Class.Feature (Feature_Name));
--              exit;
--           end if;
--        end loop;
--     end Redefine;

   ------------
   -- Rename --
   ------------

   procedure Rename
     (Class            : in out Class_Entity_Record'Class;
      Inherited_Class  : not null access Class_Entity_Record'Class;
      Feature_Name     : Name_Id;
      New_Feature_Name : Name_Id)
   is
   begin
      for Inherited of Class.Inherited_Types loop
         if Inherited.Inherited_Type.Class = Inherited_Class then
            Inherited.Renamed_Features.Append
              ((Feature_Name, New_Feature_Name));
            exit;
         end if;
      end loop;
   end Rename;

   --------------------
   -- Scan_Ancestors --
   --------------------

   procedure Scan_Ancestors
     (Class            : not null access constant Class_Entity_Record'Class;
      Proper_Ancestors : Boolean;
      Process          : not null access
        procedure (Ancestor : not null access constant
                     Class_Entity_Record'Class))
   is
      package Name_Sets is
        new WL.String_Maps (Boolean);

      Scanned : Name_Sets.Map;

      procedure Scan
        (Ancestor : not null access constant
           Class_Entity_Record'Class);

      ----------
      -- Scan --
      ----------

      procedure Scan
        (Ancestor : not null access constant
           Class_Entity_Record'Class)
      is
      begin
         for Inherited of Ancestor.Inherited_Types loop
            if not Scanned.Contains
              (Inherited.Inherited_Type.Class.Qualified_Name)
            then
               Scanned.Insert
                 (Inherited.Inherited_Type.Class.Qualified_Name, True);
               Process (Inherited.Inherited_Type.Class);
               Scan (Inherited.Inherited_Type.Class);
            end if;
         end loop;
      end Scan;

   begin
      Scan (Class);

      if not Proper_Ancestors then
         Process (Class);
      end if;
   end Scan_Ancestors;

   -------------------
   -- Scan_Features --
   -------------------

   procedure Scan_Features
     (Class : Class_Entity_Record'Class;
      Process : not null access
        procedure (Feature : not null access constant
                     Ack.Features.Feature_Entity_Record'Class))
   is
      function Always (Feature : not null access constant
                         Ack.Features.Feature_Entity_Record'Class)
                       return Boolean
      is (True);

   begin
      Class.Scan_Features (Always'Access, Process);
   end Scan_Features;

   -------------------
   -- Scan_Features --
   -------------------

   procedure Scan_Features
     (Class   : Class_Entity_Record'Class;
      Test    : not null access
        function (Feature : not null access constant
                    Ack.Features.Feature_Entity_Record'Class)
          return Boolean;
      Process : not null access
        procedure (Feature : not null access constant
                     Ack.Features.Feature_Entity_Record'Class))
   is
      package Name_Sets is new WL.String_Maps (Boolean);

      Scanned : Name_Sets.Map;

   begin
      for Feature of Class.Class_Features loop
         if Test (Feature) then
            Scanned.Insert (Feature.Standard_Name, True);
            Process (Feature);
         end if;
      end loop;
      for Inherited of Class.Inherited_List loop
         for Feature of Inherited.Class_Features loop
            if not Scanned.Contains (Feature.Standard_Name)
              and then Test (Feature)
            then
               Scanned.Insert (Feature.Standard_Name, True);
               Process (Feature);
            end if;
         end loop;
      end loop;
   end Scan_Features;

end Ack.Classes;
