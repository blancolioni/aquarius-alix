with Ack.Environment;

package body Ack.Classes is

   --------------------
   -- Add_Constraint --
   --------------------

   procedure Add_Constraint
     (Formal     : in out Class_Entity_Record'Class;
      Class      : Class_Entity)
   is
   begin
      Formal.Constraints.Append (Class);
   end Add_Constraint;

   -----------------
   -- Add_Feature --
   -----------------

   procedure Add_Feature
     (Class   : in out Class_Entity_Record'Class;
      Feature : not null access Ack.Features.Feature_Entity_Record'Class)
   is
   begin
      Class.Class_Features.Append (Feature);
   end Add_Feature;

   ------------------------
   -- Add_Generic_Formal --
   ------------------------

   procedure Add_Generic_Formal
     (Class  : in out Class_Entity_Record'Class;
      Formal : not null access Class_Entity_Record'Class)
   is
   begin
      Class.Formal_Arguments.Append (Class_Entity (Formal));
      Class.Insert (Formal);
      Class.Generic_Class := True;
   end Add_Generic_Formal;

   ----------
   -- Bind --
   ----------

   overriding procedure Bind
     (Class : in out Class_Entity_Record)
   is
   begin
      for Feature of Class.Class_Features loop
         Class.Insert (Feature);
      end loop;
      for Inherited of Class.Inherited_Classes loop
         for Feature of Inherited.Inherited_Class.Class_Features loop
            if not Inherited.Redefined_Features.Contains (Feature) then
               Class.Insert (Feature);
            end if;
         end loop;
      end loop;
   end Bind;

   -----------------
   -- Conforms_To --
   -----------------

   overriding function Conforms_To
     (Class : not null access Class_Entity_Record;
      Other : not null access Root_Entity_Type'Class)
      return Boolean
   is
      Ancestor : constant Class_Entity :=
                   (if Other.all in Class_Entity_Record'Class
                    then Class_Entity (Other) else null);

      function Try (Current : Class_Entity) return Boolean;

      ---------
      -- Try --
      ---------

      function Try (Current : Class_Entity) return Boolean is
      begin
         if Current = Ancestor then
            return True;
         else
            for Inherited of Current.Inherited_Classes loop
               if Try (Inherited.Inherited_Class) then
                  return True;
               end if;
            end loop;
            return False;
         end if;
      end Try;

   begin
      if Ancestor = null then
         return False;
      end if;

      return Try (Class_Entity (Class));
   end Conforms_To;

   ----------------------
   -- Detachable_Class --
   ----------------------

   function Detachable_Class
     (From_Class : not null access Class_Entity_Record'Class)
      return Class_Entity
   is
   begin
      return Result : constant Class_Entity := new Class_Entity_Record do
         Result.Detachable := True;
         Result.Reference_Class := Class_Entity (From_Class);
      end return;
   end Detachable_Class;

   -------------
   -- Feature --
   -------------

   function Feature
     (Class : Class_Entity_Record'Class;
      Name  : Name_Id)
      return Ack.Features.Feature_Entity
   is
   begin
      return Ack.Features.Feature_Entity (Class.Get (Name));
   end Feature;

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

   -------------
   -- Inherit --
   -------------

   procedure Inherit
     (Class           : in out Class_Entity_Record'Class;
      Inherited_Class : not null access Class_Entity_Record'Class)
   is
   begin
      Class.Inherited_Classes.Append
        ((Inherited_Class => Class_Entity (Inherited_Class),
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
      for Inherited of Class.Inherited_Classes loop
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
      for Inherited of Class.Inherited_Classes loop
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
   begin
      return Result : constant Class_Entity := new Class_Entity_Record do
         Result.Create
           (Name, Declaration,
            Parent_Environment => Ack.Environment.Top_Level,
            Context            => Context);
         Result.Reference_Class := Context;
         Result.Top_Level := Context = null;
      end return;
   end New_Class;

   ------------------------
   -- New_Generic_Formal --
   ------------------------

   function New_Generic_Formal
     (Name        : Name_Id;
      Declaration : Node_Id)
      return Class_Entity
   is
   begin
      return Formal : constant Class_Entity :=
        new Class_Entity_Record
      do
         Formal.Create (Name, Declaration, null, null);
         Formal.Formal_Argument := True;
      end return;
   end New_Generic_Formal;

   ----------------------------
   -- New_Instantiated_Class --
   ----------------------------

   function New_Instantiated_Class
     (Generic_Class   : Class_Entity;
      Generic_Actuals : Array_Of_Classes;
      Node            : Node_Id)
      return Class_Entity
   is
   begin
      return Result : constant Class_Entity := new Class_Entity_Record do
         Result.Create
           (Get_Name_Id (Generic_Class.Standard_Name), Node, null, null);
         Result.Instantiated_Class := True;
         Result.Reference_Class := Generic_Class;
         for Actual of Generic_Actuals loop
            Result.Actual_Arguments.Append (Actual);
         end loop;
      end return;
   end New_Instantiated_Class;

   --------------
   -- Redefine --
   --------------

   procedure Redefine
     (Class           : in out Class_Entity_Record'Class;
      Inherited_Class : not null access Class_Entity_Record'Class;
      Feature_Name    : Name_Id)
   is
   begin
      for Inherited of Class.Inherited_Classes loop
         if Inherited.Inherited_Class = Inherited_Class then
            Inherited.Redefined_Features.Append
              (Inherited_Class.Feature (Feature_Name));
            exit;
         end if;
      end loop;
   end Redefine;

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
      for Inherited of Class.Inherited_Classes loop
         if Inherited.Inherited_Class = Inherited_Class then
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
         for Inherited of Ancestor.Inherited_Classes loop
            if not Scanned.Contains
              (Inherited.Inherited_Class.Qualified_Name)
            then
               Scanned.Insert (Inherited.Inherited_Class.Qualified_Name, True);
               Process (Inherited.Inherited_Class);
               Scan (Inherited.Inherited_Class);
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
   begin
      for Feature of Class.Class_Features loop
         if Test (Feature) then
            Process (Feature);
         end if;
      end loop;
   end Scan_Features;

   --------------------------
   -- Scan_Old_Definitions --
   --------------------------

   procedure Scan_Old_Definitions
     (Class        : Class_Entity_Record'Class;
      Feature_Name : Name_Id;
      Process      : not null access
        procedure (Feature : not null access constant
                     Ack.Features.Feature_Entity_Record'Class))
   is
   begin
      for Inherited of Class.Inherited_Classes loop
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
                  Process (Feature);
               end if;
            end loop;
         end;
      end loop;
   end Scan_Old_Definitions;

end Ack.Classes;
