with Ada.Characters.Handling;
with Ada.Containers.Ordered_Maps;

package body Aquarius.Ack is

   package Entity_Maps is
     new Ada.Containers.Ordered_Maps
       (Name_Id, Entity_Id);

   Top_Level_Entities : Entity_Maps.Map;

   function To_Lower_Case_String
     (Name : Name_Id)
      return String
   is (Ada.Characters.Handling.To_Lower (To_String (Name)));

   ------------
   -- Append --
   ------------

   procedure Append (List : List_Id;
                     Node : Node_Id)
   is
   begin
      List_Table (List).List.Append (Node);
   end Append;

   -------------------
   -- Contains_Name --
   -------------------

   function Contains_Name
     (List : List_Id;
      Name : Name_Id)
      return Boolean
   is
   begin
      for Item of List_Table (List).List loop
         if Get_Name (Item) = Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Name;

   ----------------------
   -- Depth_First_Scan --
   ----------------------

   procedure Depth_First_Scan
     (Top     : Node_Id;
      Process : not null access
        procedure (Node : Node_Id))
   is
      procedure Scan (Node : Node_Id);

      ----------
      -- Scan --
      ----------

      procedure Scan (Node : Node_Id) is
         Rec : Node_Record renames Node_Table (Node);
      begin
         Process (Node);
         for N of Rec.Field loop
            if N in Real_Node_Id then
               Scan (N);
            end if;
         end loop;

         if Rec.List /= No_List then
            for N of List_Table (Rec.List).List loop
               Scan (N);
            end loop;
         end if;
      end Scan;

   begin
      Scan (Top);
   end Depth_First_Scan;

   -----------
   -- Error --
   -----------

   procedure Error
     (Node   : Node_Id;
      Kind   : Error_Kind;
      Entity : Entity_Id := No_Entity)
   is
   begin
      Node_Table (Node).Error := Kind;
      Node_Table (Node).Error_Entity := Entity;
   end Error;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Entity
     (Context : Entity_Id;
      Name    : Name_Id)
      return Entity_Id
   is
      Result : constant Entity_Id := Find_Local_Entity (Context, Name);
   begin
      if Result = Undeclared_Entity
        and then Context /= No_Entity
      then
         return Find_Entity (Entity_Table (Context).Context, Name);
      else
         return Result;
      end if;
   end Find_Entity;

   -----------------------
   -- Find_Local_Entity --
   -----------------------

   function Find_Local_Entity
     (Context : Entity_Id;
      Name    : Name_Id)
      return Entity_Id
   is
   begin
      if Context = No_Entity then
         declare
            Position : constant Entity_Maps.Cursor :=
                         Top_Level_Entities.Find (Name);
         begin
            if Entity_Maps.Has_Element (Position) then
               return Entity_Maps.Element (Position);
            else
               return Undeclared_Entity;
            end if;
         end;
      else
         declare
            Entity_Rec : Entity_Record renames Entity_Table (Context);
         begin
            for Item of Entity_Rec.Children loop
               if Get_Name (Item) = Name then
                  return Item;
               end if;
            end loop;
         end;
         return Undeclared_Entity;
      end if;
   end Find_Local_Entity;

   ------------------
   -- Find_Name_Id --
   ------------------

   function Find_Name_Id
     (Name              : String)
      return Name_Id
   is
   begin
      if Name_Map.Contains (Name) then
         return Name_Map.Element (Name);
      else
         return No_Name;
      end if;
   end Find_Name_Id;

   -------------------
   -- Get_Link_Name --
   -------------------

   function Get_Link_Name (Entity : Entity_Id) return String is
      Local_Name : constant String :=
                     To_Lower_Case_String (Get_Name (Entity));
   begin
      if Get_Context (Entity) = No_Entity then
         return Local_Name;
      else
         return Get_Link_Name (Get_Defined_In (Entity)) & "__" & Local_Name;
      end if;
   end Get_Link_Name;

   -----------------
   -- Get_Name_Id --
   -----------------

   function Get_Name_Id
     (Name              : String)
      return Name_Id
   is
   begin
      if Name_Map.Contains (Name) then
         return Name_Map.Element (Name);
      else
         Name_Table.Append (Name);
         Name_Map.Insert (Name, Name_Table.Last_Index);
         return Name_Table.Last_Index;
      end if;
   end Get_Name_Id;

   --------------------
   -- Inherit_Entity --
   --------------------

   procedure Inherit_Entity
     (Entity        : Entity_Id;
      Derived_Class : Entity_Id;
      Declaration   : Node_Id;
      Redefine      : Boolean;
      Rename        : Name_Id)
   is
      Result : constant Entity_Id :=
                 New_Entity
                   (Name        => (if Rename = No_Name
                                    then Get_Name (Entity)
                                    else Rename),
                    Kind        => Get_Kind (Entity),
                    Context     => Derived_Class,
                    Declaration => Declaration,
                    Entity_Type => Get_Type (Entity));
   begin
      Entity_Table (Result).Redefine := Redefine;
      Entity_Table (Result).Inherited_From := Get_Context (Entity);

      Entity_Table (Result).Defined_In :=
        (if Redefine then Derived_Class else Get_Context (Entity));
   end Inherit_Entity;

   ----------------
   -- New_Entity --
   ----------------

   function New_Entity
     (Name        : Name_Id;
      Kind        : Entity_Kind;
      Context     : Entity_Id;
      Declaration : Node_Id;
      Entity_Type : Entity_Id)
      return Entity_Id
   is
      Existing : constant Entity_Id :=
                   Find_Local_Entity (Context, Name);
      Offset   : Natural := 0;
   begin

      if Existing /= No_Entity and then Existing /= Undeclared_Entity then
         if Entity_Table.Element (Existing).Redefine then
            if Get_Type (Existing) /= Entity_Type then
               Error (Declaration, E_Type_Error, Get_Type (Existing));
               --  FIXME: check arguments
            end if;
            Entity_Table.Replace_Element
              (Existing,
               (Entity_Table.Element (Existing) with delta
                  Redefine => False,
                  Entity_Type => Entity_Type,
                  Declaration => Declaration));
            return Existing;
         else
            Error (Declaration, E_Redefined_Name, Existing);
         end if;
      end if;

      if Kind = Feature_Entity then
         Entity_Table (Context).Offset :=
           Entity_Table (Context).Offset + 1;
         Offset := Entity_Table (Context).Offset;
      end if;

      return Entity : constant Entity_Id := Entity_Table.Last_Index + 1 do
         Entity_Table.Append
           (Entity_Record'
              (Redefine       => False,
               Name           => Name,
               Kind           => Kind,
               Context        => Context,
               Defined_In     => Context,
               Inherited_From => No_Entity,
               Declaration    => Declaration,
               Entity_Type    => Entity_Type,
               Offset         => Offset,
               Children       => <>));
         if Context = No_Entity then
            Top_Level_Entities.Insert
              (Name, Entity);
         else
            Entity_Table (Context).Children.Append (Entity);
         end if;
      end return;
   end New_Entity;

   --------------
   -- New_List --
   --------------

   function New_List return List_Id is
   begin
      return List : constant List_Id := List_Table.Last_Index + 1 do
         List_Table.Append ((others => <>));
      end return;
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind     : Node_Kind;
      From     : Aquarius.Programs.Program_Tree;
      Deferred : Boolean    := False;
      Expanded : Boolean    := False;
      Frozen   : Boolean    := False;
      Defining : Boolean    := False;
      Once     : Boolean    := False;
      Field_1  : Node_Id    := No_Node;
      Field_2  : Node_Id    := No_Node;
      Field_3  : Node_Id    := No_Node;
      Field_4  : Node_Id    := No_Node;
      Field_5  : Node_Id    := No_Node;
      Field_6  : Node_Id    := No_Node;
      List     : List_Id    := No_List;
      Name     : Name_Id    := No_Name;
      Entity   : Entity_Id  := No_Entity)
      return Node_Id
   is
   begin
      return Node : constant Node_Id := Node_Table.Last_Index + 1 do
         Node_Table.Append
           (Node_Record'
              (Kind => Kind, From => From, Deferred => Deferred,
               Expanded => Expanded, Frozen => Frozen,
               Defining => Defining, Single => False, Once => Once,
               Field    =>
                 (1 => Field_1,
                  2 => Field_2,
                  3 => Field_3,
                  4 => Field_4,
                  5 => Field_5,
                  6 => Field_6),
               List       => List,
               Name       => Name,
               Integer_Value => 0,
               Entity     => Entity, Error_Entity => No_Entity,
               Error      => E_No_Error));
      end return;
   end New_Node;

   -------------------------
   -- New_Primitive_Class --
   -------------------------

   function New_Primitive_Class
     (Name        : Name_Id)
      return Entity_Id
   is
   begin
      return Entity : constant Entity_Id := Entity_Table.Last_Index + 1 do
         Entity_Table.Append
           (Entity_Record'
              (Redefine       => False,
               Name           => Name,
               Kind           => Class_Entity,
               Context        => No_Entity,
               Defined_In     => No_Entity,
               Inherited_From => No_Entity,
               Declaration    => No_Node,
               Entity_Type    => Entity,
               Offset         => 0,
               Children       => <>));
         Top_Level_Entities.Insert
           (Name, Entity);
      end return;
   end New_Primitive_Class;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (List    : List_Id;
      Process : not null access
        procedure (Node : Node_Id))
   is
   begin
      if List /= No_List then
         for Node of List_Table.Element (List).List loop
            Process (Node);
         end loop;
      end if;
   end Scan;

   -------------------
   -- Scan_Children --
   -------------------

   procedure Scan_Children
     (Entity  : Entity_Id;
      Process : not null access
        procedure (Child : Entity_Id))
   is
   begin
      for Child of Entity_Table.Element (Entity).Children loop
         Process (Child);
      end loop;
   end Scan_Children;

   -------------------
   -- Scan_Children --
   -------------------

   procedure Scan_Children
     (Entity  : Entity_Id;
      Test    : not null access
        function (Child : Entity_Id)
      return Boolean;
      Process : not null access
        procedure (Child : Entity_Id))
   is
   begin
      for Child of Entity_Table.Element (Entity).Children loop
         if Test (Child) then
            Process (Child);
         end if;
      end loop;
   end Scan_Children;

   -----------------
   -- Scan_Errors --
   -----------------

   procedure Scan_Errors
     (Top     : Node_Id;
      Process : not null access
        procedure (Node : Node_Id;
                   Error : Error_Kind))
   is
      procedure Process_Node (Node : Node_Id);

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Node : Node_Id) is
      begin
         if Has_Error (Node) then
            Process (Node, Get_Error (Node));
         end if;
      end Process_Node;

   begin
      Depth_First_Scan (Top, Process_Node'Access);
   end Scan_Errors;

   ---------------------------
   -- Set_Declaration_Count --
   ---------------------------

   procedure Set_Declaration_Count
     (N     : Node_Id;
      Count : Natural)
   is
   begin
      Node_Table (N).Integer_Value := Count;
   end Set_Declaration_Count;

   ----------------
   -- Set_Entity --
   ----------------

   procedure Set_Entity
     (Node   : Real_Node_Id;
      Entity : Entity_Id)
   is
   begin
      Node_Table (Node).Entity := Entity;
   end Set_Entity;

end Aquarius.Ack;