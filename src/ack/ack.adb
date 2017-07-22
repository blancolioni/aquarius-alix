package body Ack is

   ------------
   -- Append --
   ------------

   procedure Append (List : List_Id;
                     Node : Node_Id)
   is
   begin
      List_Table (List).List.Append (Node);
   end Append;

   -----------------
   -- Conforms_To --
   -----------------

   function Conforms_To
     (Class : not null access constant Root_Entity_Type;
      Other : not null access constant Root_Entity_Type'Class)
      return Boolean
   is
   begin
      return Root_Entity_Type'Class (Class.all)'Access = Other;
   end Conforms_To;

   --------------
   -- Contains --
   --------------

   function Contains
     (Table_Entity : Root_Entity_Type;
      Name         : String;
      Recursive    : Boolean := True)
      return Boolean
   is
   begin
      if Table_Entity.Children /= null
        and then Table_Entity.Children.Map.Contains (Name)
      then
         return True;
      elsif Recursive and then Table_Entity.Parent_Environment /= null then
         return Table_Entity.Parent_Environment.Contains (Name, Recursive);
      else
         return False;
      end if;
   end Contains;

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

   ------------
   -- Create --
   ------------

   procedure Create
     (Entity             : in out Root_Entity_Type'Class;
      Name               : Name_Id;
      Node               : Node_Id;
      Table              : Boolean;
      Parent_Environment : access Root_Entity_Type'Class := null;
      Context            : access Root_Entity_Type'Class := null)
   is
   begin
      Entity.Name := +(To_Standard_String (Name));
      Entity.Source_Name := +(To_String (Name));
      Entity.Declaration_Node := Node;
      Entity.Value_Type := null;
      Entity.Parent_Environment := Entity_Type (Parent_Environment);
      Entity.Declaration_Context := Entity_Type (Context);
      if Table then
         Entity.Children := new Entity_Table_Record;
      end if;

   end Create;

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
      Entity : Entity_Type := null)
   is
   begin
      Node_Table (Node).Error := Kind;
      Node_Table (Node).Error_Entity := Entity;
   end Error;

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

   ---------
   -- Get --
   ---------

   function Get
     (Table_Entity : Root_Entity_Type;
      Name         : String)
      return Entity_Type
   is
   begin
      if Table_Entity.Children /= null
        and then Table_Entity.Children.Map.Contains (Name)
      then
         return Table_Entity.Children.Map.Element (Name);
      else
         return Table_Entity.Parent_Environment.Get (Name);
      end if;
   end Get;

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

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Table_Entity : Root_Entity_Type'Class;
      Entity       : not null access Root_Entity_Type'Class)
   is
   begin
      Table_Entity.Children.Map.Insert
        (-(Entity.Name), Entity_Type (Entity));
      Table_Entity.Children.List.Append (Entity_Type (Entity));
   end Insert;

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
      Deferred : Boolean     := False;
      Expanded : Boolean     := False;
      Frozen   : Boolean     := False;
      Defining : Boolean     := False;
      Once     : Boolean     := False;
      Field_1  : Node_Id     := No_Node;
      Field_2  : Node_Id     := No_Node;
      Field_3  : Node_Id     := No_Node;
      Field_4  : Node_Id     := No_Node;
      Field_5  : Node_Id     := No_Node;
      Field_6  : Node_Id     := No_Node;
      List     : List_Id     := No_List;
      Name     : Name_Id     := No_Name;
      Entity   : Entity_Type := null)
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
               Entity        => Entity,
               Error_Entity  => null,
               Error         => E_No_Error));
      end return;
   end New_Node;

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

   ------------------------------
   -- Scan_Entity_Declarations --
   ------------------------------

   procedure Scan_Entity_Declarations
     (Group   : Node_Id;
      Process : not null access
        procedure (Declaration_Node : Node_Id))
   is
      procedure Process_Group (Node : Node_Id);

      -------------------
      -- Process_Group --
      -------------------

      procedure Process_Group (Node : Node_Id) is

         procedure Process_Id (Id_Node : Node_Id);

         ----------------
         -- Process_Id --
         ----------------

         procedure Process_Id (Id_Node : Node_Id) is
         begin
            Process (Id_Node);
         end Process_Id;

      begin
         Scan (Node_Table (Node).List, Process_Id'Access);
      end Process_Group;

   begin
      Scan (Node_Table (Group).List, Process_Group'Access);
   end Scan_Entity_Declarations;

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
     (Node : Real_Node_Id;
      Entity : not null access Root_Entity_Type'Class)
   is
   begin
      Node_Table (Node).Entity := Entity_Type (Entity);
   end Set_Entity;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (List : List_Id)
      return Array_Of_Nodes
   is
      Length : constant Natural :=
                 Natural (List_Table.Element (List).List.Length);
      Count  : Natural := 0;
   begin
      return Result : Array_Of_Nodes (1 .. Length) do
         for Node of List_Table.Element (List).List loop
            Count := Count + 1;
            Result (Count) := Node;
         end loop;
      end return;
   end To_Array;

end Ack;
