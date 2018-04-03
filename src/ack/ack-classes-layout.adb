with Ada.Text_IO;

with WL.String_Maps;
with WL.String_Sets;

with Tagatha.Operands;

package body Ack.Classes.Layout is

   type Base_Table_Record is
      record
         Offset : Natural;
      end record;

   package Base_Table_Maps is
     new WL.String_Maps (Base_Table_Record);

   function Show_Address
     (Offset : Natural)
      return String;

   function Show
     (Item : Layout_Entry)
      return String;

   --------------------------
   -- Create_Object_Layout --
   --------------------------

   function Create_Object_Layout
     (Class : not null access constant Class_Entity_Record'Class)
      return Object_Layout
   is

      Layout : Object_Layout;

      procedure Add (Item : Layout_Entry);

      procedure Add_Properties
        (Base : not null access constant Class_Entity_Record'Class);

      ---------
      -- Add --
      ---------

      procedure Add (Item : Layout_Entry) is
      begin
         Layout.Entries.Append (Item);
      end Add;

      --------------------
      -- Add_Properties --
      --------------------

      procedure Add_Properties
        (Base : not null access constant Class_Entity_Record'Class)
      is
         Offset : Word_Offset := 0;
      begin
         for Feature of Base.Class_Features loop
            if Feature.Is_Property
              and then Constant_Class_Entity (Feature.Active_Class)
              = Constant_Class_Entity (Base)
            then
               Offset := Offset + 1;
               Feature.Set_Property_Offset (Offset);
               Add ((Property_Value, null,
                    Ack.Features.Constant_Feature_Entity (Feature)));
            end if;
         end loop;
      end Add_Properties;

   begin
      Add ((Table_Link, null, Constant_Class_Entity (Class)));
      Add_Properties (Class);
      return Layout;
   end Create_Object_Layout;

   ---------------------------------
   -- Create_Virtual_Table_Layout --
   ---------------------------------

   function Create_Virtual_Table_Layout
     (Class : not null access Class_Entity_Record'Class)
      return Virtual_Table_Layout
   is

      Layout         : Virtual_Table_Layout;
      Bases          : Base_Table_Maps.Map;
      Feature_Offset : Word_Offset;

      procedure Add_Virtual_Table
        (Base : not null access constant Class_Entity_Record'Class);

      -----------------------
      -- Add_Virtual_Table --
      -----------------------

      procedure Add_Virtual_Table
        (Base : not null access constant Class_Entity_Record'Class)
      is
         procedure Add_Link
           (To : not null access constant Class_Entity_Record'Class);

         --------------
         -- Add_Link --
         --------------

         procedure Add_Link
           (To : not null access constant Class_Entity_Record'Class)
         is
         begin
            Layout.Entries.Append
              (Layout_Entry'
                 (Entry_Type  => Label_Link,
                  Label       => Constant_Class_Entity (Base),
                  Link_Name   =>
                    Get_Name_Id
                      (Class.Link_Name
                       & "$" & To.Link_Name & "$vt")));
            Feature_Offset := Feature_Offset + 4;
         end Add_Link;

      begin
         if not Bases.Contains (Base.Link_Name) then
            Bases.Insert
              (Key => Base.Link_Name,
               New_Item =>
                 Base_Table_Record'
                   (Offset => Natural (Layout.Entries.Length)));

            Feature_Offset := 0;

            Base.Scan_Ancestors
              (Proper_Ancestors => True,
               Process          => Add_Link'Access);

            for Feature of Base.Class_Features loop
               if Feature.Definition_Class = Constant_Class_Entity (Base) then
                  --  class Any has no ancestors, so First
                  --  can still be true here

                  Feature.Set_Virtual_Table_Offset (Feature_Offset);
                  Feature_Offset := Feature_Offset + 4;

                  declare
                     Class_Feature : constant Ack.Features.Feature_Entity :=
                                       Class.Feature
                                         (Get_Name_Id
                                            (Feature.Standard_Name));
                  begin
                     if Class_Feature.Deferred then
                        Layout.Entries.Append
                          (Layout_Entry'
                             (Entry_Type  => Internal_Offset,
                              Label       => Constant_Class_Entity (Base),
                              Word_Offset => 0));
                     else
                        Layout.Entries.Append
                          (Layout_Entry'
                             (Entry_Type  => Feature_Address,
                              Label       => Constant_Class_Entity (Base),
                              Feature     =>
                                Ack.Features.Constant_Feature_Entity
                                  (Class_Feature)));
                     end if;
                  end;
               end if;
            end loop;

            Base.Scan_Ancestors
              (Proper_Ancestors => True,
               Process          => Add_Virtual_Table'Access);

         end if;
      end Add_Virtual_Table;

   begin
      Add_Virtual_Table (Class);
      Class.Scan_Ancestors
        (Proper_Ancestors => True,
         Process          => Add_Virtual_Table'Access);
      return Layout;
   end Create_Virtual_Table_Layout;

   -------------------------------
   -- Generate_Object_Allocator --
   -------------------------------

   procedure Generate_Object_Allocator
     (Unit   : in out Tagatha.Units.Tagatha_Unit;
      Class  : not null access constant Class_Entity_Record'Class)
   is
      use type Tagatha.Tagatha_Integer;
      Layout : constant Object_Layout :=
                 Create_Object_Layout (Class);
   begin
      Unit.Begin_Routine
        (Name           => Class.Link_Name & "$create",
         Argument_Words => 0,
         Frame_Words    => 0,
         Result_Words   => 0,
         Global         => True);
      Unit.Push (Tagatha.Tagatha_Integer (Layout.Entries.Length) * 4);
      Unit.Pop_Register ("r0");
      Unit.Call ("__allocate");
      Unit.Push_Register ("r0");
      Unit.Pop_Register ("r1");

      for Item of Layout.Entries loop
         case Item.Entry_Type is
            when Property_Value =>
               Unit.Push (0);
            when Internal_Offset =>
               Unit.Push (Tagatha.Tagatha_Integer (Item.Word_Offset * 4));
            when Table_Link =>
               Unit.Push_Operand
                 (Tagatha.Operands.External_Operand
                    (Item.Table_Class.Link_Name & "$vt",
                     Immediate => True),
                  Size => Tagatha.Default_Size);
            when Feature_Address =>
               Unit.Push_Operand
                 (Tagatha.Operands.External_Operand
                    (Item.Feature.Link_Name,
                     Immediate => True),
                  Size => Tagatha.Default_Size);
            when Label_Link =>
               Unit.Push_Operand
                 (Tagatha.Operands.External_Operand
                    (To_Standard_String (Item.Link_Name),
                     Immediate => True),
                  Size => Tagatha.Default_Size);

         end case;

         Unit.Pop_Operand
           (Tagatha.Operands.Register_Operand
              ("r1", Dereference => True, Postinc => True),
            Tagatha.Default_Size);
      end loop;

      Unit.End_Routine;
   end Generate_Object_Allocator;

   ----------------------------
   -- Generate_Virtual_Table --
   ----------------------------

   procedure Generate_Virtual_Table
     (Unit   : in out Tagatha.Units.Tagatha_Unit;
      Class  : not null access Class_Entity_Record'Class)
   is
      Layout : constant Virtual_Table_Layout :=
                 Create_Virtual_Table_Layout (Class);
      Labels : WL.String_Sets.Set;
      Offset : Word_Offset := 0;
   begin
      Unit.Segment (Tagatha.Read_Only);
      Unit.Label (Class.Link_Name & "$vt");
      for Item of Layout.Entries loop
         if Item.Label /= null
           and then not Labels.Contains (Item.Label.Link_Name)
         then
            Unit.Label (Class.Link_Name & "$" & Item.Label.Link_Name
                        & "$vt");
            Labels.Insert (Item.Label.Link_Name);
            Class.Ancestor_List.Append
              (Ancestor_Class_Record'
                 (Ancestor     => Item.Label,
                  Table_Offset => Offset));
         end if;

         case Item.Entry_Type is
            when Property_Value =>
               Unit.Data (0);
            when Internal_Offset =>
               Unit.Data (Tagatha.Tagatha_Integer (Item.Word_Offset * 4));
            when Table_Link =>
               Unit.Data
                 (Label_Name => Item.Table_Class.Link_Name & "$vt");
            when Feature_Address =>
               Unit.Data
                 (Label_Name => Item.Feature.Link_Name);
            when Label_Link =>
               Unit.Data
                 (Label_Name => To_Standard_String (Item.Link_Name));
         end case;

         Offset := Offset + 1;

      end loop;
      Unit.Segment (Tagatha.Executable);
   end Generate_Virtual_Table;

   ----------
   -- Show --
   ----------

   function Show
     (Item : Layout_Entry)
      return String
   is
   begin
      case Item.Entry_Type is
         when Property_Value =>
            return "prop: " & Item.Property.Declared_Name
              & " : " & Item.Property.Get_Type.Qualified_Name;
         when Internal_Offset =>
            return "offset: " & Integer'Image (Item.Word_Offset * 4);
         when Table_Link =>
            return "vt: " & Item.Table_Class.Qualified_Name;
         when Feature_Address =>
            return "feature: " & Item.Feature.Declared_Name;
         when Label_Link =>
            return "link: " & To_Standard_String (Item.Link_Name);
      end case;
   end Show;

   ------------------
   -- Show_Address --
   ------------------

   function Show_Address
     (Offset : Natural)
      return String
   is
      Hex : constant String := "0123456789ABCDEF";
      It  : Natural := Offset;
   begin
      return Img : String (1 .. 4) do
         for Ch of reverse Img loop
            Ch := Hex (It mod 16 + 1);
            It := It / 16;
         end loop;
      end return;
   end Show_Address;

   -----------
   -- Write --
   -----------

   procedure Write (Layout : Object_Layout) is
      use Ada.Text_IO;
      Offset : Natural := 0;
   begin
      for Item of Layout.Entries loop
         Put (Show_Address (Offset));
         Put (": ");
         Put (Show (Item));
         New_Line;
         Offset := Offset + 4;
      end loop;
   end Write;

   -----------
   -- Write --
   -----------

   overriding procedure Write (Layout : Virtual_Table_Layout) is
   begin
      Write (Object_Layout (Layout));
   end Write;

end Ack.Classes.Layout;
