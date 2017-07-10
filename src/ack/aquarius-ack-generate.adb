with Tagatha.Units;

with Aquarius.Config_Paths;

with Aquarius.Ack.Files;

package body Aquarius.Ack.Generate is

   procedure Generate_Feature
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Feature : Entity_Id);

   --------------------------------
   -- Generate_Class_Declaration --
   --------------------------------

   procedure Generate_Class_Declaration
     (Node : Node_Id)
   is
      Unit : Tagatha.Units.Tagatha_Unit;
      Entity : constant Entity_Id := Get_Entity (Node);
   begin
      Unit.Create_Unit
        (Aquarius.Ack.Files.Base_File_Name (Get_Entity (Node)),
         Get_Program (Node).Source_File_Name);

      Unit.Begin_Routine
        (Get_Link_Name (Entity) & "__default_create",
         Argument_Words => 0,
         Frame_Words    => 0,
         Result_Words   => 0,
         Global         => True);

      Unit.Segment (Tagatha.Read_Only);
      Unit.Label (Get_Link_Name (Entity) & "__vt",
                  Export => True);

      declare

         function Virtual_Table_Feature
           (Feature : Entity_Id)
            return Boolean
         is (Get_Kind (Feature) = Feature_Entity);

         function Class_Defined_Feature
           (Feature : Entity_Id)
            return Boolean
         is (Get_Kind (Feature) = Feature_Entity
             and then Get_Defined_In (Feature) = Entity);

         procedure Feature_Address (Feature : Entity_Id);
         procedure Generate_Feature (Feature : Entity_Id);

         ---------------------
         -- Feature_Address --
         ---------------------

         procedure Feature_Address (Feature : Entity_Id) is
         begin
            Unit.Data (Get_Link_Name (Feature));
         end Feature_Address;

         ----------------------
         -- Generate_Feature --
         ----------------------

         procedure Generate_Feature (Feature : Entity_Id) is
         begin
            Generate_Feature (Unit, Feature);
         end Generate_Feature;

      begin
         Scan_Children (Entity, Virtual_Table_Feature'Access,
                        Feature_Address'Access);
         Scan_Children (Entity, Class_Defined_Feature'Access,
                        Generate_Feature'Access);
      end;

      Unit.Finish_Unit;
      Unit.Write
        (Target_Name    => "pdp32",
         Directory_Path => Aquarius.Config_Paths.Config_File ("scratch"));
   end Generate_Class_Declaration;

   ----------------------
   -- Generate_Feature --
   ----------------------

   procedure Generate_Feature
     (Unit    : in out Tagatha.Units.Tagatha_Unit;
      Feature : Entity_Id)
   is
      Feature_Node : constant Node_Id := Get_Declaration (Feature);
      Dec_Body     : constant Node_Id := Declaration_Body (Feature_Node);
      Arg_Node     : constant Node_Id := Formal_Arguments (Dec_Body);
--        Type_Node    : constant Node_Id := Value_Type (Dec_Body);
--        Value_Node   : constant Node_Id := Value (Dec_Body);

      Arg_Count    : constant Natural :=
                       (if Arg_Node = No_Node then 0
                        else Declaration_Count
                          (Entity_Declaration_Group_List (Arg_Node)));
   begin
      Unit.Begin_Routine
        (Name           => Get_Link_Name (Feature),
         Argument_Words => Arg_Count,
         Frame_Words    => 0,
         Result_Words   => 1,
         Global         => True);
      Unit.End_Routine;
   end Generate_Feature;

end Aquarius.Ack.Generate;
