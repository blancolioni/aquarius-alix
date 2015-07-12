with Ada.Strings.Fixed;

with Aquarius.Paths;

with Aqua.Traps;

package body Aquarius.Actions.Pdp_11 is

   use Ada.Text_IO;

   procedure External_Procedure
     (Processor : in out Pdp_Scanner'Class;
      Name      : String;
      Immediate : Boolean);

   function Next_Label
     (Processor : in out Pdp_Scanner'Class)
      return Label_Type;

   function Image (Label : Label_Type) return String;

   -------------------
   -- Action_Header --
   -------------------

   overriding procedure Action_Header
     (Processor : in out Pdp_Scanner;
      Position  : Rule_Position;
      Parent    : String;
      Child     : String)
   is
   begin
      Put_Line (Processor.File,
                ".bind_action "
                & Action_Group_Name (Processor.Group)
                & " "
                & (if Position = Before then "before" else "after")
                & " "
                & Parent
                & (if Child = "" then "" else " " & Child));

      Processor.Action_Parent := True;
      Processor.Action_Child  := Child /= "";

      if not Processor.Frame_Contains (Parent) then
         Processor.Add_Frame_Entry (Parent, 8);
      end if;

      if Processor.Action_Child
        and then not Processor.Frame_Contains (Child)
      then
         Processor.Add_Frame_Entry (Child, 10);
      end if;

   end Action_Header;

   ------------
   -- Assign --
   ------------

   overriding procedure Assign
     (Processor : in out Pdp_Scanner)
   is
   begin
      Put_Line (Processor.File,
                "    trap property_set");
   end Assign;

   ------------------
   -- Clear_Result --
   ------------------

   overriding procedure Clear_Result
     (Processor : in out Pdp_Scanner)
   is
   begin
      Put_Line (Processor.File, "    tst (sp)+");
   end Clear_Result;

   ---------------------
   -- End_Action_Body --
   ---------------------

   overriding procedure End_Action_Body
     (Processor : in out Pdp_Scanner)
   is
   begin
      Processor.Delete_Frame;
      Put_Line (Processor.File,
                "    mov fp, sp");
      Put_Line (Processor.File,
                "    mov (sp)+, fp");
      Put_Line (Processor.File,
                "    rts pc");
   end End_Action_Body;

   -------------------
   -- End_Aggregate --
   -------------------

   overriding procedure End_Aggregate
     (Processor : in out Pdp_Scanner)
   is
      pragma Unreferenced (Processor);
   begin
      null;
   end End_Aggregate;

   ---------------------------
   -- End_Aggregate_Element --
   ---------------------------

   overriding procedure End_Aggregate_Element
     (Processor : in out Pdp_Scanner;
      Name      : in     String)
   is
   begin
      Put_Line (Processor.File,
                "    mov 2(sp), -(sp)");
      Put_Line (Processor.File,
                "    mov """ & Name & """, -(sp)");
      Put_Line (Processor.File,
                "    trap property_set");
   end End_Aggregate_Element;

   -----------------
   -- End_Process --
   -----------------

   overriding procedure End_Process
     (Processor : in out Pdp_Scanner)
   is
   begin
      Close (Processor.File);
   end End_Process;

   ------------------------
   -- External_Procedure --
   ------------------------

   procedure External_Procedure
     (Processor : in out Pdp_Scanner'Class;
      Name      : String;
      Immediate : Boolean)
   is
   begin
      Put_Line
        (Processor.File,
         ".extern " & Name);
      Processor.Add_Global_Entry (Name, Immediate);
   end External_Procedure;

   ------------------
   -- Get_Property --
   ------------------

   overriding procedure Get_Property
     (Processor : in out Pdp_Scanner;
      Argument_Count : Natural)
   is
   begin
      Put_Line
        (Processor.File,
         "    mov #" & Natural'Image (Argument_Count) & ", -(sp)");
      Put_Line
        (Processor.File,
         "    trap property_get");
   end Get_Property;

   ------------------
   -- If_Statement --
   ------------------

   overriding procedure If_Statement
     (Processor : in out Pdp_Scanner;
      Expressions : Aquarius.Programs.Array_Of_Program_Trees;
      Statements  : Aquarius.Programs.Array_Of_Program_Trees)
   is
      L1 : Label_Type := Next_Label (Processor);
      Exit_Label : constant Label_Type := Next_Label (Processor);
   begin
      for I in Expressions'Range loop
         if I > Expressions'First then
            Put_Line (Processor.File, Image (L1) & ":");
            L1 := Next_Label (Processor);
         end if;
         Scanner.Scan_Expression (Processor, Expressions (I));
         Put_Line (Processor.File,
                   "    tst (sp)+");
         Put_Line (Processor.File,
                   "    bne +1");
         Put_Line (Processor.File,
                   "    jmp " & Image (L1));
         Put_Line (Processor.File, "1:");
         Scanner.Scan_Action (Processor, Statements (I));
         Put_Line (Processor.File,
                   "    jmp " & Image (Exit_Label));
      end loop;

      Put_Line (Processor.File, Image (L1) & ":");

      if Statements'Length > Expressions'Length then
         Scanner.Scan_Action (Processor, Statements (Statements'Last));
      end if;

      Put_Line (Processor.File, Image (Exit_Label) & ":");

   end If_Statement;

   -----------
   -- Image --
   -----------

   function Image (Label : Label_Type) return String is
      N : constant String :=
            Ada.Strings.Fixed.Trim
              (Label_Type'Image (Label), Ada.Strings.Both);
   begin
      return "L" & N;
   end Image;

   ------------------------
   -- Iterator_Statement --
   ------------------------

   overriding procedure Iterator_Statement
     (Processor : in out Pdp_Scanner;
      Identifier  : String;
      Statements  : Aquarius.Programs.Program_Tree)
   is
      Id : constant String :=
             (if Identifier = "" then "current" else Identifier);
      Loop_Label : constant Label_Type := Next_Label (Processor);
      Exit_Label : constant Label_Type := Next_Label (Processor);
   begin
      Processor.Frame_Offset := Processor.Frame_Offset - 4;
      Processor.Add_Frame_Entry (Id, Processor.Frame_Offset);
      Put_Line (Processor.File, "    trap iterator_start");
      Put_Line (Processor.File, Image (Loop_Label) & ":");
      Put_Line (Processor.File, "    trap iterator_next");
      Put_Line (Processor.File, "    bne +1");
      Put_Line (Processor.File, "    jmp " & Image (Exit_Label));
      Put_Line (Processor.File, "1:");
      Scanner.Scan_Action (Processor, Statements);
      Put_Line (Processor.File, "    jmp " & Image (Loop_Label));
      Put_Line (Processor.File, Image (Exit_Label) & ":");
      Processor.Delete_Frame_Entry (Id);
      Processor.Frame_Offset := Processor.Frame_Offset + 4;
   end Iterator_Statement;

   ------------------
   -- Literal_Null --
   ------------------

   overriding procedure Literal_Null
     (Processor : in out Pdp_Scanner)
   is
   begin
      Put_Line
        (Processor.File,
         "    mov #0, -(sp)");
   end Literal_Null;

   --------------------
   -- Literal_Number --
   --------------------

   overriding procedure Literal_Number
     (Processor : in out Pdp_Scanner;
      Value     : Integer)
   is
   begin
      Put_Line (Processor.File,
                "    mov #" & Integer'Image (Value)
                & ", -(sp)");
   end Literal_Number;

   --------------------
   -- Literal_String --
   --------------------

   overriding procedure Literal_String
     (Processor : in out Pdp_Scanner;
      Value     : String)
   is
   begin
      Put_Line
        (Processor.File,
         "    mov """ & Value & """, -(sp)");
   end Literal_String;

   ----------------
   -- Next_Label --
   ----------------

   function Next_Label
     (Processor : in out Pdp_Scanner'Class)
      return Label_Type
   is
   begin
      Processor.Next_Label := Processor.Next_Label + 1;
      return Processor.Next_Label;
   end Next_Label;

   --------------
   -- Operator --
   --------------

   overriding procedure Operator
     (Processor : in out Pdp_Scanner;
      Name      : String)
   is
   begin
      if Name = "&" then
         Put_Line (Processor.File, "    trap join_strings");
      elsif Name = "=" then
         Put_Line (Processor.File, "    clr r0");
         Put_Line (Processor.File, "    mov (sp)+, r1");
         Put_Line (Processor.File, "    cmp r1, (sp)+");
         Put_Line (Processor.File, "    bne +1");
         Put_Line (Processor.File, "    inc r0");
         Put_Line (Processor.File, "1:  mov r0, -(sp)");
      elsif Name = "not" then
         Put_Line (Processor.File, "    clr r0");
         Put_Line (Processor.File, "    tst (sp)+");
         Put_Line (Processor.File, "    bne +1");
         Put_Line (Processor.File, "    inc r0");
         Put_Line (Processor.File, "1:  mov r0, -(sp)");
      else
         raise Constraint_Error with
           "unimplemented: " & Name;
      end if;
   end Operator;

   -----------------
   -- Output_Path --
   -----------------

   function Output_Path
     (Scanner : Pdp_Scanner'Class)
      return String
   is
   begin
      return Aquarius.Paths.Scratch_File
        (Name      => Scanner.Source_Base_Name,
         Extension => "m11");
   end Output_Path;

   ------------------------
   -- Pop_External_Entry --
   ------------------------

   overriding procedure Pop_External_Entry
     (Processor  : in out Pdp_Scanner;
      Name       : String)
   is
   begin
      Put_Line
        (Processor.File,
         "    mov (sp)+, "
           & Name);
   end Pop_External_Entry;

   overriding procedure Pop_Frame_Entry
     (Processor  : in out Pdp_Scanner;
      Offset     : Integer)
   is
   begin
      Put_Line
        (Processor.File,
         "    mov (sp)+, "
         & Integer'Image (Offset)
         & "(fp)");
   end Pop_Frame_Entry;

   -------------------------
   -- Push_External_Entry --
   -------------------------

   overriding procedure Push_External_Entry
     (Processor  : in out Pdp_Scanner;
      Name       : String;
      Immediate  : Boolean)
   is
   begin
      Put_Line
        (Processor.File,
         "    mov "
         & (if Immediate then "#" else "")
         & Name & ", -(sp)");
   end Push_External_Entry;

   ----------------------
   -- Push_Frame_Entry --
   ----------------------

   overriding procedure Push_Frame_Entry
     (Processor  : in out Pdp_Scanner;
      Offset     : Integer)
   is
   begin
      Put_Line
        (Processor.File,
         "    mov "
         & Integer'Image (Offset)
         & "(fp), -(sp)");
   end Push_Frame_Entry;

   -------------------------
   -- Push_String_Literal --
   -------------------------

   overriding procedure Push_String_Literal
     (Processor  : in out Pdp_Scanner;
      Literal    : String)
   is
   begin
      Put_Line
        (Processor.File,
         "    mov """ & Literal & """, -(sp)");
   end Push_String_Literal;

   -------------------------
   -- Put_Source_Location --
   -------------------------

   overriding procedure Put_Source_Location
     (Processor : in out Pdp_Scanner;
      Line           : in Natural;
      Column         : in Natural)
   is
   begin
      Put_Line (Processor.File,
                ".source_position "
                & Natural'Image (Line)
                & Natural'Image (Column));
   end Put_Source_Location;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (Processor : in out Pdp_Scanner)
   is
   begin
      Put_Line
        (Processor.File,
         "    trap property_set");
   end Set_Property;

   ----------------------
   -- Source_Reference --
   ----------------------

   overriding procedure Source_File
     (Processor : in out Pdp_Scanner;
      Directory_Name : in String;
      File_Name      : in String)
   is
   begin
      Put_Line (Processor.File,
                ".source_file """
                & Directory_Name
                & "/"
                & File_Name
                & """");
   end Source_File;

   -----------------------
   -- Start_Action_Body --
   -----------------------

   overriding procedure Start_Action_Body
     (Processor : in out Pdp_Scanner)
   is
   begin
      Processor.Add_Frame_Entry ("komnenos", 4);
      Processor.Add_Frame_Entry ("top", 6);
      Processor.Add_Frame_Entry ("tree", 8);

      if Processor.Action_Child then
         Processor.Add_Frame_Entry ("parent", 8);
         Processor.Add_Frame_Entry ("child", 10);
      end if;

      Put_Line (Processor.File,
                "    mov fp, -(sp)");
      Put_Line (Processor.File,
                "    mov sp, fp");
   end Start_Action_Body;

   ---------------------
   -- Start_Aggregate --
   ---------------------

   overriding procedure Start_Aggregate
     (Processor : in out Pdp_Scanner)
   is
   begin
      Put_Line (Processor.File,
                "    trap allocate");
   end Start_Aggregate;

   -----------------------------
   -- Start_Aggregate_Element --
   -----------------------------

   overriding procedure Start_Aggregate_Element
     (Processor : in out Pdp_Scanner;
      Name      : in     String)
   is
      pragma Unreferenced (Processor);
      pragma Unreferenced (Name);
   begin
      null;
   end Start_Aggregate_Element;

   -------------------
   -- Start_Process --
   -------------------

   overriding procedure Start_Process
     (Processor  : in out Pdp_Scanner;
      Group_Name : String)
   is
      use Aqua.Traps;
   begin
      Create (Processor.File, Out_File, Processor.Output_Path);

      Put_Line (Processor.File,
                "property_get_address ="
                & Natural'Image (Property_Get_Address));
      Put_Line (Processor.File,
                "Property_Get ="
                & Natural'Image (Property_Get));
      Put_Line (Processor.File,
                "Property_Set ="
                & Natural'Image (Property_Set));
      Put_Line (Processor.File,
                "Allocate ="
                & Natural'Image (Allocate));
      Put_Line (Processor.File,
                "Join_Strings ="
                & Natural'Image (Join_Strings));
      Put_Line (Processor.File,
                "cpu_report_state ="
                & Natural'Image (Report_State));
      Put_Line (Processor.File,
                "iterator_start ="
                & Natural'Image (Iterator_Start));
      Put_Line (Processor.File,
                "iterator_next ="
                & Natural'Image (Iterator_Next));

      New_Line (Processor.File);

      Put_Line (Processor.File,
                "map ="
                & Natural'Image (16#4001#));

      Put_Line (Processor.File,
                "array ="
                & Natural'Image (16#4002#));

      Put_Line (Processor.File,
                "aqua ="
                & Natural'Image (16#4003#));

      Put_Line (Processor.File,
                "io ="
                & Natural'Image (16#4004#));

      Put_Line (Processor.File,
                "project ="
                & Natural'Image (16#0100#));

      External_Procedure (Processor, "map", Immediate => True);
      External_Procedure (Processor, "array", Immediate => True);
      External_Procedure (Processor, "io", Immediate => True);
      External_Procedure (Processor, "aqua", Immediate => True);
      External_Procedure (Processor, "ada", Immediate => True);
      External_Procedure (Processor, "komnenos", Immediate => True);
      External_Procedure (Processor, "project", Immediate => False);

      New_Line (Processor.File);

      Put_Line (Processor.File,
                ".action_group_name " & Group_Name);

   end Start_Process;

end Aquarius.Actions.Pdp_11;
