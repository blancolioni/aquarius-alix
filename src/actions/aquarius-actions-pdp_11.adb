with Aqua.Traps;

package body Aquarius.Actions.Pdp_11 is

   use Ada.Text_IO;

   procedure External_Procedure
     (Processor : in out Pdp_Scanner'Class;
      Name      : String);

   procedure Push_Entry
     (Processor : in out Pdp_Scanner'Class;
      Item      : Table_Entry;
      Arguments : Aquarius.Programs.Array_Of_Program_Trees);

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

      if not Processor.Frame_Table.Contains (Parent) then
         Processor.Frame_Table.Insert (Parent, (Local, 6));
      end if;

      if Processor.Action_Child
        and then not Processor.Frame_Table.Contains (Child)
      then
         Processor.Frame_Table.Insert (Child, (Local, 8));
      end if;
   end Action_Header;

   -----------------------
   -- Ancestor_Selector --
   -----------------------

   overriding procedure Ancestor_Selector
     (Processor  : in out Pdp_Scanner;
      Identifier : String;
      Last       : Boolean)
   is
      pragma Unreferenced (Last);
   begin
      Put_Line (Processor.File,
                "    mov """ & Identifier & """, -(sp)");
      Put_Line (Processor.File,
                "    trap tree_ancestor");
   end Ancestor_Selector;

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

   ------------------------
   -- Component_Selector --
   ------------------------

   overriding procedure Component_Selector
     (Processor  : in out Pdp_Scanner;
      Identifier : String;
      Arguments  : Aquarius.Programs.Array_Of_Program_Trees;
      Last       : Boolean)
   is
      use Scanner;
   begin

      if Last or else Arguments'Length > 0 then
         declare
            Object_Start : constant Symbol_Tables.Cursor :=
                             Processor.Object_Start;
            Object_Partial : constant Aqua.String_Vectors.Vector :=
                               Processor.Object_Partial;
         begin
            for Arg of reverse Arguments loop
               Scanner.Scan_Expression (Processor, Arg);
            end loop;

            Processor.Object_Start := Object_Start;
            Processor.Object_Partial := Object_Partial;

         end;

         if Symbol_Tables.Has_Element (Processor.Object_Start) then
            Push_Entry
              (Processor, Symbol_Tables.Element (Processor.Object_Start),
               Arguments (1 .. 0));
         end if;

         for Component of Processor.Object_Partial loop
            Put_Line (Processor.File,
                      "    mov """ & Component & """, -(sp)");
            Put_Line (Processor.File,
                      "    trap property_get");
         end loop;

         Processor.Object_Partial.Clear;
         Processor.Object_Start := Symbol_Tables.No_Element;

      end if;

      if Last then
         Put_Line (Processor.File,
                   "    mov """ & Identifier & """, -(sp)");
         case Processor.Context is
            when Evaluation | Call =>
               Put_Line (Processor.File,
                         "    trap property_get");
            when Allocation =>
               Put_Line (Processor.File,
                         "    trap allocate");
               Put_Line (Processor.File,
                         "    trap property_set");
            when Assignment_Target =>
               null;
         end case;
      elsif Arguments'Length = 0 then
         Processor.Object_Partial.Append (Identifier);
      else
         Put_Line (Processor.File,
                   "    mov """ & Identifier & """, -(sp)");
         Put_Line (Processor.File,
                   "    trap property_get");
      end if;
   end Component_Selector;

   -----------------------------
   -- Current_Source_Location --
   -----------------------------

   overriding procedure Current_Source_Location
     (Processor : in out Pdp_Scanner;
      Line           : in Natural;
      Column         : in Natural)
   is
   begin
      if Line /= Processor.Last_Line
        and then Column /= Processor.Last_Col
      then
         Put_Line (Processor.File,
                   ".source_position "
                   & Natural'Image (Line)
                   & Natural'Image (Column));
         Processor.Last_Line := Line;
         Processor.Last_Col := Column;
      end if;
   end Current_Source_Location;

   ---------------------
   -- End_Action_Body --
   ---------------------

   overriding procedure End_Action_Body
     (Processor : in out Pdp_Scanner)
   is
   begin
      Put_Line (Processor.File,
                "    mov fp, sp");
      Put_Line (Processor.File,
                "    mov (sp)+, fp");
      Put_Line (Processor.File,
                "    rts pc");
      Processor.Frame_Table.Clear;
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
     (Processor : in out Pdp_Scanner)
   is
   begin
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
      Name      : String)
   is
   begin
      Put_Line
        (Processor.File,
         ".extern " & Name);
      Processor.Global_Table.Insert
        (Name,
         (Subroutine, Ada.Strings.Unbounded.To_Unbounded_String (Name)));
   end External_Procedure;

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
                   "    beq " & Image (L1));
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
      elsif Name = "not" then
         Put_Line (Processor.File, "    tst (sp)+");
         Put_Line (Processor.File, "    beq +1");
         Put_Line (Processor.File, "    clr -(sp)");
         Put_Line (Processor.File, "    beq +2");
         Put_Line (Processor.File, "1:  mov #1, -(sp)");
         Put_Line (Processor.File, "2:");
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
      return Scanner.Top.Source_File_Name & ".m11";
   end Output_Path;

   ----------------
   -- Push_Entry --
   ----------------

   procedure Push_Entry
     (Processor : in out Pdp_Scanner'Class;
      Item      : Table_Entry;
      Arguments : Aquarius.Programs.Array_Of_Program_Trees)
   is
   begin
      for Arg of reverse Arguments loop
         Scanner.Scan_Expression (Processor, Arg);
      end loop;
      case Item.Storage is
         when Local =>
            Put_Line
              (Processor.File,
               "    mov "
               & Integer'Image (Item.Frame_Offset)
               & "(fp), -(sp)");
         when External =>
            Put_Line
              (Processor.File,
               "    mov #"
               & Ada.Strings.Unbounded.To_String (Item.External_Name)
               & ", -(sp)");
         when Subroutine =>
            Put_Line
              (Processor.File,
               "    jsr pc, "
               & Ada.Strings.Unbounded.To_String (Item.External_Name));
            Put_Line
              (Processor.File,
               "    mov r0, -(sp)");
      end case;

   end Push_Entry;

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
      Processor.Frame_Table.Insert ("top", (Local, 4));
      Processor.Frame_Table.Insert ("tree", (Local, 6));
      if Processor.Action_Child then
         Processor.Frame_Table.Insert ("parent", (Local, 6));
         Processor.Frame_Table.Insert ("child", (Local, 8));
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
   begin
      Put_Line (Processor.File,
                "    mov (sp), -(sp)");
      Put_Line (Processor.File,
                "    mov """ & Name & """, -(sp)");
   end Start_Aggregate_Element;

   ----------------------------
   -- Start_Object_Reference --
   ----------------------------

   overriding procedure Start_Object_Reference
     (Processor  : in out Pdp_Scanner;
      Context    : Scanner.Object_Reference_Context;
      Identifier : String;
      Arguments  : Aquarius.Programs.Array_Of_Program_Trees;
      Last       : Boolean)
   is
      use Scanner;
      use Symbol_Tables;
      Position : Cursor := Processor.Frame_Table.Find (Identifier);
   begin
      if not Has_Element (Position) then
         Position := Processor.Global_Table.Find (Identifier);
         if not Has_Element (Position) then
            raise Constraint_Error with
              "undefined: " & Identifier;
         end if;
      end if;

      Processor.Context := Context;

      if Last then
         case Context is
            when Assignment_Target =>
               null;
            when Evaluation =>
               Push_Entry (Processor, Element (Position), Arguments);
            when Allocation =>
               null;
            when Call =>
               for Arg of reverse Arguments loop
                  Scanner.Scan_Expression (Processor, Arg);
               end loop;
               Put_Line (Processor.File, "    jsr pc, " & Identifier);
         end case;
      else
         if Arguments'Length = 0 then
            Processor.Object_Partial.Clear;
            Processor.Object_Start := Position;
         else
            Push_Entry (Processor, Element (Position), Arguments);
         end if;
      end if;
   end Start_Object_Reference;

   -------------------
   -- Start_Process --
   -------------------

   overriding procedure Start_Process
     (Processor : in out Pdp_Scanner;
      Top       : Aquarius.Programs.Program_Tree;
      Group     : Action_Group)
   is
      use Aqua.Traps;
   begin
      Processor.Top := Top;
      Processor.Group := Group;
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

      New_Line (Processor.File);

      External_Procedure (Processor, "set_output");
      External_Procedure (Processor, "put");
      External_Procedure (Processor, "put_line");
      External_Procedure (Processor, "new_line");
      External_Procedure (Processor, "report_state");
      External_Procedure (Processor, "ada_specification_name");

      New_Line (Processor.File);

      Put_Line (Processor.File,
                ".action_group_name " & Action_Group_Name (Group));

   end Start_Process;

   ----------------------
   -- Subtree_Selector --
   ----------------------

   overriding procedure Subtree_Selector
     (Processor  : in out Pdp_Scanner;
      Identifier : String;
      Last       : Boolean)
   is
      pragma Unreferenced (Last);
      No_Arguments : Aquarius.Programs.Array_Of_Program_Trees (1 .. 0);
   begin
      Put_Line (Processor.File,
                "    mov """ & Identifier & """, -(sp)");

      if Symbol_Tables.Has_Element (Processor.Object_Start) then
         Push_Entry
           (Processor, Symbol_Tables.Element (Processor.Object_Start),
            No_Arguments);
      end if;

      for Component of Processor.Object_Partial loop
         Put_Line (Processor.File,
                   "    mov """ & Component & """, -(sp)");
         Put_Line (Processor.File,
                   "    trap property_get");
      end loop;

      Processor.Object_Partial.Clear;
      Processor.Object_Start := Symbol_Tables.No_Element;

      Put_Line (Processor.File,
                "    mov ""tree_child"", -(sp)");
      Put_Line (Processor.File,
                "    trap property_get");

   end Subtree_Selector;

end Aquarius.Actions.Pdp_11;
