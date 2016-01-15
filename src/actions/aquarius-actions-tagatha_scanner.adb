with Ada.Directories;

with Aquarius.Paths;

with Tagatha.Operands;

package body Aquarius.Actions.Tagatha_Scanner is

   procedure External_Procedure
     (Processor : in out Tagatha_Scanner'Class;
      Name      : String;
      Immediate : Boolean);

   -------------------
   -- Action_Header --
   -------------------

   overriding procedure Action_Header
     (Processor : in out Tagatha_Scanner;
      Position  : Rule_Position;
      Parent    : String;
      Child     : String)
   is
      Routine_Name : constant String :=
                       Action_Group_Name (Processor.Group)
                     & "__"
                       & (if Position = Before then "before" else "after")
                     & "__"
                       & Parent
                       & (if Child = "" then "" else "__" & Child);
   begin
      Processor.Action_Parent := True;
      Processor.Action_Child  := Child /= "";

      if not Processor.Frame_Contains (Parent) then
         Processor.Add_Frame_Entry (Parent, 16);
      end if;

      if Processor.Action_Child
        and then not Processor.Frame_Contains (Child)
      then
         Processor.Add_Frame_Entry (Child, 20);
      end if;

      Processor.Unit.Begin_Routine
        (Name           => Routine_Name,
         Argument_Words => (if Processor.Action_Child then 5 else 4),
         Frame_Words    => 0,
         Result_Words   => 0,
         Global         => False);

      Processor.Unit.Directive
        (".bind_action "
         & Action_Group_Name (Processor.Group)
         & " "
         & (if Position = Before then "before" else "after")
         & " "
         & Parent
         & (if Child = "" then "" else " " & Child),
         Address => 0);

   end Action_Header;

   ------------
   -- Assign --
   ------------

   overriding procedure Assign
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Unit.Pop_Register ("op");
      Processor.Unit.Pop_Register ("pv");
      Processor.Unit.Native_Stack_Operation
        ("set_property "
         & Ada.Strings.Unbounded.To_String (Processor.Property_Name),
         0, 0);
   end Assign;

   ------------------
   -- Clear_Result --
   ------------------

   overriding procedure Clear_Result
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Unit.Drop;
   end Clear_Result;

   ---------------------
   -- End_Action_Body --
   ---------------------

   overriding procedure End_Action_Body
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Delete_Frame;
      Processor.Unit.End_Routine;
   end End_Action_Body;

   -------------------
   -- End_Aggregate --
   -------------------

   overriding procedure End_Aggregate
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Nested_Aggregates := Processor.Nested_Aggregates - 1;
      if Processor.Nested_Aggregates > 0 then
         Processor.Unit.Pop_Register ("agg");
      end if;
   end End_Aggregate;

   ---------------------------
   -- End_Aggregate_Element --
   ---------------------------

   overriding procedure End_Aggregate_Element
     (Processor : in out Tagatha_Scanner;
      Name      : in     String)
   is
   begin
      Processor.Unit.Pop_Register ("pv");
      Processor.Unit.Push_Register ("agg");
      Processor.Unit.Pop_Register ("op");
      Processor.Unit.Native_Stack_Operation
        ("set_property " & Name, 0, 0);
   end End_Aggregate_Element;

   -----------------
   -- End_Process --
   -----------------

   overriding procedure End_Process
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Unit.Finish_Unit;
   end End_Process;

   ------------------------
   -- External_Procedure --
   ------------------------

   procedure External_Procedure
     (Processor : in out Tagatha_Scanner'Class;
      Name      : String;
      Immediate : Boolean)
   is
   begin
      Processor.Add_Global_Entry (Name, Immediate);
   end External_Procedure;

   -----------------------------
   -- Finish_Object_Reference --
   -----------------------------

   overriding procedure Finish_Object_Reference
     (Processor  : in out Tagatha_Scanner)
   is
   begin
      Processor.Nested_Properties :=
        Processor.Nested_Properties - 1;

--        if Processor.Nested_Properties > 0 then
--           Processor.Unit.Pop_Register ("pv");
--           Processor.Unit.Pop_Register ("op");
--        end if;

   end Finish_Object_Reference;

   ------------------
   -- Get_Property --
   ------------------

   overriding procedure Get_Property
     (Processor      : in out Tagatha_Scanner;
      Name           : String;
      Argument_Count : Natural)
   is
   begin
      Processor.Unit.Pop_Register ("op");
      Processor.Unit.Native_Stack_Operation
        ("get_property " & Name & "," & Natural'Image (Argument_Count),
         Argument_Count, 0);
      Processor.Unit.Push_Register ("pv");
   end Get_Property;

   ------------------
   -- If_Statement --
   ------------------

   overriding procedure If_Statement
     (Processor : in out Tagatha_Scanner;
      Expressions : Aquarius.Programs.Array_Of_Program_Trees;
      Statements  : Aquarius.Programs.Array_Of_Program_Trees)
   is
      L1 : Positive := Processor.Unit.Next_Label;
      Exit_Label : constant Positive := Processor.Unit.Next_Label;
   begin
      for I in Expressions'Range loop
         if I > Expressions'First then
            Processor.Unit.Label (L1);
            L1 := Processor.Unit.Next_Label;
         end if;
         Scanner.Scan_Expression (Processor, Expressions (I));
         Processor.Unit.Operate (Tagatha.Op_Test);
         Processor.Unit.Jump (L1, Tagatha.C_Equal);
         Scanner.Scan_Action (Processor, Statements (I));
         Processor.Unit.Jump (Exit_Label);
      end loop;

      Processor.Unit.Label (L1);

      if Statements'Length > Expressions'Length then
         Scanner.Scan_Action (Processor, Statements (Statements'Last));
      end if;

      Processor.Unit.Label (Exit_Label);

   end If_Statement;

   -----------------------------
   -- If_Then_Else_Expression --
   -----------------------------

   overriding procedure If_Then_Else_Expression
     (Processor : in out Tagatha_Scanner;
      Sequence  : Aquarius.Programs.Array_Of_Program_Trees)
   is
      Condition  : Boolean := True;
      L1 : Positive := Processor.Unit.Next_Label;
      Exit_Label : constant Positive := Processor.Unit.Next_Label;
   begin
      for I in Sequence'Range loop
         if I > Sequence'First then
            if Condition then
               Processor.Unit.Label (L1);
               L1 := Processor.Unit.Next_Label;
            end if;
         end if;

         Scanner.Scan_Expression (Processor, Sequence (I));

         if I < Sequence'Last then
            if Condition then
               Processor.Unit.Operate (Tagatha.Op_Test);
               Processor.Unit.Jump (L1, Tagatha.C_Equal);
            else
               Processor.Unit.Jump (Exit_Label);
            end if;

            Condition := not Condition;
         end if;

      end loop;

      Processor.Unit.Label (L1);
      Processor.Unit.Label (Exit_Label);

   end If_Then_Else_Expression;

   ------------------------
   -- Iterator_Statement --
   ------------------------

   overriding procedure Iterator_Statement
     (Processor : in out Tagatha_Scanner;
      Identifier  : String;
      Statements  : Aquarius.Programs.Program_Tree)
   is
      Id : constant String :=
             (if Identifier = "" then "current" else Identifier);
      Loop_Label : constant Positive := Processor.Unit.Next_Label;
      Exit_Label : constant Positive := Processor.Unit.Next_Label;
   begin
      if Processor.Nested_Loops > 0 then
         Processor.Unit.Push_Register ("ctr");
         Processor.Unit.Pop_Register ("t1");
      end if;

      Processor.Unit.Pop_Register ("ctr");

      if Processor.Nested_Loops > 0 then
         Processor.Unit.Push_Register ("t1");
         Processor.Unit.Push_Register ("it");
         Processor.Frame_Offset := Processor.Frame_Offset - 8;
      end if;

      Processor.Nested_Loops := Processor.Nested_Loops + 1;

      Processor.Unit.Native_Stack_Operation ("iterator_start", 0, 0);
      Processor.Frame_Offset := Processor.Frame_Offset - 4;
      Processor.Add_Frame_Entry (Id, Processor.Frame_Offset);
      Processor.Unit.Label (Loop_Label);
      Processor.Unit.Native_Stack_Operation ("iterator_next", 0, 1);
      Processor.Unit.Jump (Exit_Label, Tagatha.C_Equal);

      Scanner.Scan_Action (Processor, Statements);

      Processor.Unit.Jump (Loop_Label);
      Processor.Unit.Label (Exit_Label);

      Processor.Nested_Loops := Processor.Nested_Loops - 1;

      Processor.Delete_Frame_Entry (Id);
      Processor.Frame_Offset := Processor.Frame_Offset + 4;

      if Processor.Nested_Loops > 0 then
         Processor.Unit.Pop_Register ("it");
         Processor.Unit.Pop_Register ("ctr");
         Processor.Frame_Offset := Processor.Frame_Offset + 8;
      end if;

   end Iterator_Statement;

   ------------------
   -- Literal_Null --
   ------------------

   overriding procedure Literal_Null
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Unit.Push (0);
   end Literal_Null;

   --------------------
   -- Literal_Number --
   --------------------

   overriding procedure Literal_Number
     (Processor : in out Tagatha_Scanner;
      Value     : Integer)
   is
   begin
      Processor.Unit.Push (Tagatha.Tagatha_Integer (Value));
   end Literal_Number;

   --------------------
   -- Literal_String --
   --------------------

   overriding procedure Literal_String
     (Processor : in out Tagatha_Scanner;
      Value     : String)
   is
   begin
      Processor.Unit.Push_Text (Value);
   end Literal_String;

   --------------
   -- Operator --
   --------------

   overriding procedure Operator
     (Processor : in out Tagatha_Scanner;
      Name      : String)
   is
   begin
      if Name = "&" then
         Processor.Unit.Native_Stack_Operation ("join", 2, 1);
      elsif Name = "=" then
         Processor.Unit.Operate (Tagatha.Op_Compare);
      elsif Name = "not" then
         Processor.Unit.Operate (Tagatha.Op_Not);
      elsif Name = "*" then
         Processor.Unit.Operate (Tagatha.Op_Mul);
      elsif Name = "+" then
         Processor.Unit.Operate (Tagatha.Op_Add);
      elsif Name = "-" then
         Processor.Unit.Operate (Tagatha.Op_Sub);
      else
         raise Constraint_Error with
           "unimplemented: " & Name;
      end if;
   end Operator;

   -----------------
   -- Output_Path --
   -----------------

   function Output_Path
     (Scanner : Tagatha_Scanner'Class)
      return String
   is
   begin
      return Aquarius.Paths.Scratch_File
        (Name      => Scanner.Source_Base_Name,
         Extension => "m32");
   end Output_Path;

   ------------------------
   -- Pop_External_Entry --
   ------------------------

   overriding procedure Pop_External_Entry
     (Processor  : in out Tagatha_Scanner;
      Name       : String)
   is
   begin
      Processor.Unit.Pop_Label (Name, External => True);
   end Pop_External_Entry;

   ---------------------
   -- Pop_Frame_Entry --
   ---------------------

   overriding procedure Pop_Frame_Entry
     (Processor  : in out Tagatha_Scanner;
      Offset     : Integer)
   is
   begin
      if Offset < 0 then
         Processor.Unit.Pop_Local
           (Tagatha.Local_Offset (abs Offset / 4));
      else
         Processor.Unit.Pop_Argument
           (Tagatha.Argument_Offset (Offset / 4));
      end if;
   end Pop_Frame_Entry;

   -------------------------
   -- Push_External_Entry --
   -------------------------

   overriding procedure Push_External_Entry
     (Processor  : in out Tagatha_Scanner;
      Name       : String;
      Immediate  : Boolean)
   is
   begin
      Processor.Unit.Push_Operand
        (Tagatha.Operands.External_Operand (Name, Immediate),
         Tagatha.Default_Size);
   end Push_External_Entry;

   ----------------------
   -- Push_Frame_Entry --
   ----------------------

   overriding procedure Push_Frame_Entry
     (Processor  : in out Tagatha_Scanner;
      Offset     : Integer)
   is
   begin
      if Offset < 0 then
         Processor.Unit.Push_Local
           (Tagatha.Local_Offset (abs Offset / 4));
      else
         Processor.Unit.Push_Argument
           (Tagatha.Argument_Offset (Offset / 4));
      end if;
   end Push_Frame_Entry;

   -------------------------
   -- Push_String_Literal --
   -------------------------

   overriding procedure Push_String_Literal
     (Processor  : in out Tagatha_Scanner;
      Literal    : String)
   is
   begin
      Processor.Literal_String (Literal);
   end Push_String_Literal;

   -------------------------
   -- Put_Source_Location --
   -------------------------

   overriding procedure Put_Source_Location
     (Processor : in out Tagatha_Scanner;
      Line           : in Natural;
      Column         : in Natural)
   is
   begin
      Processor.Unit.Directive
        (".source_position "
         & Natural'Image (Line)
         & Natural'Image (Column));
   end Put_Source_Location;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (Processor : in out Tagatha_Scanner;
      Name      : String)
   is
   begin
      Processor.Unit.Pop_Register ("op");
      Processor.Unit.Pop_Register ("pv");
      Processor.Unit.Native_Stack_Operation
        ("set_property " & Name, 0, 0);
   end Set_Property;

   ----------------------
   -- Source_Reference --
   ----------------------

   overriding procedure Source_File
     (Processor : in out Tagatha_Scanner;
      Directory_Name : in String;
      File_Name      : in String)
   is
   begin
      if False then
         Processor.Unit.Directive
           (".source_file """
            & Directory_Name
            & "/"
            & File_Name
            & """");
      end if;
   end Source_File;

   -----------------------
   -- Start_Action_Body --
   -----------------------

   overriding procedure Start_Action_Body
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Add_Frame_Entry ("komnenos", 8);
      Processor.Add_Frame_Entry ("top", 12);
      Processor.Add_Frame_Entry ("tree", 16);

      if Processor.Action_Child then
         Processor.Add_Frame_Entry ("parent", 16);
         Processor.Add_Frame_Entry ("child", 20);
      end if;

   end Start_Action_Body;

   ---------------------
   -- Start_Aggregate --
   ---------------------

   overriding procedure Start_Aggregate
     (Processor : in out Tagatha_Scanner)
   is
   begin
      if Processor.Nested_Aggregates > 0 then
         Processor.Unit.Push_Register ("agg");
      end if;
      Processor.Nested_Aggregates := Processor.Nested_Aggregates + 1;
      Processor.Unit.Native_Stack_Operation
        ("allocate", 0, 1);
      Processor.Unit.Pop_Register ("agg");
      Processor.Unit.Push_Register ("agg");
   end Start_Aggregate;

   -----------------------------
   -- Start_Aggregate_Element --
   -----------------------------

   overriding procedure Start_Aggregate_Element
     (Processor : in out Tagatha_Scanner;
      Name      : in     String)
   is
      pragma Unreferenced (Processor);
      pragma Unreferenced (Name);
   begin
      null;
   end Start_Aggregate_Element;

   ----------------------------
   -- Start_Object_Reference --
   ----------------------------

   overriding procedure Start_Object_Reference
     (Processor  : in out Tagatha_Scanner)
   is
   begin
--        if Processor.Nested_Properties > 0 then
--           Processor.Unit.Push_Register ("op");
--           Processor.Unit.Push_Register ("pv");
--        end if;
      Processor.Nested_Properties :=
        Processor.Nested_Properties + 1;
   end Start_Object_Reference;

   -------------------
   -- Start_Process --
   -------------------

   overriding procedure Start_Process
     (Processor  : in out Tagatha_Scanner;
      File_Name  : String;
      Group_Name : String)
   is
   begin
      Processor.Unit.Create_Unit
        (Ada.Directories.Base_Name (File_Name),
         Processor.Source_Path);
      Processor.Unit.Directive (".group " & Group_Name, 0);
      External_Procedure (Processor, "map", Immediate => True);
      External_Procedure (Processor, "array", Immediate => True);
      External_Procedure (Processor, "io", Immediate => True);
      External_Procedure (Processor, "aqua", Immediate => True);
      External_Procedure (Processor, "ada", Immediate => True);
      External_Procedure (Processor, "komnenos", Immediate => True);
      External_Procedure (Processor, "project", Immediate => False);
   end Start_Process;

   -----------
   -- Write --
   -----------

   procedure Write
     (Scanner : in out Tagatha_Scanner'Class)
   is
   begin
      Scanner.Unit.Write
        (Target_Name => "pdp32",
         Directory_Path => Aquarius.Paths.Scratch_Path);
   end Write;

end Aquarius.Actions.Tagatha_Scanner;
