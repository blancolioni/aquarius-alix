with Ada.Directories;

with Aquarius.Paths;

with Tagatha.Operands;

with Tagatha.Units.Listing;

package body Aquarius.Actions.Tagatha_Scanner is

   Write_Listing : constant Boolean := False;

   procedure External_Name
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
         Processor.Add_Frame_Entry (Parent, 3);
      end if;

      if Processor.Action_Child
        and then not Processor.Frame_Contains (Child)
      then
         Processor.Add_Frame_Entry (Child, 4);
      end if;

      if not Processor.Shared_Binding then
         Processor.Shared_Binding := True;
         Processor.Unit.Begin_Routine
           (Name           => Routine_Name,
            Argument_Words => (if Processor.Action_Child then 4 else 3),
            Frame_Words    => 0,
            Result_Words   => 0,
            Global         => False);
      end if;

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

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Processor      : in out Tagatha_Scanner)
   is
   begin
      Processor.Unit.Pop_Register ("op");
      Processor.Unit.Native_Operation
        ("allocate",
         Input_Stack_Words  => 0,
         Output_Stack_Words => 0,
         Changed_Registers  => "op");
      Processor.Unit.Push_Register ("op");
   end Allocate;

   ------------
   -- Assign --
   ------------

   overriding procedure Assign
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Unit.Pop_Register ("op");
      Processor.Unit.Pop_Register ("pv");
      Processor.Unit.Native_Operation
        ("set_property "
         & Ada.Strings.Unbounded.To_String (Processor.Property_Name));
   end Assign;

   -------------------
   -- Call_Function --
   -------------------

   overriding procedure Call_Function
     (Processor      : in out Tagatha_Scanner;
      Name           : String;
      Argument_Count : Natural)
   is
   begin
      Processor.Unit.Call (Name);
      for I in 1 .. Argument_Count loop
         Processor.Unit.Drop;
      end loop;
      Processor.Unit.Push_Register ("r0");
   end Call_Function;

   -------------------
   -- Call_Property --
   -------------------

   overriding procedure Call_Property
     (Processor      : in out Tagatha_Scanner;
      Name           : String;
      Argument_Count : Natural)
   is
      pragma Unreferenced (Argument_Count);
   begin
      Processor.Unit.Pop_Register ("op");
      Processor.Unit.Native_Operation
        ("get_property " & Name,
         Input_Stack_Words  => 0,
         Output_Stack_Words => 0,
         Changed_Registers  => "pv");
      Processor.Unit.Push_Register ("pv");
      Processor.Unit.Indirect_Call;
      Processor.Unit.Push_Result;
   end Call_Property;

   ------------------
   -- Clear_Result --
   ------------------

   overriding procedure Clear_Result
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Unit.Drop;
   end Clear_Result;

   -------------------------------
   -- Declare_External_Function --
   -------------------------------

   overriding procedure Declare_External_Function
     (Processor      : in out Tagatha_Scanner;
      Name           : String)
   is
   begin
      Processor.Unit.Directive (".extern " & Name);
   end Declare_External_Function;

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
      if Name = "" then
         Processor.Unit.Push_Register ("agg");
         Processor.Unit.Pop_Register ("op");
         Processor.Unit.Native_Operation
           ("get_property append, 1", Input_Stack_Words => 1);
      else
         Processor.Unit.Pop_Register ("pv");
         Processor.Unit.Push_Register ("agg");
         Processor.Unit.Pop_Register ("op");
         Processor.Unit.Native_Operation
           ("set_property " & Name);
      end if;
   end End_Aggregate_Element;

   ------------------
   -- End_Function --
   ------------------

   overriding procedure End_Function
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Unit.End_Routine;
      Processor.Delete_Frame;
   end End_Function;

   -----------------
   -- End_Process --
   -----------------

   overriding procedure End_Process
     (Processor : in out Tagatha_Scanner)
   is
   begin
      Processor.Unit.Finish_Unit;
      if Write_Listing then
         Tagatha.Units.Listing.Write_Command_Listing
           (Processor.Unit);
      end if;
   end End_Process;

   -------------------
   -- External_Name --
   -------------------

   procedure External_Name
     (Processor : in out Tagatha_Scanner'Class;
      Name      : String;
      Immediate : Boolean)
   is
   begin
      Processor.Add_Global_Entry (Name, Immediate);
   end External_Name;

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
      Processor.Unit.Native_Operation
        ("get_property " & Name
         & (if Argument_Count > 0
           then "," & Natural'Image (Argument_Count)
           else ""),
         Input_Stack_Words => Argument_Count,
         Output_Stack_Words => 0,
         Changed_Registers  => "pv");
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
         --  Processor.Unit.Operate (Tagatha.Op_Test);
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
      Register   : String := "r8";
      Loop_Label : constant Positive := Processor.Unit.Next_Label;
      Exit_Label : constant Positive := Processor.Unit.Next_Label;
   begin
      Register (2) :=
        Character'Val (Character'Pos ('8') - Processor.Nested_Loops);

      if Processor.Nested_Loops > 0 then
         Processor.Unit.Push_Register ("ctr");
         Processor.Unit.Pop_Register ("r0");
      end if;

      Processor.Unit.Pop_Register ("ctr");

      if Processor.Nested_Loops > 0 then
         Processor.Unit.Push_Register ("r0");
      end if;

      Processor.Nested_Loops := Processor.Nested_Loops + 1;

      Processor.Unit.Native_Operation
        ("iterator_start", Changed_Registers => "ctr");
      Processor.Add_Frame_Entry (Id, Register);
      Processor.Unit.Label (Loop_Label);
      Processor.Unit.Native_Operation
        ("iterator_next " & Register, Changed_Registers => "ctr," & Register);
      Processor.Unit.Push_Register (Register);
      Processor.Unit.Operate (Tagatha.Op_Test);
      Processor.Unit.Jump (Exit_Label, Tagatha.C_Equal);

      Scanner.Scan_Action (Processor, Statements);

      Processor.Unit.Jump (Loop_Label);
      Processor.Unit.Label (Exit_Label);

      Processor.Nested_Loops := Processor.Nested_Loops - 1;

      Processor.Delete_Frame_Entry (Id);

      if Processor.Nested_Loops > 0 then
         Processor.Unit.Pop_Register ("ctr");
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
         Processor.Unit.Operate (Tagatha.Op_Add);
      elsif Name = "=" then
         Processor.Unit.Operate (Tagatha.Op_Compare);
      elsif Name = "/=" then
         Processor.Unit.Operate (Tagatha.Op_Compare);
         Processor.Unit.Operate (Tagatha.Op_Not);
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
           (Tagatha.Local_Offset (abs Offset));
      else
         Processor.Unit.Pop_Argument
           (Tagatha.Argument_Offset (Offset));
      end if;
   end Pop_Frame_Entry;

   ----------------------
   -- Pop_Return_Value --
   ----------------------

   overriding procedure Pop_Return_Value
     (Processor  : in out Tagatha_Scanner)
   is
   begin
      Processor.Unit.Pop_Register ("r0");
   end Pop_Return_Value;

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
           (Tagatha.Local_Offset (abs Offset));
      else
         if Offset < 100 then
            Processor.Unit.Push_Argument
              (Tagatha.Argument_Offset (Offset));
         else
            raise Constraint_Error with "suspiciously large offset";
         end if;
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
      Processor.Unit.Source_Position (Line, Column);
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
      Processor.Unit.Native_Operation
        ("set_property " & Name);
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
      Processor.Add_Frame_Entry ("komnenos", 1);
      Processor.Add_Frame_Entry ("top", 2);
      Processor.Add_Frame_Entry ("tree", 3);

      if Processor.Action_Child then
         Processor.Add_Frame_Entry ("parent", 3);
         Processor.Add_Frame_Entry ("child", 4);
      end if;

      Processor.Shared_Binding := False;

   end Start_Action_Body;

   ---------------------
   -- Start_Aggregate --
   ---------------------

   overriding procedure Start_Aggregate
     (Processor  : in out Tagatha_Scanner;
      Class_Name : String)
   is
   begin
      if Processor.Nested_Aggregates > 0 then
         Processor.Unit.Push_Register ("agg");
      end if;
      Processor.Nested_Aggregates := Processor.Nested_Aggregates + 1;
      Processor.Unit.Push_Operand
        (Tagatha.Operands.External_Operand (Class_Name, True),
         Tagatha.Default_Size);
      Processor.Unit.Pop_Register ("op");
      Processor.Unit.Native_Operation
        ("allocate",
         Input_Stack_Words  => 0,
         Output_Stack_Words => 0,
         Changed_Registers  => "op");
      Processor.Unit.Push_Register ("op");
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

   --------------------
   -- Start_Function --
   --------------------

   overriding procedure Start_Function
     (Processor : in out Tagatha_Scanner;
      Name      : in String;
      Arguments : in Aquarius.Programs.Array_Of_Program_Trees;
      Locals    : in Aquarius.Programs.Array_Of_Program_Trees)
   is
      use Aquarius.Programs;
      use type Tagatha.Local_Offset;
      Arg_Offset   : Integer := 0;
      Frame_Offset : Integer := 0;
   begin

      for Arg of Arguments loop
         Arg_Offset := Arg_Offset + 1;
         Processor.Add_Frame_Entry (Arg.Text, Arg_Offset);
      end loop;

      for Dec of Locals loop
         declare
            Local_List  : constant Program_Tree :=
                            Dec.Program_Child ("local_variable_list");
            Names       : constant Array_Of_Program_Trees :=
                            Local_List.Direct_Children ("identifier");
         begin
            for Id of Names loop
               Frame_Offset := Frame_Offset - 1;
               Processor.Add_Frame_Entry
                 (Id.Standard_Text, Frame_Offset);
            end loop;
         end;
      end loop;

      Processor.Unit.Begin_Routine
        (Name           => Name,
         Argument_Words => Arguments'Length,
         Frame_Words    => 0,
         Result_Words   => 1,
         Global         => True);

      for Dec of Locals loop
         declare
            Local_List  : constant Program_Tree :=
                            Dec.Program_Child ("local_variable_list");
            Names       : constant Array_Of_Program_Trees :=
                            Local_List.Direct_Children ("identifier");
            Initialiser : constant Program_Tree :=
                            Dec.Program_Child ("expression");
            First       : Boolean := True;
         begin
            for Id of Names loop
               if First then
                  if Initialiser = null then
                     Processor.Literal_Null;
                  else
                     Processor.Scan_Expression (Initialiser);
                  end if;
                  if Names'Length > 1 then
                     Processor.Unit.Pop_Register ("r1");
                  end if;
                  First := False;
               end if;
               if Names'Length > 1 then
                  Processor.Unit.Push_Register ("r1");
               end if;
            end loop;
         end;
      end loop;

   end Start_Function;

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

      Processor.Unit.Directive
        ("map ="
         & Natural'Image (16#3000_0001#));

      Processor.Unit.Directive
        ("array ="
         & Natural'Image (16#3000_0002#));

      Processor.Unit.Directive
        ("aqua ="
         & Natural'Image (16#3000_0003#));

      Processor.Unit.Directive
        ("io ="
         & Natural'Image (16#3000_0004#));

      Processor.Unit.Directive
        ("project ="
         & Natural'Image (16#0100#));

      External_Name (Processor, "map", Immediate => True);
      External_Name (Processor, "array", Immediate => True);
      External_Name (Processor, "io", Immediate => True);
      External_Name (Processor, "aqua", Immediate => True);
      External_Name (Processor, "ada", Immediate => True);
      External_Name (Processor, "komnenos", Immediate => True);
      External_Name (Processor, "project", Immediate => False);
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
      if Write_Listing then
         Tagatha.Units.Listing.Write_Transfer_Listing (Scanner.Unit);
      end if;
   end Write;

end Aquarius.Actions.Tagatha_Scanner;
