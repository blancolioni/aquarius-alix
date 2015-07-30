with Tagatha.Units;

package body Aquarius.Programs.Aqua_Tagatha is

   Current_Unit : Tagatha.Units.Tagatha_Unit;

   ----------------------
   -- Tagatha_Allocate --
   ----------------------

   function Tagatha_Allocate
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      Word_Count : constant Natural :=
                     Natural (Aqua.Get_Integer (Arguments (2)));
   begin
      Current_Unit.Push (Tagatha.Tagatha_Integer (Word_Count));
      Current_Unit.Call ("tagatha__allocate");
      return 0;
   end Tagatha_Allocate;

   ------------------------
   -- Tagatha_Begin_Unit --
   ------------------------

   function Tagatha_Begin_Unit
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Program : constant Program_Tree :=
                  Program_Tree (Context.To_External_Object (Arguments (1)));
      Unit_Name : constant String := Context.To_String (Arguments (2));
   begin
      Current_Unit.Create_Unit
        (Name           => Unit_Name,
         Source_File    =>
           Aquarius.Names.To_String (Program.Source_File_Name));
      return 0;
   end Tagatha_Begin_Unit;

   --------------------------
   -- Tagatha_Code_Segment --
   --------------------------

   function Tagatha_Code_Segment
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      Current_Unit.Segment (Tagatha.Executable);
      return 0;
   end Tagatha_Code_Segment;

   ------------------
   -- Tagatha_Data --
   ------------------

   function Tagatha_Data
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      use Aqua;
      Data : constant Word := Arguments (2);
   begin
      if Is_String_Reference (Data) then
         Current_Unit.Data
           (Context.To_String (Data));
      else
         Current_Unit.Data
           (Tagatha.Tagatha_Integer
              (Get_Integer (Data)));
      end if;
      return 0;
   end Tagatha_Data;

   ----------------------
   -- Tagatha_End_Unit --
   ----------------------

   function Tagatha_End_Unit
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      Current_Unit.Finish_Unit;
      Current_Unit.Write ("pdp-11");
      return 0;
   end Tagatha_End_Unit;

   -------------------
   -- Tagatha_Label --
   -------------------

   function Tagatha_Label
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
   begin
      Current_Unit.Label
        (Context.To_String (Arguments (2)));
      return 0;
   end Tagatha_Label;

   -----------------------
   -- Tagatha_Procedure --
   -----------------------

   function Tagatha_Procedure
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Name : constant String := Context.To_String (Arguments (2));
      Arg_Count : constant Natural :=
                    Natural (Aqua.Get_Integer (Arguments (3)));
      Frame_Count : constant Natural :=
                      Natural (Aqua.Get_Integer (Arguments (4)));
      Result_Count : constant Natural :=
                       Natural (Aqua.Get_Integer (Arguments (5)));
   begin
      Current_Unit.Begin_Routine
        (Name, Arg_Count, Frame_Count, Result_Count, True);
      return 0;
   end Tagatha_Procedure;

   --------------------
   -- Tagatha_Return --
   --------------------

   function Tagatha_Return
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      Current_Unit.End_Routine;
      Current_Unit.Optimise;
      return 0;
   end Tagatha_Return;

   function Tagatha_Text_Segment
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      pragma Unreferenced (Context);
      pragma Unreferenced (Arguments);
   begin
      Current_Unit.Segment (Tagatha.Read_Only);
      return 0;
   end Tagatha_Text_Segment;

end Aquarius.Programs.Aqua_Tagatha;
