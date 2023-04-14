with Ada.Calendar;
with Ada.Sequential_IO;

package body As.Objects is

   L_Header   : constant := 1;
   L_Footer   : constant := 2;
   L_Quote    : constant := 3;
   L_Location : constant := 4;

   function Get_Context
     (This : Instance'Class;
      From : As.Files.File_Context)
      return Context_Record
     with Pre => This.Has_Context (From);

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Instance'Class; Value : Word_8) is
   begin
      if not This.Raw and then Value = Loader_Value
        and then This.Data.Last_Index mod 4 = 0
      then
         This.Data.Append (Loader_Value);
         This.Data.Append (L_Quote);
         This.Data.Append (0);
         This.Data.Append (1);
      end if;

      This.Data.Append (Value);

      if not This.Raw then
         This.Loc := This.Loc + 1;
         if Context_Lists.Has_Element (This.Current) then
            This.Contexts (This.Current).Last := This.Data.Last_Index;
         end if;
      end if;

   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Instance'Class; Value : Word_16) is
   begin
      This.Append (Word_8 (Value / 256));
      This.Append (Word_8 (Value mod 256));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Instance'Class; Value : Word_32) is
   begin
      This.Append (Word_16 (Value / 65536));
      This.Append (Word_16 (Value mod 65536));
   end Append;

   ------------
   -- Create --
   ------------

   function Create return Reference is
      use Ada.Calendar;
      Now : constant Time := Clock;
      Y : constant Year_Number := Year (Now);
   begin
      return This : constant Reference := new Instance do
         This.Loader (L_Header, 1, 2);
         This.Raw := True;
         This.Append (Word_8 (Y / 256 mod 256));
         This.Append (Word_8 (Y mod 256));
         This.Append (Word_8 (Month (Now)));
         This.Append (Word_8 (Day (Now)));
         This.Append (Word_32 (Seconds (Now)));
         This.Raw := False;
      end return;
   end Create;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Word_32
   is
   begin
      return Get_Context (This, Context).Address;
   end Get_Address;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
     (This : Instance'Class;
      From : As.Files.File_Context)
      return Context_Record
   is
      use type As.Files.Reference;
   begin
      for Rec of This.Contexts loop
         if Rec.Context.File = From.File
           and then Rec.Context.Line = From.Line
         then
            return Rec;
         end if;
      end loop;
      raise Constraint_Error with
        "no such context: " & As.Files.To_String (From);
   end Get_Context;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Context_Data
   is
      Rec : constant Context_Record := This.Get_Context (Context);
      First : constant Positive := Rec.First;
      Last  : constant Natural := Rec.Last;
   begin
      return Data : Context_Data (1 .. Last - First + 1) do
         for I in First .. Last loop
            Data (I - First + 1) := This.Data (I);
         end loop;
      end return;
   end Get_Data;

   ------------------
   -- Global_Value --
   ------------------

   procedure Global_Value
     (This  : in out Instance'Class;
      Value : Word_32)
   is
   begin
      This.Raw := True;
      This.Append (Value);
      This.Raw := False;
   end Global_Value;

   -------------
   -- Globals --
   -------------

   procedure Globals
     (This : in out Instance'Class;
      Last : Register_Index)
   is
   begin
      This.Loader (L_Footer, 0, Word_8 (Last));
   end Globals;

   ------------
   -- Loader --
   ------------

   procedure Loader
     (This    : in out Instance'Class;
      X, Y, Z : Word_8)
   is
   begin
      while This.Data.Last_Index mod 4 /= 0 loop
         This.Append (Word_8'(0));
      end loop;
      This.Raw := True;
      This.Append (Loader_Value);
      This.Append (X);
      This.Append (Y);
      This.Append (Z);
      This.Raw := False;
   end Loader;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (This    : in out Instance'Class;
      Context : As.Files.File_Context)
   is
   begin
      This.Contexts.Append
        (Context_Record'
           (Context => Context,
            Address => This.Location,
            First   => This.Data.Last_Index + 1,
            Last    => This.Data.Last_Index));
      This.Current := This.Contexts.Last;
   end Set_Context;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (This  : in out Instance'Class;
      Value : Word_32)
   is
   begin
      This.Loader (L_Location, 0, 0);
      This.Raw := True;
      This.Append (Value);
      This.Raw := False;
      This.Loc := Value;
   end Set_Location;

   -----------
   -- Write --
   -----------

   procedure Write (This : in out Instance'Class; Path : String) is
      package Word_8_IO is new Ada.Sequential_IO (Word_8);
      use Word_8_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      for W of This.Data loop
         Write (File, W);
      end loop;
      Close (File);
   end Write;

end As.Objects;
