with Tagatha.Code.Pdp11;
with Tagatha.Code.I686;
with Tagatha.Code.M6502;
with Tagatha.Code.X86_64;

package body Tagatha.Code is

   ------------------
   -- Address_Size --
   ------------------

   function Address_Size (T : Translator) return Tagatha_Size is
   begin
      return Word_Size (Translator'Class (T));
   end Address_Size;

   --------------------
   -- Get_Translator --
   --------------------

   function Get_Translator (Name : String) return Translator'Class is
   begin
      if Name = "pdp-11" then
         return Tagatha.Code.Pdp11.Get_Translator;
      elsif Name = "i686" then
         return Tagatha.Code.I686.Get_Translator;
      elsif Name = "x86_64" then
         return Tagatha.Code.X86_64.Get_Translator;
      elsif Name = "6502" then
         return Tagatha.Code.M6502.Get_Translator;
      else
         raise Constraint_Error with
           "unknown target: " & Name;
      end if;
   end Get_Translator;

   ------------------
   -- Integer_Size --
   ------------------

   function Integer_Size (T : Translator) return Tagatha_Size is
   begin
      return Word_Size (Translator'Class (T));
   end Integer_Size;

end Tagatha.Code;
