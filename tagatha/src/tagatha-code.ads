with Tagatha.Constants;
with Tagatha.Labels;
with Tagatha.Transfers;

package Tagatha.Code is

   type Assembly is limited interface;

   procedure Put_Line (A    : in out Assembly;
                       Line : in     String)
   is abstract;

   type Translator is abstract tagged private;

   procedure File_Preamble (T                : in out Translator;
                            Asm              : in out Assembly'Class;
                            Source_File_Name : in     String)
   is null;

   procedure File_Postamble (T                : in out Translator;
                             Asm              : in out Assembly'Class)
   is null;

   procedure Start (T      : in out Translator;
                    Asm    : in out Assembly'Class;
                    Name   : in     String;
                    Global : in     Boolean)
      is abstract;

   procedure Finish (T : in out Translator;
                     Asm  : in out Assembly'Class)
      is abstract;

   procedure Begin_Frame (T           : in out Translator;
                          Asm  : in out Assembly'Class;
                          Arg_Count   : in     Natural;
                          Local_Count : in     Natural)
   is null;

   procedure End_Frame (T           : in out Translator;
                        Asm         : in out Assembly'Class;
                        Arg_Count   : in     Natural;
                        Local_Count : in     Natural)
   is null;

   procedure Encode (T    : in out Translator;
                     Asm  : in out Assembly'Class;
                     Item : Tagatha.Transfers.Transfer)
      is abstract;

   procedure Label (T     : in out Translator;
                    Asm   : in out Assembly'Class;
                    Label : in     Tagatha.Labels.Tagatha_Label)
     is abstract;

   function General_Registers (T : Translator) return Positive
                               is abstract;

   procedure Data
     (T     : in out Translator;
      Asm   : in out Assembly'Class;
      Value : Tagatha.Constants.Tagatha_Constant)
   is null;

   function Word_Size (T : Translator) return Tagatha_Size is abstract;
   function Address_Size (T : Translator) return Tagatha_Size;
   function Integer_Size (T : Translator) return Tagatha_Size;

   function Get_Translator (Name : String) return Translator'Class;

private

   type Translator is abstract tagged null record;

end Tagatha.Code;
