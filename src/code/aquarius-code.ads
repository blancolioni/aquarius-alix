private with Ada.Strings.Unbounded;
with Aquarius.Target;

package Aquarius.Code is

   type Frame_Reference is private;

   type Stack_Argument is private;

   function Value_Argument (Value : Aquarius.Target.Aquarius_Value;
                            Size  : Aquarius.Target.Aquarius_Data_Size)
                           return Stack_Argument;

   function Frame_Argument (Frame : Frame_Reference;
                            Size  : Aquarius.Target.Aquarius_Data_Size)
                           return Stack_Argument;

   function Label_Argument (Label : String;
                            Size  : Aquarius.Target.Aquarius_Data_Size)
                       return Stack_Argument;

   type Generator is Interface;

   procedure Push (Gen  : in out Generator;
                   Item : in     Stack_Argument)
      is abstract;

   procedure Pop (Gen   : in out Generator;
                  Item  : in     Stack_Argument)
      is abstract;

private

   type Frame_Reference is new Integer;

   type Stack_Argument_Type is
     (Value_Argument,
      Frame_Argument,
      Label_Argument);

   type Stack_Argument
     (Argument_Type : Stack_Argument_Type := Value_Argument)
      is
      record
         Size : Aquarius.Target.Aquarius_Data_Size;
         case Argument_Type is
            when Value_Argument =>
               Value : Aquarius.Target.Aquarius_Value;
            when Frame_Argument =>
               Frame : Frame_Reference;
            when Label_Argument =>
               Label : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

end Aquarius.Code;

