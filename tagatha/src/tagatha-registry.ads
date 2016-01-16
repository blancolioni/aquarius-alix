private with Ada.Containers.Vectors;

private with Tagatha.Expressions;
private with Tagatha.Temporaries;

with Tagatha.Labels;
with Tagatha.Operands;
with Tagatha.Transfers;
with Tagatha.Transfers.Transfer_Vectors;

package Tagatha.Registry is

   type Tagatha_Registry is tagged private;

   procedure Start (Register    : in out Tagatha_Registry;
                    Unit_Label  : in     Tagatha.Labels.Tagatha_Label;
                    Size        : in     Natural);

   procedure Record_Push (Register : in out Tagatha_Registry;
                          Size     : in     Tagatha_Size;
                          Operand  : in     Tagatha.Operands.Tagatha_Operand);

   procedure Record_Pop (Register : in out Tagatha_Registry;
                         Size     : in     Tagatha_Size;
                         Operand  : in     Tagatha.Operands.Tagatha_Operand);

   procedure Record_Drop (Register : in out Tagatha_Registry;
                          Size     : in     Tagatha_Size);

   procedure Record_Operation (Register : in out Tagatha_Registry;
                               Operator : in     Tagatha_Operator);

   procedure Record_Native_Operation
     (Register          : in out Tagatha_Registry;
      Name              : String;
      Changed_Registers : String;
      Input_Words       : Natural;
      Output_Words      : Natural);

   procedure Record_Call (Register   : in out Tagatha_Registry;
                          Subroutine : in     Tagatha.Labels.Tagatha_Label);

   procedure Record_Loop (Register   : in out Tagatha_Registry;
                          Limit      : in     Local_Offset;
                          Counter    : in     Local_Offset;
                          End_Label  : in     Tagatha.Labels.Tagatha_Label);

   procedure Record_Jump (Register    : in out Tagatha_Registry;
                          Condition   : in     Tagatha_Condition;
                          Destination : in     Tagatha.Labels.Tagatha_Label);

   procedure Record_Label (Register   : in out Tagatha_Registry;
                           Label      : in     Tagatha.Labels.Tagatha_Label);

   procedure Get_Transfers
     (Register  : in Tagatha_Registry;
      Transfers : in out Tagatha.Transfers.Transfer_Vectors.Vector);

private

   package Expression_Vectors is
      new Ada.Containers.Vectors (Positive,
                                  Tagatha.Expressions.Expression,
                                  Tagatha.Expressions."=");

   package Transfer_Vectors is
     new Ada.Containers.Vectors (Positive,
                                 Tagatha.Transfers.Transfer,
                                 Tagatha.Transfers."=");

   type Tagatha_Registry is tagged
      record
         Frame_Size  : Natural;
         Unit_Label  : Tagatha.Labels.Tagatha_Label;
         Stack       : Expression_Vectors.Vector;
         Transfers   : Tagatha.Transfers.Transfer_Vectors.Vector;
         Temps       : Tagatha.Temporaries.Temporary_Source;
         Last_Label  : Tagatha.Labels.Tagatha_Label;
      end record;

   procedure Append (Register : in out Tagatha_Registry;
                     T        : in     Tagatha.Transfers.Transfer);

end Tagatha.Registry;
