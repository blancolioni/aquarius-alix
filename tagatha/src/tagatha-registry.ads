private with Ada.Containers.Vectors;

private with Tagatha.Expressions;
with Tagatha.Labels;
with Tagatha.Operands;
with Tagatha.Transfers;

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

   procedure Record_Operation (Register : in out Tagatha_Registry;
                               Operator : in     Tagatha_Operator);

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

   function Get_Transfers (Register : in Tagatha_Registry)
                          return Tagatha.Transfers.Array_Of_Transfers;

private

   package Expression_Vectors is
      new Ada.Containers.Vectors (Positive,
                                  Tagatha.Expressions.Expression,
                                  Tagatha.Expressions."=");

   package Transfer_Vectors is
     new Ada.Containers.Vectors (Positive,
                                 Tagatha.Transfers.Transfer,
                                 Tagatha.Transfers."=");

   package Label_Vectors is
     new Ada.Containers.Vectors (Positive,
                                 Tagatha.Labels.Tagatha_Label,
                                 Tagatha.Labels."=");

   type Tagatha_Registry is tagged
      record
         Frame_Size  : Natural;
         Unit_Label  : Tagatha.Labels.Tagatha_Label;
         Stack       : Expression_Vectors.Vector;
         Transfers   : Transfer_Vectors.Vector;
         Labels      : Label_Vectors.Vector;
         Last_Label  : Tagatha.Labels.Tagatha_Label;
      end record;

   procedure Append (Register : in out Tagatha_Registry;
                     T        : in     Tagatha.Transfers.Transfer);

end Tagatha.Registry;
