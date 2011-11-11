private with Ada.Containers.Indefinite_Vectors;

package Aquarius.Drys.Statements is

   type Statement is abstract new Root_Drys_Type with private;

   function Null_Statement return Statement'Class;

   type Sequence_Of_Statements is
     new Root_Drys_Type with private;

   procedure Append (Sequence : in out Sequence_Of_Statements;
                     Item     : in     Statement'Class);

private

   type Statement is abstract new Root_Drys_Type with null record;

   package Statement_Vectors is
      new Ada.Containers.Indefinite_Vectors (Positive, Statement'Class);

   type Sequence_Of_Statements is
     new Root_Drys_Type with
      record
         Vector : Statement_Vectors.Vector;
      end record;

end Aquarius.Drys.Statements;
