with Aquarius.Keys.Sequences;

generic
   type Bound_Object is private;
package Aquarius.Keys.Bindings is

   type Binding_Table is tagged private;

   procedure Create (Table : in out Binding_Table);

   procedure Get_Binding
     (Table      : Binding_Table;
      Sequence   : Aquarius.Keys.Sequences.Key_Sequence;
      Incomplete : out Boolean;
      Match      : out Boolean;
      Binding    : out Bound_Object);

   procedure Add_Binding
     (Table : Binding_Table;
      Sequence : Aquarius.Keys.Sequences.Key_Sequence;
      Binding  : Bound_Object);

private

   type Binding_Table_Record;

   type Binding_Table is tagged
      record
         Table : access Binding_Table_Record;
      end record;

end Aquarius.Keys.Bindings;
