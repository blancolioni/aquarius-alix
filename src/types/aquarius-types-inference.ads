with Aquarius.Entries;
with Aquarius.Programs;

package Aquarius.Types.Inference is

   function Get_Possible_Types
     (Tree : not null access Aquarius.Programs.Program_Tree_Type'Class)
     return Possible_Types;

   procedure Set_Possible_Types
     (Tree  : not null access Aquarius.Programs.Program_Tree_Type'Class;
      Types : in Possible_Types);

   function Has_Possible_Types
     (Tree : not null access Aquarius.Programs.Program_Tree_Type'Class)
     return Boolean;

   function Get_Inferred_Types
     (Tree : not null access Aquarius.Programs.Program_Tree_Type'Class)
     return Possible_Types;

   function Has_Inferred_Types
     (Tree : not null access Aquarius.Programs.Program_Tree_Type'Class)
     return Boolean;

   procedure Set_Inferred_Types
     (Tree  : not null access Aquarius.Programs.Program_Tree_Type'Class;
      Types : in Possible_Types);

   procedure Check_Types
     (Item : not null access Aquarius.Programs.Program_Tree_Type'Class);

   function Get_Function_Candidates
     (Table          : in Aquarius.Entries.Symbol_Table;
      Name           : in String;
      Argument_Count : in Natural;
      Result_Type    : in Possible_Types)
     return Aquarius.Entries.Array_Of_Entries;

end Aquarius.Types.Inference;
