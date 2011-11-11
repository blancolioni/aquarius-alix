with Aquarius.Programs;

package Aquarius.Plugins.Klein.Inference is

   function Get_Possible_Types
     (Tree : not null access Aquarius.Programs.Program_Tree_Type'Class)
     return Aquarius.Types.Possible_Types;

   procedure Set_Possible_Types
     (Tree  : not null access Aquarius.Programs.Program_Tree_Type'Class;
      Types : in Aquarius.Types.Possible_Types);

   function Get_Inferred_Types
     (Tree : not null access Aquarius.Programs.Program_Tree_Type'Class)
     return Aquarius.Types.Possible_Types;

   procedure Set_Inferred_Types
     (Tree  : not null access Aquarius.Programs.Program_Tree_Type'Class;
      Types : in Aquarius.Types.Possible_Types);

   procedure Check_Types
     (Item : not null access Aquarius.Programs.Program_Tree_Type'Class);

   function Get_Function_Candidates
     (Table          : Aquarius.Entries.Symbol_Table;
      Name           : String;
      Argument_Count : Natural;
      Result_Type    : Aquarius.Types.Possible_Types)
     return Aquarius.Entries.Array_Of_Entries;

end Aquarius.Plugins.Klein.Inference;
