package body Aquarius.Semantics is

   ---------------
   -- Add_Phase --
   ---------------

   procedure Add_Phase
     (To   : in out Semantic_Phase_List;
      Name : in String)
   is
   begin
      To.Count := To.Count + 1;
      To.List (To.Count) := To_Unbounded_String (Name);
   end Add_Phase;

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count
     (From_List : Semantic_Phase_List)
     return Semantic_Phase_Count
   is
   begin
      return From_List.Count;
   end Get_Count;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index
     (From_List : Semantic_Phase_List;
      Name      : String)
     return Semantic_Phase_Index
   is
   begin
      for I in 1 .. From_List.Count loop
         if From_List.List (I) = Name then
            return I;
         end if;
      end loop;
      raise Constraint_Error;
   end Get_Index;

   --------------------
   -- Get_Phase_Name --
   --------------------

   function Get_Phase_Name
     (From_List : Semantic_Phase_List;
      Index     : Semantic_Phase_Index)
   return String
   is
   begin
      return To_String (From_List.List (Index));
   end Get_Phase_Name;

   ------------------
   -- Phase_Exists --
   ------------------

   function Phase_Exists
     (In_List : Semantic_Phase_List;
      Name    : String)
     return Boolean
   is
   begin
      for I in 1 .. In_List.Count loop
         if In_List.List (I) = Name then
            return True;
         end if;
      end loop;
      return False;
   end Phase_Exists;

end Aquarius.Semantics;
