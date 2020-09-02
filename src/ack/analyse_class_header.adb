   --------------------------
   -- Analyse_Class_Header --
   --------------------------

   function Analyse_Class_Header
     (Class  : Node_Id;
      Header : Node_Id)
      return Ack.Classes.Class_Entity
   is
      Formal_Generics_Node : constant Node_Id := Formal_Generics (Header);
      Creators_Node        : constant Node_Id := Class_Creators (Class);
      Result               : Ack.Classes.Class_Entity;
   begin
      Analyse_Class_Name (null, Class_Name (Header),
                          Defining_Name => True);
      Result := Ack.Classes.Get_Class_Entity (Class_Name (Header));

      Result.Set_Top_Class_Node (Class);

      if Node_Table.Element (Header).Deferred then
         Result.Set_Deferred;
      elsif Node_Table.Element (Header).Expanded then
         Result.Set_Expanded;
      elsif Node_Table.Element (Header).Frozen then
         Result.Set_Frozen;
      end if;

      Set_Entity (Class, Result);
      if Formal_Generics_Node /= No_Node then
         Analyse_Formal_Generics (Result, Formal_Generics_Node);
      end if;

      if Creators_Node in Real_Node_Id then
         declare
            procedure Add_Creator_Clause
              (Node : Node_Id);

            ------------------------
            -- Add_Creator_Clause --
            ------------------------

            procedure Add_Creator_Clause
              (Node : Node_Id)
            is
               procedure Add_Creator_Name (Creator_Node : Node_Id);

               ----------------------
               -- Add_Creator_Name --
               ----------------------

               procedure Add_Creator_Name (Creator_Node : Node_Id) is
               begin
                  Result.Add_Creator
                    (Get_Name (Creator_Node));
               end Add_Creator_Name;

            begin
               Scan (Creator_List (Node), Add_Creator_Name'Access);
            end Add_Creator_Clause;

         begin
            Scan (Creator_Clauses (Creators_Node),
                  Add_Creator_Clause'Access);
         end;
      end if;

      return Result;
   end Analyse_Class_Header;
