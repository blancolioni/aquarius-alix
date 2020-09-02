   procedure Analyse_Class_Declaration
     (Node : Node_Id)
   is

      Notes_Node       : constant Node_Id := Notes (Node);
      Inheritance_Node : constant Node_Id := Inheritance (Node);
      Features_Node    : constant Node_Id := Class_Features (Node);
      Class            : constant Ack.Classes.Class_Entity :=
                           Analyse_Class_Header (Node, Class_Header (Node));

      procedure Add_Feature_Work_Items
        (Features_Node : Node_Id;
         Category      : Ack.Semantic.Work.Work_Item_Category);

      ----------------------------
      -- Add_Feature_Work_Items --
      ----------------------------

      procedure Add_Feature_Work_Items
        (Features_Node : Node_Id;
         Category      : Ack.Semantic.Work.Work_Item_Category)
      is
         Clause_List : constant List_Id :=
                         Feature_Clauses (Features_Node);
      begin
         for Clause_Node of List_Table.Element (Clause_List).List loop
            declare
               Feature_List : constant List_Id :=
                                Feature_Declarations (Clause_Node);
            begin
               for Feature_Node of List_Table.Element (Feature_List).List loop
                  Ack.Semantic.Work.Add_Work_Item
                    (Category => Category,
                     Class    => Class,
                     Feature  => Feature_Node);
               end loop;
            end;
         end loop;
      end Add_Feature_Work_Items;

   begin

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Analysing: " & Class.Qualified_Name);
      end if;

      if Notes_Node in Real_Node_Id then
         Analyse_Notes (Class, Notes_Node);
      end if;

      if Features_Node in Real_Node_Id then
         if Trace_Class_Analysis then
            Ada.Text_IO.Put_Line
              ("Analysing feature names: " & Class.Qualified_Name);
         end if;

         Analyse_Features (Class, Features_Node,
                           Analyse_Feature_Name'Access);
      end if;

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Analysing inheritance: " & Class.Qualified_Name);
      end if;

      if Inheritance_Node /= No_Node then
         Analyse_Inheritance (Class, Inheritance_Node);
      elsif Class.Standard_Name /= "any" then
         Class.Inherit
           (Ack.Types.New_Class_Type
              (Node       => Node,
               Class      =>
                 Ack.Semantic.Classes.Load_Class
                   (Get_Program (Node), Ack.Environment.Top_Level,
                    Get_Name_Id ("Any")),
               Detachable => False));
      end if;

      if Features_Node in Real_Node_Id then
         if Trace_Class_Analysis then
            Ada.Text_IO.Put_Line
              ("Adding feature work items: " & Class.Qualified_Name);
         end if;

         Add_Feature_Work_Items
           (Features_Node, Ack.Semantic.Work.Feature_Header);

      end if;

      Ack.Semantic.Work.Add_Work_Item
        (Category  => Ack.Semantic.Work.Class_Binding,
         Class     => Class,
         Feature   => No_Node);

      if Features_Node in Real_Node_Id then

         Add_Feature_Work_Items
           (Features_Node, Ack.Semantic.Work.Feature_Body);

      end if;

      if not Class.Deferred then
         declare
            procedure Check_Effective
              (Feature : not null access constant
                 Ack.Features.Feature_Entity_Record'Class);

            ---------------------
            -- Check_Effective --
            ---------------------

            procedure Check_Effective
              (Feature : not null access constant
                 Ack.Features.Feature_Entity_Record'Class)
            is
            begin
               if Feature.Deferred then
                  Error
                    (Node   => Class.Declaration_Node,
                     Kind   => E_Requires_Definition,
                     Entity => Constant_Entity_Type (Feature));
               end if;
            end Check_Effective;

         begin
            Class.Scan_Features (Check_Effective'Access);
         end;
      end if;

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("  virtual and object tables: " & Class.Qualified_Name);
      end if;

      Ack.Semantic.Work.Add_Work_Item
        (Category  => Ack.Semantic.Work.Class_Layout,
         Class     => Class,
         Feature   => No_Node);

      Ack.Semantic.Work.Add_Work_Item
        (Category  => Ack.Semantic.Work.Error_Report,
         Class     => Class,
         Feature   => No_Node);

      if Trace_Class_Analysis then
         Ada.Text_IO.Put_Line
           ("Finished: " & Class.Qualified_Name);
      end if;

   end Analyse_Class_Declaration;
