with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Ack.Classes;
with Ack.Compile;
with Ack.Errors;
with Ack.Features;
with Ack.Types;

package body Ack.Bindings is

   Report_Calls : constant Boolean := False;

   ----------------------
   -- Load_Ack_Binding --
   ----------------------

   function Load_Ack_Binding
     (Action_File_Path : String;
      Base_Aqua_Path   : String;
      Image            : Aqua.Images.Image_Type;
      Grammar          : Aquarius.Grammars.Aquarius_Grammar;
      Group            : Aquarius.Actions.Action_Group;
      Trigger          : Aquarius.Actions.Action_Execution_Trigger)
      return Boolean
   is
      pragma Unreferenced (Trigger);

      Action_File : Ada.Text_IO.File_Type;

      References  : List_Of_Entities.List;

      function Is_Group_Reference
        (Class    : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Property : not null access constant Root_Entity_Type'Class)
         return Boolean;

      procedure Load_Class
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      procedure Add_Feature_Binding
        (Class        : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Feature      : not null access constant
           Ack.Root_Entity_Type'Class);

      procedure Check_Allocated
        (Class     : not null access constant
           Ack.Root_Entity_Type'Class;
         Tree_Name : String);

      -------------------------
      -- Add_Feature_Binding --
      -------------------------

      procedure Add_Feature_Binding
        (Class        : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Feature      : not null access constant
           Ack.Root_Entity_Type'Class)
      is
         Feature_Name : constant String := Feature.Standard_Name;
         Index         : constant Natural :=
                           Ada.Strings.Fixed.Index (Feature_Name, "_");
         Position_Name : constant String :=
                           (if Index > 0
                            then Feature_Name (Feature_Name'First .. Index - 1)
                            else "");
         Child_Tree    : constant String :=
                           Feature_Name (Index + 1 .. Feature_Name'Last);
         Parent_Tree   : constant String := Class.Standard_Name;
         Group_Property : constant String := Class.Link_Name;
      begin
         if Ack.Features.Is_Feature (Feature)
           and then Ack.Features.Feature_Entity_Record (Feature.all)
           .Is_Property
           and then Is_Group_Reference
             (Class,
              Ack.Features.Feature_Entity_Record'Class (Feature.all)'Access)
         then
            References.Append (Feature);
         end if;

         if Index > 0
           and then (Position_Name = "before"
                     or else Position_Name = "after")
         then
            if Child_Tree = "node" then
               Ada.Text_IO.Put_Line
                 (Action_File,
                  Position_Name & " " & Parent_Tree & " do");
            else
               Ada.Text_IO.Put_Line
                 (Action_File,
                  Position_Name & " " & Parent_Tree
                  & "/" & Child_Tree & " do");
            end if;

            Check_Allocated (Class, "tree");

            for Reference of References loop
               Ada.Text_IO.Put_Line
                 (Action_File,
                  "   if tree." & Reference.Get_Type.Link_Name & " then");
               Ada.Text_IO.Put_Line
                 (Action_File,
                  "      tree." & Group_Property
                  & "." & Class.Link_Name
                  & "." & Reference.Standard_Name & " :=");
               Ada.Text_IO.Put_Line
                 (Action_File,
                  "        tree." & Reference.Get_Type.Link_Name);
               Ada.Text_IO.Put_Line
                 (Action_File,
                  "   end if");
            end loop;

            if False then
               Ada.Text_IO.Put_Line
                 (Action_File,
                  "   IO.put_line (tree." & Group_Property & ".Image)");
            end if;

            if Child_Tree = "node" then
               Ada.Text_IO.Put_Line
                 (Action_File,
                  "   call "
                  & "tree." & Group_Property & "."
                  & Class.Link_Name
                  & "."
                  & Position_Name & "_node"
                  & "(tree." & Group_Property & ")");
            else
               declare
                  Child_Argument : constant Entity_Type :=
                                     Feature.Argument (1);
                  Child_Type     : constant Entity_Type :=
                                     Child_Argument.Get_Type;
               begin
                  if Child_Type.Standard_Name = "string" then
                     if False then
                        Ada.Text_IO.Put_Line
                          (Action_File,
                           "   IO.put_line (""" & Parent_Tree
                           & "/" & Child_Tree
                           & " -> "" & child.text.to_lower)");
                     end if;

                     Ada.Text_IO.Put_Line
                       (Action_File,
                        "   call "
                        & "tree." & Group_Property & "."
                        & Class.Link_Name
                        & "."
                        & Position_Name & "_" & Child_Tree
                        & "(tree." & Group_Property & ","
                        & "child.text);");
                  elsif Is_Group_Reference
                    (Class, Child_Argument)
                  then
                     Check_Allocated
                       (Child_Type, "child");

                     if Report_Calls then
                        Ada.Text_IO.Put_Line
                          (Action_File,
                           "   IO.put_line (""Before call: "
                           & Position_Name & " "
                           & Parent_Tree
                           & "/" & Child_Tree
                           & " -> "" & child."
                           & Child_Type.Link_Name
                           & ".Image)");
                     end if;

                     Ada.Text_IO.Put_Line
                       (Action_File,
                        "   call "
                        & "tree." & Group_Property & "."
                        & Class.Link_Name
                        & "."
                        & Position_Name & "_" & Child_Tree
                        & "(tree." & Group_Property & ","
                        & "child." & Child_Type.Link_Name & ");");

                     if Report_Calls then
                        Ada.Text_IO.Put_Line
                          (Action_File,
                           "   IO.put_line (""After Call: "
                           & Position_Name & " "
                           & Parent_Tree
                           & "/" & Child_Tree
                           & " -> "" & child."
                           & Child_Type.Link_Name
                           & ".Image)");
                     end if;

                  else
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "warning: "
                        & Feature.Full_Name & ": cannot bind argument "
                        & Child_Argument.Declared_Name
                        & " of type "
                        & Child_Type.Full_Name);
                  end if;
               end;
            end if;

            Ada.Text_IO.Put_Line
              (Action_File,
               "end;");
            Ada.Text_IO.New_Line (Action_File);

         end if;
      end Add_Feature_Binding;

      ---------------------
      -- Check_Allocated --
      ---------------------

      procedure Check_Allocated
        (Class : not null access constant
             Ack.Root_Entity_Type'Class;
         Tree_Name : String)
      is
         Group_Property : constant String := Class.Link_Name;
      begin
         Ada.Text_IO.Put_Line
           (Action_File,
            "   if not " & Tree_Name & "." & Group_Property & " then");
         Ada.Text_IO.Put_Line
           (Action_File,
            "      " & Tree_Name & "." & Group_Property & " :=");
         Ada.Text_IO.Put_Line
           (Action_File,
            "        "
            & Class.Link_Name
            & "$allocate");
         if Report_Calls then
            Ada.Text_IO.Put_Line
              (Action_File,
               "      IO.Put_Line (""Allocated " & Tree_Name & ": "" & "
               & Tree_Name & "." & Group_Property & ".Image)");
         end if;

         Ada.Text_IO.Put_Line
           (Action_File,
            "   end if");
      end Check_Allocated;

      ------------------------
      -- Is_Group_Reference --
      ------------------------

      function Is_Group_Reference
        (Class    : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Property : not null access constant
           Root_Entity_Type'Class)
         return Boolean
      is
         use Ada.Strings.Fixed;
         Property_Type : constant Entity_Type := Property.Get_Type;
         Property_Link_Name : constant String := Property_Type.Link_Name;
         Class_Link_Name    : constant String := Class.Link_Name;
         Property_Top_Name  : constant String :=
                                Property_Link_Name
                                  (Property_Link_Name'First
                                   .. Index (Property_Link_Name, "__"));
         Class_Top_Name     : constant String :=
                                Class_Link_Name
                                  (Class_Link_Name'First
                                   .. Index (Class_Link_Name, "__"));
         Count              : Natural := 0;
      begin
         for I in Property_Link_Name'First .. Property_Link_Name'Last - 1 loop
            if Property_Link_Name (I .. I + 1) = "__" then
               Count := Count + 1;
            end if;
         end loop;

         return Count = 2
           and then Property_Link_Name /= Class_Link_Name
           and then Property_Top_Name = Class_Top_Name;
      end Is_Group_Reference;

      ----------------
      -- Load_Class --
      ----------------

      procedure Load_Class
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
      begin
         Ack.Compile.Compile_Class
           (Ada.Directories.Full_Name (Directory_Entry), Image,
            Add_Feature_Binding'Access);
      end Load_Class;

      Group_Name : constant String :=
                     Aquarius.Actions.Action_Group_Name (Group);

   begin
      Ada.Text_IO.Create (Action_File, Ada.Text_IO.Out_File,
                          Action_File_Path);

      Ada.Directories.Search
        (Base_Aqua_Path,
         Grammar.Name & "-" & Group_Name & "*.aqua",
         Process => Load_Class'Access);

      Ada.Text_IO.Close (Action_File);

      return not Ack.Errors.Has_Errors;

--        if not Ack.Errors.Has_Errors then
--           Load_Action_File
--             (Full_Path => Action_File_Path,
--              Group     => Group,
--              Image     => Image);
--        end if;

   end Load_Ack_Binding;

end Ack.Bindings;
