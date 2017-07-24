with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Ack.Classes;
with Ack.Compile;
with Ack.Errors;
with Ack.Features;
with Ack.Types;

package body Ack.Bindings is

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

      procedure Load_Class
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      procedure Add_Feature_Binding
        (Class        : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Feature_Name : String;
         Child_Name   : String;
         Child_Type   : Ack.Entity_Type);

      -------------------------
      -- Add_Feature_Binding --
      -------------------------

      procedure Add_Feature_Binding
        (Class        : not null access constant
           Ack.Classes.Class_Entity_Record'Class;
         Feature_Name : String;
         Child_Name   : String;
         Child_Type   : Ack.Entity_Type)
      is
         pragma Unreferenced (Child_Name, Child_Type);
         Index         : constant Natural :=
                           Ada.Strings.Fixed.Index (Feature_Name, "_");
         Position_Name : constant String :=
                           (if Index > 0
                            then Feature_Name (Feature_Name'First .. Index - 1)
                            else "");
         Child_Tree    : constant String :=
                           Feature_Name (Index + 1 .. Feature_Name'Last);
         Parent_Tree   : constant String := Class.Standard_Name;
         Feature       : constant Ack.Features.Feature_Entity :=
                           Class.Feature (Get_Name_Id (Feature_Name));
      begin
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

            Ada.Text_IO.Put_Line
              (Action_File,
               "   if not tree.__" & Parent_Tree & " then");
            Ada.Text_IO.Put_Line
              (Action_File,
               "      tree.__" & Parent_Tree & " :=");
            Ada.Text_IO.Put_Line
              (Action_File,
               "        "
               & Class.Link_Name
               & "$allocate");
            Ada.Text_IO.Put_Line
              (Action_File,
               "   end if");

            if Child_Tree = "node" then
               Ada.Text_IO.Put_Line
                 (Action_File,
                  "   call "
                  & "tree.__" & Parent_Tree & "."
                  & Class.Link_Name
                  & "."
                  & Position_Name & "_node"
                  & "(tree.__" & Parent_Tree & ")");
            else
               if Feature.Argument (1).Value_Type.Standard_Name = "string" then
                  if False then
                     Ada.Text_IO.Put_Line
                       (Action_File,
                        "   IO.put_line (tree.__"
                        & Parent_Tree & ".Image)");
                  end if;
                  Ada.Text_IO.Put_Line
                    (Action_File,
                     "   call "
                     & "tree.__" & Parent_Tree & "."
                     & Class.Link_Name
                     & "."
                     & Position_Name & "_" & Child_Tree
                     & "(tree.__" & Parent_Tree & ","
                     & "child.text);");
               end if;
            end if;

            Ada.Text_IO.Put_Line
              (Action_File,
               "end;");
            Ada.Text_IO.New_Line (Action_File);

         end if;
      end Add_Feature_Binding;

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
