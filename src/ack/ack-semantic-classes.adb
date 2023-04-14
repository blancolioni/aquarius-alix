with Ada.Directories;
--  with Ada.Text_IO;

with Aquarius.Loader;
with Aquarius.Programs.Komnenos_Entities;

with Komnenos.Entities.Tables;

with Ack.Files;
with Ack.Parser;

package body Ack.Semantic.Classes is

   ----------------
   -- Load_Class --
   ----------------

   function Load_Class
     (Referrer        : Aquarius.Programs.Program_Tree;
      Parent          : not null access Root_Entity_Type'Class;
      Name            : Name_Id)
      return Ack.Classes.Class_Entity
   is
      Entity : Entity_Type :=
                 (if Parent.Contains (Name)
                  then Parent.Get (Name)
                  else null);
   begin

      if Entity = null then
         declare
            Path : constant String :=
              Ack.Files.Find_Class_File
                (Referrer, Parent, Name);
         begin
            if Path /= "" then
               declare
                  Base_Name : constant String :=
                    Ada.Directories.Base_Name (Path);
                  Node : Node_Id;
               begin
                  if Loaded_Classes.Contains (Base_Name) then
                     Node := Loaded_Classes.Element (Base_Name);
                     Entity := Get_Entity (Node);
                  else
                     declare
                        Program : constant Aquarius.Programs.Program_Tree :=
                          Aquarius.Loader.Load_From_File
                            (Path);
                     begin
                        Node := Ack.Parser.Import (Program);
                        Ack.Semantic.Analyse_Class_Declaration (Node);
                        Entity := Get_Entity (Node);
                        Parent.Insert (Entity);

                        declare
                           Base_Name : constant String :=
                             Entity.Base_File_Name;
                        begin
                           Loaded_Classes.Insert
                             (Base_Name, Node);
                        end;

                        declare
                           use Aquarius.Programs.Komnenos_Entities;
                        begin
                           Create_Aquarius_Source_Entity
                             (Table            =>
                                Komnenos.Entities.Tables.Table ("aqua"),
                              Name             => Entity.Qualified_Name,
                              Qualified_Name   => Entity.Qualified_Name,
                              Class_Name       => "class",
                              Top_Level        => True,
                              Compilation_Unit => Program,
                              Defining_Name    => Program,
                              Entity_Spec      => Program,
                              Entity_Body      => Program);
                        end;

                        Partial_Class_List.Append (Node);

                        --  Ada.Text_IO.Put_Line
                        --    ("loaded: "
                        --     & Entity.Full_Name);

                     end;
                  end if;

               end;
            end if;
         end;
      end if;

      if Entity = null then
         return null;
      else
         return Ack.Classes.Class_Entity (Entity);
      end if;

   end Load_Class;

end Ack.Semantic.Classes;
