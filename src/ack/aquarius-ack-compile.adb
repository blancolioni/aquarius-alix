with Ada.Calendar;
with Ada.Directories;
with Ada.Text_IO;

with Aquarius.Ack.Parser;
with Aquarius.Ack.Semantic;
with Aquarius.Ack.Generate;

with Aquarius.Ack.Errors;

with Aquarius.Loader;
with Aquarius.Messages;
with Aquarius.Messages.Console;

with Aquarius.Actions;
with Aquarius.Grammars;
with Aquarius.Grammars.Manager;

with Aquarius.Paths;

package body Aquarius.Ack.Compile is

   procedure Load_Class
     (Source_Path : String;
      To_Image    : Aqua.Images.Image_Type);

   procedure Generate_Object_Code
     (Base_Name   : String);

   -------------------
   -- Compile_Class --
   -------------------

   procedure Compile_Class
     (Source_Path : String;
      To_Image    : Aqua.Images.Image_Type;
      Feature_Callback : access
        procedure (Class        : Entity_Id;
                   Feature_Name : String;
                   Child_Name   : String;
                   Child_Type   : Entity_Id))
   is
      Base_Name : constant String :=
                    Ada.Directories.Base_Name (Source_Path);
   begin
      if not Class_Object_Paths.Contains (Base_Name) then
         Load_Class (Source_Path, To_Image);

         for Partial_Class of Partial_Class_List loop
            declare
               Key : constant String :=
                       Get_File_Name (Get_Entity (Partial_Class));
            begin
               if not Class_Object_Paths.Contains (Key) then
                  declare
                     Program : constant Aquarius.Programs.Program_Tree :=
                                 Get_Program (Partial_Class);
                     Source_Path : constant String :=
                                     Program.Source_Directory
                                     & "/" & Program.Source_File_Name;
                  begin
                     Load_Class (Source_Path, To_Image);
                  end;
               end if;
            end;
         end loop;

         Partial_Class_List.Clear;
      end if;

      if Feature_Callback /= null then
         declare
            Class : constant Real_Entity_Id :=
                      Get_Entity (Loaded_Classes.Element (Base_Name));

            function OK (Entity : Entity_Id) return Boolean
            is (Get_Kind (Entity) = Routine_Feature_Entity);

            procedure Call (Feature : Entity_Id);

            ----------
            -- Call --
            ----------

            procedure Call (Feature : Entity_Id) is
            begin
               Feature_Callback
                 (Class, To_Standard_String (Get_Name (Feature)),
                  "", No_Entity);
            end Call;

         begin
            Scan_Children (Class, OK'Access, Call'Access);
         end;
      end if;

   end Compile_Class;

   --------------------------
   -- Generate_Object_Code --
   --------------------------

   procedure Generate_Object_Code
     (Base_Name   : String)
   is
      Assembly_Path    : constant String :=
                           Aquarius.Paths.Scratch_File
                             (Base_Name, "m32");
      Assembly_Program : constant Aquarius.Programs.Program_Tree :=
                           Aquarius.Loader.Load_From_File
                             (Assembly_Path);
      Assembly_Grammar : constant Aquarius.Grammars.Aquarius_Grammar :=
                           Aquarius.Grammars.Manager.Get_Grammar_For_File
                             (Assembly_Path);
   begin
      Assembly_Grammar.Run_Action_Trigger
        (Assembly_Program, Aquarius.Actions.Semantic_Trigger);
   end Generate_Object_Code;

   ----------------
   -- Load_Class --
   ----------------

   procedure Load_Class
     (Source_Path : String;
      To_Image    : Aqua.Images.Image_Type)
   is
      Base_Name : constant String :=
                    Ada.Directories.Base_Name (Source_Path);
   begin
      if not Class_Object_Paths.Contains (Base_Name) then
         if not Loaded_Classes.Contains (Base_Name) then
            declare
               Source_Program : constant Aquarius.Programs.Program_Tree :=
                                  Aquarius.Loader.Load_From_File
                                    (Source_Path);
               Node           : constant Aquarius.Ack.Node_Id :=
                                  Aquarius.Ack.Parser.Import
                                    (Source_Program);
            begin
               Aquarius.Ack.Semantic.Analyse_Class_Declaration (Node);
               Aquarius.Ack.Errors.Record_Errors (Node);

               declare
                  use Aquarius.Messages;
                  List : Message_List;
               begin
                  Source_Program.Get_Messages (List);
                  if Message_Count (List) > 0 then
                     Aquarius.Messages.Console.Show_Messages (List);
                     if Highest_Level (List) > Warning then
                        return;
                     end if;
                  end if;
               end;

               Loaded_Classes.Insert (Base_Name, Node);
            end;

         end if;

         declare
            use Ada.Directories, Ada.Calendar;
            Object_Path : constant String :=
                            Aquarius.Paths.Scratch_File
                              (Base_Name & ".o32");
         begin
            if not Exists (Object_Path)
              or else Modification_Time (Object_Path)
              < Modification_Time (Source_Path)
            then
               Ada.Text_IO.Put_Line
                 ("generating " & Base_Name);
               Aquarius.Ack.Generate.Generate_Class_Declaration
                 (Loaded_Classes.Element (Base_Name));

               Generate_Object_Code (Base_Name);
            end if;

            To_Image.Load (Base_Name & ".o32");
            Class_Object_Paths.Insert (Base_Name, Object_Path);

         end;

      end if;

   end Load_Class;

end Aquarius.Ack.Compile;
