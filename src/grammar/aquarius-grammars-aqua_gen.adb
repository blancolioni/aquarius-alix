with Ada.Characters.Handling;
with Ada.Text_IO;

package body Aquarius.Grammars.Aqua_Gen is

   procedure Generate_Class
     (Language_Name : String;
      Syntax        : Aquarius.Syntax.Syntax_Tree;
      Path          : String);

   function To_Mixed_Case (Name : String) return String;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Grammar : Aquarius_Grammar;
      Path    : String)
   is
   begin
      for Syntax of Grammar.Non_Terminals loop
         Generate_Class (Grammar.Name, Syntax, Path);
      end loop;
   end Generate;

   --------------------
   -- Generate_Class --
   --------------------

   procedure Generate_Class
     (Language_Name : String;
      Syntax        : Aquarius.Syntax.Syntax_Tree;
      Path          : String)
   is
      use Ada.Characters.Handling;
      use Ada.Text_IO;

      use Aquarius.Syntax;

      procedure Scan_Features (Tree : Aquarius.Syntax.Syntax_Tree);

      -------------------
      -- Scan_Features --
      -------------------

      procedure Scan_Features (Tree : Aquarius.Syntax.Syntax_Tree) is

         procedure Put_Feature (Prefix : String);

         -----------------
         -- Put_Feature --
         -----------------

         procedure Put_Feature (Prefix : String) is
         begin
            New_Line;
            Put_Line ("   " & To_Mixed_Case (Prefix & Tree.Name)
                      & " (Child : "
                      & To_Mixed_Case (Tree.Name & "_Node")
                      & ")");
            Put_Line ("      require");
            Put_Line ("         Child /= null");
            Put_Line ("      do");
            Put_Line ("      end");
         end Put_Feature;

      begin
         if Tree.Syntax_Class = Terminal then
            null;
         elsif Tree.Name /= "" then
            Put_Feature ("Before_");
            Put_Feature ("After_");
         else
            for I in 1 .. Tree.Child_Count loop
               Scan_Features (Aquarius.Syntax.Syntax_Tree (Tree.Child (I)));
            end loop;
         end if;
      end Scan_Features;

      File : File_Type;
   begin
      Create (File, Out_File,
              Path & "/"
              & To_Lower (Syntax.Name)
              & "_node"
              & ".aqua");
      Set_Output (File);
      Put_Line ("class "
                & To_Mixed_Case (Language_Name)
                & ".Syntax."
                & To_Mixed_Case (Syntax.Name & "_Node"));
      New_Line;
      Put_Line ("inherit");
      Put_Line ("   Program_Node");
      New_Line;
      Put_Line ("feature");
      for I in 1 .. Syntax.Child_Count loop
         Scan_Features (Aquarius.Syntax.Syntax_Tree (Syntax.Child (I)));
      end loop;
      New_Line;
      Put_Line ("end");
      Set_Output (Standard_Output);
      Close (File);
   end Generate_Class;

   -------------------
   -- To_Mixed_Case --
   -------------------

   function To_Mixed_Case (Name : String) return String is
      use Ada.Characters.Handling;
      Result : String := Name;
      First  : Boolean := True;
   begin
      for I in Result'Range loop
         if Result (I) = '_' then
            First := True;
         elsif First then
            Result (I) := To_Upper (Result (I));
            First := False;
         end if;
      end loop;
      return Result;
   end To_Mixed_Case;

end Aquarius.Grammars.Aqua_Gen;
