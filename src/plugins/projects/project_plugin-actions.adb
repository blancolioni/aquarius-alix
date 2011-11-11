with Ada.Characters.Handling;

with Aquarius.Programs;                 use Aquarius.Programs;
with Aquarius.Projects;                 use Aquarius.Projects;
with Aquarius.Source;
with Aquarius.Trees.Properties;

package body Project_Plugin.Actions is

   procedure Source_File_Before
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure Package_Declaration_Before
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure Setting_Declaration_After
     (Target : not null access Aquarius.Actions.Actionable'Class);

   procedure Value_Iterator
     (Value     : Program_Tree;
      Project   : Aquarius_Project;
      Action    : not null access procedure
        (Project : not null access Aquarius_Project_Type'Class;
         Text    : String));

   procedure Set_Package_Setting (Project      : in Aquarius_Project;
                                  Package_Tree : in Program_Tree;
                                  Name         : in Program_Tree;
                                  Value        : in Program_Tree);

   procedure Set_Setting_Value (Project      : in Aquarius_Project;
                                Name         : in Program_Tree;
                                Value        : in Program_Tree);

   ------------------------
   -- Bind_Parse_Actions --
   ------------------------

  procedure Bind_Parse_Actions
    (Plugin    : not null access Project_Plugin_Type'Class;
     Parser    : Aquarius.Actions.Action_Group)
  is
  begin
      Plugin.Register_Action
        ("source_file", Parser, Aquarius.Before,
         Source_File_Before'Access);
      Plugin.Register_Action
        ("package_declaration", Parser, Aquarius.Before,
         Package_Declaration_Before'Access);
      Plugin.Register_Action
        ("setting_declaration", Parser, Aquarius.After,
         Setting_Declaration_After'Access);
  end Bind_Parse_Actions;

  --------------------------------
  -- Package_Declaration_Before --
  --------------------------------

  procedure Package_Declaration_Before
    (Target : not null access Aquarius.Actions.Actionable'Class)
  is
     Tree : constant Program_Tree := Program_Tree (Target);
  begin
     Tree.Set_Property (Plugin.Package_Property, Tree);
  end Package_Declaration_Before;

  -------------------------
  -- Set_Package_Setting --
  -------------------------

  procedure Set_Package_Setting (Project      : in Aquarius_Project;
                                 Package_Tree : in Program_Tree;
                                 Name         : in Program_Tree;
                                 Value        : in Program_Tree)
  is
     pragma Unreferenced (Project);
     pragma Unreferenced (Name);
     pragma Unreferenced (Package_Tree);
     pragma Unreferenced (Value);
  begin
     null;
  end Set_Package_Setting;

  -----------------------
  -- Set_Setting_Value --
  -----------------------

  procedure Set_Setting_Value (Project      : in Aquarius_Project;
                               Name         : in Program_Tree;
                               Value        : in Program_Tree)
  is
     Setting : constant String :=
       Ada.Characters.Handling.To_Lower (Name.First_Leaf.Text);
  begin
     if Setting = "source_dirs" then
        Value_Iterator (Value, Project, Add_Search_Path'Access);
     elsif Setting = "main" then
        Value_Iterator (Value, Project, Add_Main'Access);
     end if;
  end Set_Setting_Value;

  -------------------------------
  -- Setting_Declaration_After --
  -------------------------------

  procedure Setting_Declaration_After
    (Target : not null access Aquarius.Actions.Actionable'Class)
  is
     Setting    : constant Program_Tree     := Program_Tree (Target);
     Project    : constant Aquarius_Project :=
       Aquarius.Trees.Properties.Get_Project (Setting.all);
     In_Package : constant Boolean :=
       Setting.Has_Property (Plugin.Package_Property);
     Name       : constant Program_Tree :=
       Setting.Program_Child ("setting_name");
     Value      : constant Program_Tree := Setting.Program_Child ("value");
  begin
     if In_Package then
        declare
           P : constant Program_Tree :=
             Program_Tree (Setting.Property (Plugin.Package_Property));
        begin
           Set_Package_Setting (Project, Name, P, Value);
        end;
     else
        Set_Setting_Value (Project, Name, Value);
     end if;
  end Setting_Declaration_After;

  ------------------------
  -- Source_File_Before --
  ------------------------

  procedure Source_File_Before
    (Target : not null access Aquarius.Actions.Actionable'Class)
  is
     Source_File : constant Program_Tree := Program_Tree (Target);
     P : constant Aquarius_Project :=
       New_Project (Aquarius.Source.Get_Full_Path (Source_File.Source),
                    Aquarius.Trees.Properties.Get_UI (Source_File.all));
  begin
     Aquarius.Trees.Properties.Set_Project (Source_File.all, P);
  end Source_File_Before;

  --------------------
  -- Value_Iterator --
  --------------------

  procedure Value_Iterator
    (Value     : Program_Tree;
     Project   : Aquarius_Project;
     Action    : not null access procedure
       (Project : not null access Aquarius_Project_Type'Class;
        Text    : String))
  is
     function Strip_Quotes (String_Literal : String) return String;

     ------------------
     -- Strip_Quotes --
     ------------------

     function Strip_Quotes (String_Literal : String) return String is
     begin
        return String_Literal (String_Literal'First + 1 ..
                                 String_Literal'Last - 1);
     end Strip_Quotes;

     Simple_Value : constant Program_Tree :=
       Value.Program_Child ("simple_value");
     List_Value   : constant Program_Tree :=
       Value.Program_Child ("list_value");
  begin
     if List_Value = null then
        Action (Project, Strip_Quotes (Simple_Value.First_Leaf.Text));
     else
        declare
           Values : constant Array_Of_Program_Trees :=
             List_Value.Direct_Children;
        begin
           for I in Values'Range loop
              Action (Project, Strip_Quotes (Values (I).First_Leaf.Text));
           end loop;
        end;
     end if;
  end Value_Iterator;

end Project_Plugin.Actions;
