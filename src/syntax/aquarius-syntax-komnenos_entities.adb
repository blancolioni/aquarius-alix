with Komnenos.Entities.Visuals;
with Komnenos.Entities.Visual_Manager;
with Komnenos.Fragments;
with Komnenos.Fragments.Diagrams;
with Komnenos.Styles;
with Komnenos.Themes;
with Komnenos.UI;

with Aquarius.Config_Paths;

package body Aquarius.Syntax.Komnenos_Entities is

   function Get_Key
     (Grammar_Name : String;
      Node_Name    : String)
      return String
   is ("syntax://" & Grammar_Name & "/" & Node_Name);

   type Aquarius_Syntax_Entity is
     new Komnenos.Entities.Root_Entity_Reference with
      record
         Grammar_Name : Aquarius.Names.Aquarius_Name;
         Syntax       : Syntax_Tree;
      end record;

   overriding function Top_Level
     (Entity : Aquarius_Syntax_Entity)
      return Boolean
   is (True);

   overriding function Key
     (Item : Aquarius_Syntax_Entity)
      return String
   is (Get_Key (Aquarius.Names.To_String (Item.Grammar_Name),
                Item.Syntax.Name));

   overriding procedure Select_Entity
     (Entity : not null access Aquarius_Syntax_Entity;
      Table  : access Komnenos.Entities.Entity_Table_Interface'Class;
      Parent : access Komnenos.Entities.Entity_Visual'Class;
      Visual : access Komnenos.Entities.Entity_Visual'Class;
      Offset : Komnenos.Pixel_Offset);

   overriding procedure Render
     (Entity : not null access Aquarius_Syntax_Entity;
      Visual : not null access Komnenos.Entities.Entity_Visual'Class);

   procedure Render_Text
     (Entity : not null access Aquarius_Syntax_Entity'Class;
      Visual : in out Komnenos.Entities.Text_Entity_Visual'Class);

   procedure Render_Diagram
     (Entity : not null access Aquarius_Syntax_Entity'Class;
      Visual : in out Komnenos.Entities.Visuals.Diagram_Visual'Class);

   -----------------------------------
   -- Create_Aquarius_Syntax_Entity --
   -----------------------------------

   procedure Create_Aquarius_Syntax_Entity
     (Table        : not null access
        Komnenos.Entities.Entity_Table_Interface'Class;
      Grammar_Name : String;
      Tree         : Syntax_Tree)
   is
      Entity : Aquarius_Syntax_Entity;
      Path   : constant String :=
                 Aquarius.Config_Paths.Config_File
                   ("grammar/" & Grammar_Name
                    & "/" & Grammar_Name & ".ebnf");
   begin
      Entity.Create
        (Identifier   => Tree.Name,
         Class_Name   => "aquarius-syntax-entity",
         Path         => Path,
         Display_Text => Tree.Name,
         Description  => Path);

      Entity.Grammar_Name := Aquarius.Names.To_Aquarius_Name (Grammar_Name);
      Entity.Syntax := Tree;
      Table.Add_Entity
        (Entity.Key, new Aquarius_Syntax_Entity'(Entity));
   end Create_Aquarius_Syntax_Entity;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Entity : not null access Aquarius_Syntax_Entity;
      Visual : not null access Komnenos.Entities.Entity_Visual'Class)
   is
   begin
      if Visual.all in Komnenos.Entities.Text_Entity_Visual'Class then
         Render_Text
           (Entity,
            Komnenos.Entities.Text_Entity_Visual'Class (Visual.all));
      elsif Visual.all in
        Komnenos.Entities.Visuals.Diagram_Visual'Class
      then
         Render_Diagram
           (Entity,
            Komnenos.Entities.Visuals.Diagram_Visual (Visual.all));
      end if;
   end Render;

   --------------------
   -- Render_Diagram --
   --------------------

   procedure Render_Diagram
     (Entity : not null access Aquarius_Syntax_Entity'Class;
      Visual : in out Komnenos.Entities.Visuals.Diagram_Visual'Class)
   is
      use Komnenos;
      use all type Komnenos.Entities.Visuals.Node_Style;

      Default_Style      : constant Komnenos.Styles.Komnenos_Style :=
                             Komnenos.Themes.Active_Theme.Default_Style;
      Keyword_Style      : constant Komnenos.Styles.Komnenos_Style :=
                             Komnenos.Themes.Active_Theme.Style
                               ("reserved_identifier");
      Non_Terminal_Style : constant Komnenos.Styles.Komnenos_Style :=
                             Komnenos.Themes.Active_Theme.Style
                               ("italic");

      type Context_Type is
         record
            X, Y   : Positive := 1;
            Next_X : Positive := 1;
            Next_Y : Positive := 1;
         end record;

      procedure Put_Non_Terminal
        (Context   : in out Context_Type;
         Name      : String);

      procedure Put_Terminal
        (Context  : in out Context_Type;
         Terminal : Aquarius.Tokens.Token);

      procedure Render
        (Context : in out Context_Type;
         Syntax  : Syntax_Tree);

      procedure Render_Children
        (Context : in out Context_Type;
         Syntax  : Syntax_Tree);

      ----------------------
      -- Put_Non_Terminal --
      ----------------------

      procedure Put_Non_Terminal
        (Context   : in out Context_Type;
         Name      : String)
      is
         Key : constant String :=
                 Get_Key
                   (Aquarius.Names.To_String (Entity.Grammar_Name),
                    Name);
      begin
         Visual.Put_Node
           (Key         => Key,
            X           => Context.X,
            Y           => Context.Y,
            Style       => Box,
            Label_Text  => Name,
            Label_Style => Non_Terminal_Style,
            Tool_Tip    => "",
            Link        => null);
         Context.Next_X := Positive'Max (Context.Next_X, Context.X + 1);
         Context.Next_Y := Positive'Max (Context.Next_Y, Context.Y + 1);
      end Put_Non_Terminal;

      ------------------
      -- Put_Terminal --
      ------------------

      procedure Put_Terminal
        (Context  : in out Context_Type;
         Terminal : Aquarius.Tokens.Token)
      is
         Label : constant String :=
                   Aquarius.Tokens.Get_Name
                     (Entity.Syntax.Frame,
                      Terminal);
         Key   : constant String :=
                   Get_Key
                     (Aquarius.Names.To_String (Entity.Grammar_Name),
                      Label);
      begin
         Visual.Put_Node
           (Key         => Key,
            X           => Context.X,
            Y           => Context.Y,
            Style       => Rounded_Box,
            Label_Text  => Label,
            Label_Style =>
              (if Aquarius.Tokens.Is_Reserved
                   (Entity.Syntax.Frame, Terminal)
               then Keyword_Style
               else Default_Style),
            Tool_Tip    => "",
            Link        => null);
      end Put_Terminal;

      ------------
      -- Render --
      ------------

      procedure Render
        (Context : in out Context_Type;
         Syntax  : Syntax_Tree)
      is
         Old_Context : constant Context_Type := Context;
      begin
         if Syntax /= Entity.Syntax
           and then Syntax.Syntax_Class /= Terminal
           and then Syntax.Name /= ""
         then
            Put_Non_Terminal (Context, Syntax.Name);
         elsif Syntax.Syntax_Class = Choice then
            for I in 1 .. Syntax.Child_Count loop
               declare
                  Child : constant Syntax_Tree :=
                            Syntax.Syntax_Child (I);
               begin
                  if Child.Text /= "" then
                     Put_Non_Terminal (Context, Child.Text);
                  else
                     Render (Context, Child);
                  end if;
               end;
               Context.Y := Context.Next_Y;
               Context.X := Old_Context.X;
            end loop;

         elsif Syntax.Syntax_Class = Terminal then
            Put_Terminal (Context, Syntax.Token);
         else
            Render_Children (Context, Syntax);
         end if;

      end Render;

      ---------------------
      -- Render_Children --
      ---------------------

      procedure Render_Children
        (Context : in out Context_Type;
         Syntax  : Syntax_Tree)
      is
         Start_Context : constant Context_Type := Context;
      begin
         for I in 1 .. Syntax.Child_Count loop
            declare
               Child : constant Syntax_Tree :=
                         Syntax.Syntax_Child (I);
            begin
--                 if Child.Optional then
--                    if Child.Repeatable then
--                       Put (Context, " {", Punctuation_Style);
--                    else
--                       Put (Context, " [", Punctuation_Style);
--                    end if;
--                    Context.Need_Space := True;
--                 elsif Child.Repeatable then
--                    Put (Context, " <", Punctuation_Style);
--                    Context.Need_Space := True;
--                 end if;

               Render (Context, Child);
               Context.X := Context.Next_X;
               Context.Y := Start_Context.Y;

--                 if Child.Has_Separator then
--                    Put (Context, " / " & Child.Separator.Text,
--                         Punctuation_Style);
--                 end if;
--
--                 if Child.Optional then
--                    if Child.Repeatable then
--                       Put (Context, " }", Punctuation_Style);
--                    else
--                       Put (Context, " ]", Punctuation_Style);
--                    end if;
--                    Context.Need_Space := True;
--                 elsif Child.Repeatable then
--                    Put (Context, " >", Punctuation_Style);
--                    Context.Need_Space := True;
--                 end if;
            end;
         end loop;
      end Render_Children;

      Context : Context_Type;

   begin
      Visual.Clear;

      Visual.Put_Node
        (Key         => "in",
         X           => 1,
         Y           => 1,
         Style       => Small_Circle,
         Label_Text  => "",
         Label_Style => Default_Style,
         Tool_Tip    => "",
         Link        => null);
      Context.X := 2;
      Context.Next_X := 2;

      Render (Context, Entity.Syntax);

      Visual.Put_Node
        (Key         => "out",
         X           => Context.Next_X + 1,
         Y           => 1,
         Style       => Small_Circle,
         Label_Text  => "",
         Label_Style => Default_Style,
         Tool_Tip    => "",
         Link        => null);

   end Render_Diagram;

   -----------------
   -- Render_Text --
   -----------------

   procedure Render_Text
     (Entity : not null access Aquarius_Syntax_Entity'Class;
      Visual : in out Komnenos.Entities.Text_Entity_Visual'Class)
   is
      use Komnenos;

      Default_Style      : constant Komnenos.Styles.Komnenos_Style :=
                             Komnenos.Themes.Active_Theme.Default_Style;
      Keyword_Style      : constant Komnenos.Styles.Komnenos_Style :=
                             Komnenos.Themes.Active_Theme.Style
                               ("reserved_identifier");
      Punctuation_Style  : constant Komnenos.Styles.Komnenos_Style :=
                             Komnenos.Themes.Active_Theme.Style
                               ("default");
      Non_Terminal_Style : constant Komnenos.Styles.Komnenos_Style :=
                             Komnenos.Themes.Active_Theme.Style
                               ("italic");

      type Context_Type is
         record
            Col          : Positive := 1;
            Indent       : Positive := 1;
            Right_Margin : Positive := 40;
            Need_Space   : Boolean := False;
         end record;

      procedure Put
        (Context   : in out Context_Type;
         Text      : String;
         Style     : Komnenos.Styles.Komnenos_Style := Default_Style;
         Reference : Komnenos.Entities.Entity_Reference := null);

      procedure New_Line
        (Context : in out Context_Type);

      procedure Render
        (Context : in out Context_Type;
         Syntax  : Syntax_Tree);

      procedure Render_Children
        (Context : in out Context_Type;
         Syntax  : Syntax_Tree);

      --------------
      -- New_Line --
      --------------

      procedure New_Line
        (Context : in out Context_Type)
      is
      begin
         Visual.New_Line;
         Context.Need_Space := False;
         Context.Col := Context.Indent;
         if Context.Indent > 1 then
            declare
               Spaces : constant String (1 .. Context.Indent - 1) :=
                          (others => ' ');
            begin
               Put (Context, Spaces);
            end;
         end if;
      end New_Line;

      ---------
      -- Put --
      ---------

      procedure Put
        (Context   : in out Context_Type;
         Text      : String;
         Style     : Komnenos.Styles.Komnenos_Style := Default_Style;
         Reference : Komnenos.Entities.Entity_Reference := null)
      is
      begin
         if Text'Length + Context.Col >= Context.Right_Margin then
            New_Line (Context);
         end if;
         if Context.Need_Space
           and then Text /= ""
           and then Text (Text'First) /= ' '
         then
            Visual.Put (" ", Style, "", null);
            Context.Col := Context.Col + 1;
         end if;
         Context.Need_Space := False;
         Visual.Put (Text, Style, "", Reference);
         Context.Col := Context.Col + Text'Length;
      end Put;

      ------------
      -- Render --
      ------------

      procedure Render
        (Context : in out Context_Type;
         Syntax  : Syntax_Tree)
      is
         Old_Indent : constant Positive := Context.Indent;
      begin
         if Syntax /= Entity.Syntax
           and then Syntax.Syntax_Class /= Terminal
           and then Syntax.Name /= ""
         then
            Put (Context, Syntax.Name, Non_Terminal_Style,
                 Komnenos.UI.Current_UI.Get
                   (Get_Key
                      (Aquarius.Names.To_String (Entity.Grammar_Name),
                       Syntax.Name)));
            Context.Need_Space := True;

         elsif Syntax.Syntax_Class = Choice then
            Context.Indent := Context.Col;
            if Context.Indent > 10 then
               Context.Indent := 1;
               New_Line (Context);
            end if;

            for I in 1 .. Syntax.Child_Count loop
               declare
                  Child : constant Syntax_Tree :=
                            Syntax.Syntax_Child (I);
               begin
                  if I = 1 then
                     if Context.Col = 1 then
                        Put (Context, "  ");
                     else
                        Put (Context, " ");
                     end if;
                  else
                     New_Line (Context);
                     Put (Context, "| ", Punctuation_Style);
                  end if;

                  if Child.Text /= "" then
                     Put (Context, Child.Text, Non_Terminal_Style);
                  else
                     Render (Context, Child);
                  end if;
               end;
            end loop;
            Context.Indent := Old_Indent;

         elsif Syntax.Syntax_Class = Terminal then
            if Aquarius.Tokens.Is_Reserved (Syntax.Frame, Syntax.Token) then
               Put (Context, Syntax.Text, Keyword_Style);
            else
               Put (Context, Syntax.Text, Non_Terminal_Style);
            end if;
            Context.Need_Space := True;
         else
            Render_Children (Context, Syntax);
         end if;

      end Render;

      ---------------------
      -- Render_Children --
      ---------------------

      procedure Render_Children
        (Context : in out Context_Type;
         Syntax  : Syntax_Tree)
      is
      begin
         for I in 1 .. Syntax.Child_Count loop
            declare
               Child : constant Syntax_Tree :=
                         Syntax.Syntax_Child (I);
            begin
               if Child.Optional then
                  if Child.Repeatable then
                     Put (Context, " {", Punctuation_Style);
                  else
                     Put (Context, " [", Punctuation_Style);
                  end if;
                  Context.Need_Space := True;
               elsif Child.Repeatable then
                  Put (Context, " <", Punctuation_Style);
                  Context.Need_Space := True;
               end if;

               Render (Context, Child);

               if Child.Has_Separator then
                  Put (Context, " / " & Child.Separator.Text,
                       Punctuation_Style);
               end if;

               if Child.Optional then
                  if Child.Repeatable then
                     Put (Context, " }", Punctuation_Style);
                  else
                     Put (Context, " ]", Punctuation_Style);
                  end if;
                  Context.Need_Space := True;
               elsif Child.Repeatable then
                  Put (Context, " >", Punctuation_Style);
                  Context.Need_Space := True;
               end if;
            end;
         end loop;
      end Render_Children;

      Context : Context_Type;

   begin
      Visual.Clear;
      Render (Context, Entity.Syntax);
   end Render_Text;

   -------------------
   -- Select_Entity --
   -------------------

   overriding procedure Select_Entity
     (Entity : not null access Aquarius_Syntax_Entity;
      Table  : access Komnenos.Entities.Entity_Table_Interface'Class;
      Parent : access Komnenos.Entities.Entity_Visual'Class;
      Visual : access Komnenos.Entities.Entity_Visual'Class;
      Offset : Komnenos.Pixel_Offset)
   is
      Fragment : constant Komnenos.Fragments.Fragment_Type :=
                   (if Visual = null
                    then Komnenos.Fragments.Fragment_Type
                      (Komnenos.Fragments.Diagrams.New_Diagram
                         (Entity))
                    else Komnenos.Fragments.Fragment_Type (Visual));
      pragma Unreferenced (Table);
   begin
      Komnenos.Entities.Visual_Manager.Bind_Visual (Fragment, Entity);
      Entity.Render (Fragment);

      if Visual = null then
         Komnenos.UI.Current_UI.Place_Fragment
           (Parent, Offset, Fragment);
      end if;

      Fragment.Rendered;

   end Select_Entity;

end Aquarius.Syntax.Komnenos_Entities;
