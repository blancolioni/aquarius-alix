private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

package Aquarius.Sections.Layout is

   type Section_Layout is private;

   procedure Initialise (Layout        : in out Section_Layout;
                         Width, Height : Positive);

   procedure Show_Section (Layout  : in out Section_Layout;
                           Section : Aquarius_Section;
                           Hint_X  : Integer;
                           Hint_Y  : Integer);

   procedure Render
     (Layout       : Section_Layout;
      X_Min, X_Max : Integer;
      Y_Min, Y_Max : Integer;
      Renderer     : not null access
        procedure (Section : Aquarius_Section;
                   X, Y    : Integer));

   procedure Render_Overview
     (Layout : Section_Layout;
      Renderer     : not null access
        procedure (Section : Aquarius_Section;
                   X, Y    : Integer));

private

   package List_Of_Section_Indices is
     new Ada.Containers.Doubly_Linked_Lists (Positive);

   type Section_Entry is
      record
         Section       : Aquarius_Section;
         X, Y          : Integer;
      end record;

   package Section_Entry_Vectors is
     new Ada.Containers.Vectors
       (Positive, Section_Entry);

   type Section_Layout is
      record
         Width, Height : Positive;
         Entries       : Section_Entry_Vectors.Vector;
         Horizontal    : List_Of_Section_Indices.List;
      end record;

end Aquarius.Sections.Layout;
