with Gtk.Builder;

with Aquarius.Buffers;

package Aquarius.GUI.Source is

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder);

   procedure Load_File (Path : String);
   --  Load_File: read a file from the given path, creating a buffer
   --  and displaying it in a new source tab


   procedure Load_Buffer (Buffer : Aquarius.Buffers.Aquarius_Buffer;
                          Line   : Natural := 0;
                          Col    : Natural := 0);
   --  Load_Buffer: create a view for the buffer if one does not
   --  already exist.  Select the buffer view in the notebook.
   --  If Line is non-zero, navigate to the given line.
   --  If Col is non-zero, navigate to the given column

   procedure New_File (Grammar_Name : String);

end Aquarius.GUI.Source;
