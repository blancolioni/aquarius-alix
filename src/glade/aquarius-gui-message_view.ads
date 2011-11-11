with Gtk.Builder;

with Aquarius.Messages;

package Aquarius.GUI.Message_View is

   procedure Initialise (Builder : Gtk.Builder.Gtk_Builder);

   procedure Add_Message_Source
     (M : access Aquarius.Messages.Message_Location'Class);

   procedure Remove_Message_Source
     (M : access Aquarius.Messages.Message_Location'Class);

   procedure Update;

end Aquarius.GUI.Message_View;
