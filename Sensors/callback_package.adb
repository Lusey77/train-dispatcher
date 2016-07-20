with Ada.Text_IO;
with DoubleTalk;
package body Callback_Package is

   -------------------
   -- Moniter_Halls --
   -------------------

   procedure Moniter_Halls (My_Hall : in Layout.Hall_ID) is
   begin
      Ada.Text_IO.Put_Line ("Hall sensor " & Layout.Hall_ID'Image (My_Hall) &
                              " triggered!");
      DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String (
                        "Hall sensor " & Layout.Hall_ID'Image (My_Hall) &
                              " triggered!"),
                        Voice  => DoubleTalk.Paul);
   end Moniter_Halls;

   procedure Put_Failure (Requestor : in Trains.Request_ID;
                          Turnout   : in Layout.Turnout_ID) is
   begin
      Ada.Text_IO.Put_Line ("Turnout #" & Layout.Turnout_ID'Image (Turnout) &
                              " has failed with requestor " &
                              Trains.Request_ID'Image (Requestor));
      DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String (
                        "Turnout #" & Layout.Turnout_ID'Image (Turnout) &
                              " has failed with requestor " &
                              Trains.Request_ID'Image (Requestor)),
                        Voice  => DoubleTalk.Paul);
   end Put_Failure;
   procedure Put_Recovery (Turnout  : in Layout.Turnout_ID) is
   begin
      Ada.Text_IO.Put_Line ("Turnout #" & Layout.Turnout_ID'Image (Turnout) &
                              " has recovered.");
      DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String (
                        "Turnout #" & Layout.Turnout_ID'Image (Turnout) &
                              " has recovered."),
                        Voice  => DoubleTalk.Paul);
   end Put_Recovery;

   procedure Put_Change (Turnout   : in Layout.Turnout_ID;
                         Direction : in Layout.Turn_Choice;
                         Moving    : in Boolean) is
   begin
      Ada.Text_IO.Put ("Turnout #" & Layout.Turnout_ID'Image (Turnout) &
                         " is ");
      DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String (
                        "Turnout #" & Layout.Turnout_ID'Image (Turnout) &
                         " is "),
                        Voice  => DoubleTalk.Paul);
      if not Moving then
         Ada.Text_IO.Put_Line ("positioned in " & Layout.Turn_Choice'Image
                               (Direction) & " direction.");
         DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                        ("positioned in " & Layout.Turn_Choice'Image
                               (Direction) & " direction."),
                        Voice  => DoubleTalk.Paul);
      else
         Ada.Text_IO.Put_Line (" moving in the " & Layout.Turn_Choice'Image
                               (Direction) & " direction.");
         DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                        (" moving in the " & Layout.Turn_Choice'Image
                               (Direction) & " direction."),
                        Voice  => DoubleTalk.Paul);
      end if;
   end Put_Change;

end Callback_Package;
