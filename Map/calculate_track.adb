with DoubleTalk;
with Console_Management;
procedure Calculate_Track is

begin

   procedure Hall_Triggered is

      type Int_Array is (Positive) of Layout.Block_ID;

      My_Block_Array : Int_Array :=

   begin



   end Hall_Triggered;

   Ada.Text_IO.Put_Line ("To get started, first place the green Burlington " &
                        "Northern on block 21 facing in reverse. " &
                           "It is vital that you place only the Burlington " &
                           "Northern on the track.");
   Ada.Text_IO.Put_Line ("The next part is easy, make sure the track is " &
                           "unobstructed, the block power is on, and the " &
                           "double talk speaker is turned up!");
   Ada.Text_IO.Put_Line ("Once you have completed these things hit [ENTER]!");
   Ada.Text_IO.Skip_Line;
   DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                     ("WARNING! Do not touch the trains or interrupt the" &
                       "power for any reason."),
                     Voice => DoubleTalk.Vader);
   DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                     ("This test will run on its own,a status bar will be " &
                        "displayed on the screen to moniter train status."),
                     Voice => DoubleTalk.Vader);
   delay 5.0;
   Console_Management.Clear_Screen;
   Console_Management.Set_Cursor (Row    => 0,
                                  Column => 25);
   Ada.Text_IO.Put_Line ("Starting the test");
   -- Set cab to 100
   Cabs.Set_Limit (Cab   => 1,
                   Value => 100);
   Cabs.Set (Cab   => 1,
             Value => 100);

   -- Power Next Block, wait half a second and power down old block
   -- Stop timer and store time while starting new timer







end Calculate_Track;
