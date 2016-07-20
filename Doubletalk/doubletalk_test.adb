--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullivan ------------------------------------------------
-- Tests      : DoubleTalk -----------------------------------------------------
-- Updated    : 9 Mar 15 -------------------------------------------------------
--------------------------------------------------------------------------------

with Ada.Text_IO;
with DoubleTalk;

procedure DoubleTalk_Test is

   String_Item : String (1 .. 80);
   Last_Item   : Integer;

begin

   -- Loop through and get phrases to output to the doubletalk unit
   loop
      Ada.Text_IO.Put ("Enter a phrase or enter a space to quit");
      Ada.Text_IO.Get_Line (Item => String_Item,
                            Last => Last_Item);
      exit when String_Item (1 .. Last_Item) = " ";
      DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                        (String_Item (1 .. Last_Item)),
                        Voice  => DoubleTalk.Paul);
   end loop;



   -- Loop through and test the double talk in all the different voices
   for Voice in DoubleTalk.Voice_Range'Range loop
      Ada.Text_IO.Put_Line ("Testing Phrase 'Train Starting' by " &
                              DoubleTalk.Voice_Range'Image (Voice));
      DoubleTalk.Speak (Phrase =>
                           DoubleTalk.Phrase_Strings.To_Bounded_String (
                            Source => "Train Starting"),
			Voice => Voice);
      delay 2.0;
   end loop;

   -- Loop through and test all the phrases without a delay
   for Voice in DoubleTalk.Voice_Range'Range loop
      Ada.Text_IO.Put_Line ("Testing Phrase 'Train Starting' by " &
                              DoubleTalk.Voice_Range'Image (Voice));
      DoubleTalk.Speak (Phrase =>
                           DoubleTalk.Phrase_Strings.To_Bounded_String (
                            Source => "Train Starting"),
			Voice => Voice);
   end loop;

   Ada.Text_IO.Put_Line ("Testing Two Voices at Once");
   DoubleTalk.Speak (Phrase =>  DoubleTalk.Phrase_Strings.To_Bounded_String (
                            Source => "Voice One"),
	 Voice => DoubleTalk.Vader);
   DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String (
                            Source => "Voice Two"),
                     Voice => DoubleTalk.Pete);
   delay 2.0;
   -- Make sure to set the bounded buffer size to 2 in order to produce correct
   -- results
   Ada.Text_IO.Put_Line ("Testing Bounded Buffer");
   DoubleTalk.Speak (Phrase =>  DoubleTalk.Phrase_Strings.To_Bounded_String (
                            Source => "Phrase One"),
	 Voice => DoubleTalk.Vader);
   DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String (
                            Source => "Phrase Two"),
                     Voice => DoubleTalk.Pete);
   DoubleTalk.Speak (Phrase =>  DoubleTalk.Phrase_Strings.To_Bounded_String (
                            Source => "Phrase Three"),
	 Voice => DoubleTalk.Vader);
   DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String (
                            Source => "Phrase Four"),
                     Voice => DoubleTalk.Pete);
   DoubleTalk.Speak (Phrase =>  DoubleTalk.Phrase_Strings.To_Bounded_String (
                            Source => "Phrase Five"),
	 Voice => DoubleTalk.Vader);
   DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String (
                            Source => "Phrase Six?"),
                     Voice => DoubleTalk.Pete);
end DoubleTalk_Test;
