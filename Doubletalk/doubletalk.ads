with Ada.Strings.Bounded;

package DoubleTalk is

-- This package provides access to the text to speech synthesizer

-- Written by John McCormick, February 2001
-- Revised, March 2011

   -- The different voices available on the synthesizer.
   type Voice_Range is (Paul, Vader, Bob, Pete, Randy, Biff, Skip, Robo);

   -- The maximum length of a phrase to be spoken
   Max_Phrase_Size : constant := 80;

   -- A bounded-length string class for phrases
   package Phrase_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
                                 (Max => Max_Phrase_Size);

   -- The maximum number of phrases that Doubletalk will buffer
   Buffer_Size : constant := 40;

   ----------------------------------------------------------------------------
   procedure Speak (Phrase : in Phrase_Strings.Bounded_String;
                    Voice  : in Voice_Range);
   -- Speak the Phrase in given voice
   --
   -- This procedure is non-blocking
   --
   -- Preconditions  : none
   --
   -- Postconditions : Double Talk will speak Phrase using the given Voice.
   --                  If the Doubletalk buffer is full, the oldest
   --                     phrase will be removed from the buffer to
   --                     make room for this Phrase.


end DoubleTalk;
