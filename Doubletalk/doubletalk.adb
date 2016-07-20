--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse & Ethan Morisette -----------------------------------
-- Tested By  : Travis Sullivan ------------------------------------------------
-- Updated    : 12 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with MaRTE_OS;
with Bounded_Queue;
with Port_IO;
with Ada.Unchecked_Conversion;
use type Port_IO.Address_Range;

package body DoubleTalk is

   -- Record that stores phrase and the voice associated with the phrase
   type Phrase_Rec is
      record
         Voice_Command  : String (1 .. 2);
         Phrase 	: Phrase_Strings.Bounded_String;
      end record;

   -- Array of the voice command numbers (form: "nO")
   type Voice_Command_Array is array (Voice_Range) of String (1 .. 2);

   -- Array that holds the status information of the TTS port
   type Status_Array is array (Natural range 0 .. 7) of Boolean;
   for  Status_Array'Size use 8;
   for  Status_Array'Component_Size use 1;

   -- Instantiate a package for a bounded queue type
   package Bounded_Buffer_Queue is new Bounded_Queue (Phrase_Rec);

   -- A buffer that is bounded - stores Phrase_Rec
   protected type Bounded_Buffer (Max_Size : Positive) is
      -- Add a value to the buffer
      procedure Put (Item : in Phrase_Rec);
      -- Remove a value from the buffer
      entry Take (Item : out Phrase_Rec);
   private
      Buffer : Bounded_Buffer_Queue.Queue_Type (Max_Size => Max_Size);
   end Bounded_Buffer;

   protected body Bounded_Buffer is
      procedure Put (Item : in Phrase_Rec) is
         Trash : Phrase_Rec;
      begin
         -- Remove the oldest phrase in queue if it is full
         if Bounded_Buffer_Queue.Full (Queue => Buffer) then
            Bounded_Buffer_Queue.Dequeue (Queue => Buffer,
                                          Item  => Trash);
         end if;
         -- Add the phrase to the buffer
         Bounded_Buffer_Queue.Enqueue (Queue => Buffer,
                                       Item  => Item);
      end Put;

      entry Take (Item : out Phrase_Rec)
        when not Bounded_Buffer_Queue.Empty (Queue => Buffer) is
      begin
         -- Remove phrase from the buffer
         Bounded_Buffer_Queue.Dequeue (Queue => Buffer,
                                       Item  => Item);
      end Take;
   end Bounded_Buffer;

   -- The control code character - commands must be preceded by this
   Control_Code_Constant : constant Character := Character'Val (1);
   -- The end command character - commands must end in this
   End_Command_Constant  : constant Character := Character'Val (0);
   -- Base address for the DoubleTalk TTS Port
   Base 		 : constant Port_IO.Address_Range := 16#31F#;
   -- Table of numeric values for commands to change Voice_Range in DoubleTalk
   Voice_Command_Table 	 : constant Voice_Command_Array := ("0O", "1O", "2O",
                                                            "3O", "4O", "5O",
                                                            "6O", "7O");
   -- A buffer to store phrases to be sent to DoubleTalk
   Speech_Buffer	 : Bounded_Buffer (Max_Size => Buffer_Size);
   -- Register responsible for checking the status of DoubleTalk
   Status_Register 	 : Status_Array;
--------------------------------------------------------------------------------
   -- Converts a byte to an array
   function To_Array is new Ada.Unchecked_Conversion (Source => Port_IO.Byte,
                                                      Target => Status_Array);

   -- Sends a character to DoubleTalk
   procedure Send_Character (Char : in Character) is
      -- The ASCII value of the character being sent to DoubleTalk
      ASCII_Value   : Port_IO.Byte;
      -- Reperesents the amount of time between each status register check
      Max_Wait_Time : constant Duration := 0.01;
      -- Represents the variable signalling we are done with sending a character
      Done          : Boolean;
      -- Represents the amount of time doubletalk needs to send a character
      Max_Settling_Time : constant Duration := 10 * 1.0E-9;
   begin
      -- Convert Char to ASCII value
      ASCII_Value := Character'Pos (Char);
      -- Loop while DoubleTalk is not ready (polling)
      loop
         -- Check the TTS port for status update
         Status_Register := To_Array (Port_IO.In_Byte (Address => Base));
         delay Max_Wait_Time;
         -- The value in the fourth bit of the array will tell us if we are done
         Done := Status_Register (4);
         exit when Done;
      end loop;
      -- Write ASCII value to data register
      Port_IO.Out_Byte (Address => Base,
                        Data    => ASCII_Value);
      delay Max_Settling_Time;
   end Send_Character;

   -- Task for speech consumer
   task Speech_Consumer;

   task body Speech_Consumer is
      Consumer_Phrase : Phrase_Rec;
      Char  	      : Character;
   begin
      loop
         -- Take a phrase from the speech buffer
         Speech_Buffer.Take (Consumer_Phrase);
         -- Send control character
         Send_Character (Control_Code_Constant);
         -- Send voice command
         for Count in 1 .. 2 loop
            Send_Character (Consumer_Phrase.Voice_Command (Count));
         end loop;
         -- Loop through characters of the phrase
         for Count in 1 .. Phrase_Strings.Length (Consumer_Phrase.Phrase) loop
            -- Send character to DoubleTalk
            Char := Phrase_Strings.Element (Source => Consumer_Phrase.Phrase,
                                            Index  => Count);
            Send_Character (Char => Char);
         end loop;
         -- Send end command
         Send_Character (End_Command_Constant);
      end loop;
   end Speech_Consumer;

   -- Enqueue a phrase to be spoken to speech buffer
   procedure Speak (Phrase : in Phrase_Strings.Bounded_String;
                    Voice  : in Voice_Range) is
   begin
      -- Put voice command and phrase into speech buffer
      Speech_Buffer.Put (Item => (Voice_Command => Voice_Command_Table (Voice),
                                  Phrase        => Phrase));
   end Speak;
end DoubleTalk;
