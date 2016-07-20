--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested  By : Kaleb Luse -----------------------------------------------------
-- Updated    : 2 Mar 15 -------------------------------------------------------
--------------------------------------------------------------------------------
with Common_Units;
use type Common_Units.Volts;

package ADC is

   -- Represents the range of channels on the CIO-DAS08/Jr board
   type Channel_Number is range 0 .. 7;

   -- Represents the range of volts the CIO-DAS08/Jr board can handle
   subtype Input_Volts is Common_Units.Volts range -5.0 .. 5.0;

   -- Preconditions  : None
   --
   -- Postconditions : The voltage on the given channel is returned
   --
   -- Purpose        : Performs an analog to digital conversion on the given
   --                  channel
   --
   -- Note           : This is a potentially blocking procedure
   procedure Read (Channel 	: in  Channel_Number;
                   Value 	: out Input_Volts);

private
   -- Represents the size of the integer channel_number in memory
   for Channel_Number'Size use 3;

end ADC;
