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

package DAC is

   -- Represents the range of channels available on our board
   type Channel_Number is range 0 .. 5;

   -- Represents the range of volts we can output on our board
   subtype Output_Volts is Common_Units.Volts range -5.0 .. 5.0;

   -- Represents an exception if the given volts exceeds the limits
   Over_Range : exception;

   -- Preconditions  : None
   --
   -- Postconditions : The proper voltage is set on the given channel
   --
   -- Purpose        : The purpose of this procedure is to take a given volts
   --                  and channel and convert it into a analog voltage
   procedure Write (Channel 	: in Channel_Number;
                    Value 	: in Output_Volts);

end DAC;
