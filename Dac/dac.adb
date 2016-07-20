--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested  By : Kaleb Luse -----------------------------------------------------
-- Updated    : 2 Mar 15 -------------------------------------------------------
--------------------------------------------------------------------------------
with Port_IO;
use type Port_IO.Address_Range;

package body DAC is

   -- Represents the base address of the board
   Base	: constant Port_IO.Address_Range := 16#240#;

   -----------
   -- Write --
   -----------

   procedure Write (Channel : in Channel_Number;
                    Value   : in Output_Volts) is
      -- Represents the address of where we will store the value to be converted
      Register_Address       : Port_IO.Address_Range;

   begin
      -- Specify where we will store our value based on the channel passed in
      Register_Address := Base + Port_IO.Address_Range (Channel * 2);

      -- Write the value to be converted
      Port_IO.Out_Word (Address => Register_Address,
                        Data => Port_IO.Word (((Value + 5.0) / 10) * 4095));
   end Write;

begin
   -- Initialize all the channels to 0 volts
   for Channel in Channel_Number loop
      Write (Channel => Channel,
             Value   => 0.0);
   end loop;

end DAC;
