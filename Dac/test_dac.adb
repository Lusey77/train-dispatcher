--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tests      : DAC ------------------------------------------------------------
-- Extra Note : This test requires a volt meter to test that the voltage -------
--              outputted is correct -------------------------------------------
-- Updated    : 9 Mar 15 -------------------------------------------------------
--------------------------------------------------------------------------------
with Ada.Text_IO;
with DAC;
with MaRTE_OS;
procedure Test_DAC is

   -----------------------------------------------------------------------------
   -- Represents the IO packages for channel number and volts                 --
   -----------------------------------------------------------------------------
   package Channel_IO is new Ada.Text_IO.Integer_IO (Num => DAC.Channel_Number);
   package Volts_IO   is new Ada.Text_IO.Fixed_IO (Num => DAC.Output_Volts);

   -----------------------------------------------------------------------------
   -- Represents the variables for the channel and votls                      --
   -----------------------------------------------------------------------------
   Channel : DAC.Channel_Number;
   Volts   : DAC.Output_Volts;

begin

   -----------------------------------------------------------------------------
   -- Tests the DAC package by getting channels and volts from the keyboard   --
   -----------------------------------------------------------------------------
   -- Each iteration, perform one test of the DAC                             --
   -----------------------------------------------------------------------------
   loop
      --------------------------------------------------------------------------
      -- Output request for channel and get the channel number from user      --
      --------------------------------------------------------------------------
      Ada.Text_IO.Put ("Input the channel you would like to use for your" &
                         "conversion(Number between 0 and 5): ");
      Channel_IO.Get (Channel);
      Ada.Text_IO.New_Line;
      --------------------------------------------------------------------------
      -- Output request for volts and get the volts from the user             --
      --------------------------------------------------------------------------
      Ada.Text_IO.Put ("Input the amount of volts you would like to output" &
                         "(Number between -5 and 5): ");
      Volts_IO.Get (Volts);
      Ada.Text_IO.New_Line;
      --------------------------------------------------------------------------
      -- Write the given volts to the given channel                           --
      --------------------------------------------------------------------------
      DAC.Write (Channel => Channel,
                 Value   => Volts);
   end loop;
end Test_DAC;
