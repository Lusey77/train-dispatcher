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
with System;
with Ada.Unchecked_Conversion;
use type Port_IO.Word;
use type Port_IO.Address_Range;

package body ADC is

   -- Creates a semaphore type to protect the conversion from being accessed
   -- by more than one task at a time
   protected type Semaphore (Initial_Value : Natural) is
      procedure Signal;
      entry Wait;
   private
      Count : Natural := Initial_Value;
   end Semaphore;

   -- Represents the body of the protected type semaphore
   protected body Semaphore is
      procedure Signal is
      begin
         Count := Count + 1;
      end Signal;

      entry Wait when Count > 0 is
      begin
         Count := Count - 1;
      end Wait;
   end Semaphore;

   -- Variable to declare the semaphore type
   My_Semaphore : Semaphore (Initial_Value => 1);

   -- Record for the control status register
   type CSR_Rec is
   record
      Channel 	: Channel_Number;
      Busy 	: Boolean;
   end record;

   -- Represents where in the register to store each bit
   for CSR_Rec use
      record
         Channel     at 0 range 0 .. 2;
         Busy	     at 0 range 7 .. 7;
      end record;

   -- Represents how much memory the CSR rec should take up
   for CSR_Rec'Size use 8;

   -- Specify the endian of the system
   for CSR_Rec'Bit_Order use System.Low_Order_First;

   -- Function to convert a CSR rec into a byte
   function To_Byte is new Ada.Unchecked_Conversion (Source => CSR_Rec,
                                                     Target => Port_IO.Byte);
   -- Function to convert a byte into a CSR rec
   function To_Rec is new Ada.Unchecked_Conversion (Source => Port_IO.Byte,
                                                    Target => CSR_Rec);

   -- Represents the address of the board
   Base	: constant Port_IO.Address_Range := 16#260#;

   -- Represents max conversion time
   Max_Conversion_Time : constant Duration := 25.0E-6;

   procedure Read (Channel	: in     Channel_Number;
                   Value	:    out Input_Volts) is

      -- Represents the control status register for the conversion
      Control_Status_Register : CSR_Rec;

      -- Represents the output of the conversion
      Data_Register           : Port_IO.Word;

   begin

      -- Wait for the resource to become available
      My_Semaphore.Wait;

      -- Performs analog to digital conversions of a given value
      -- Each iteration, perform one conversion
      for Count in 1 .. 2 loop
         -- Specify the channel in which to obtain the value for the conversion
         Control_Status_Register.Channel := Channel;

         -- Write the channel out to memory
         Port_IO.Out_Byte (Address => (Base + 2),
                           Data    => To_Byte (Control_Status_Register));

         -- Start the conversion
         Port_IO.Out_Byte (Address => (Base + 1),
                           Data 	=> 0);

         -- Loops until the conversion is complete or max time reached
         -- Each iteration, check to see if the conversion is complete
         loop
            delay Max_Conversion_Time / 25;
            Control_Status_Register := To_Rec
              (Port_IO.In_Byte (Address => (Base + 2)));
            exit when not Control_Status_Register.Busy;
         end loop;

         -- Convert the data into the voltage
         Data_Register := Port_IO.In_Word (Base);

         -- Mask out lower 4 bits
         Data_Register := Data_Register / 16;

         -- Convert the word into a voltage value
         Value := 10 * Common_Units.Volts (Data_Register) / 4095 - 5.0;
      end loop;

      -- Signal that the resource is available again
      My_Semaphore.Signal;
   end Read;

end ADC;

