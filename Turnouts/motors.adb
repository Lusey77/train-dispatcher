--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested By  : Travis Sullivan ------------------------------------------------
-- Updated    : 07 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Port_IO;
with Layout;
with Ada.Unchecked_Conversion;
use type Port_IO.Address_Range;
use type Layout.Turnout_ID;
package body Motors is

   -- Represents whethere the motor is in position
   type Position is (Not_In_Position, In_Position);

   -- Represents how in memory the enumeration values should be stored
   for Position use (Not_In_Position => 2#0#,
                     In_Position     => 2#1#);

   -- Represents an array of position characters to determine whether a motor
   -- is in the correct position
   type Sense_Port_Array is array (Integer range 0 .. 7) of Position;

   -- Represents an array of turn_choices to determine what position a motor is
   -- in and what the desired position is
   type Control_Port_Array is array (Integer range 0 .. 7)
     of Layout.Turn_Choice;

   -- Represets how large types should be stored in memory
   for Sense_Port_Array'Size use 8;
   for Control_Port_Array'Size use 8;
   for Sense_Port_Array'Component_Size use 1;
   for Control_Port_Array'Component_Size use 1;

   -- Represents functions to convert bytes into arrays and vice versa
   function To_Control_Port is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte, Target => Control_Port_Array);
   function To_Sense_Port is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte, Target => Sense_Port_Array);
   function To_Byte   is new Ada.Unchecked_Conversion
     (Source => Control_Port_Array, Target => Port_IO.Byte);

   -- Represents the base address of the first board
   Base : constant Port_IO.Address_Range := 16#220#;

   protected type Protected_Control_Port is
      procedure Change_Bit  (Address : in Port_IO.Address_Range;
                             Bit     : in Integer;
                             Data    : in Layout.Turn_Choice);
   private
      Port : Control_Port_Array;
   end Protected_Control_Port;

   protected body Protected_Control_Port is
      -- Change the direction of the proper motor in the IO_Port array
      procedure Change_Bit (Address : in Port_IO.Address_Range;
                            Bit     : in Integer;
                            Data    : in Layout.Turn_Choice) is
      begin
         -- Read from the address and store the values in IO_Port
         Port := To_Control_Port (Port_IO.In_Byte (Address));
         -- Change the direction of the proper motor in the IO_Port array
         Port (Bit) := Data;
         -- Write the updated array to memory
         Port_IO.Out_Byte (Address => Address,
                           Data    => To_Byte (Port));
      end Change_Bit;
   end Protected_Control_Port;

   procedure Set (Motor     : in Layout.Turnout_ID;
                  Direction : in Layout.Turn_Choice) is

      -- Represents the base address of the board the chip is on
      Board_Base    : constant Port_IO.Address_Range :=
        Port_IO.Address_Range ((Motor - 1) / 24 * 8) + Base;
      -- Represents a reference number for a motor to an address on the board
      Reference_Num : constant Integer := Integer ((Motor - 1) rem 24);
      -- Represents the bit value in the register to be written to
      Control_Bit   : constant Integer := Integer ((Motor - 1) rem 8);

      -- Represents the address for the correspnding motor
      Port_Address   : Port_IO.Address_Range;
      -- Represents the values that were in the register at the given address
      Control_Port   : Protected_Control_Port;

   begin

      -- Determine the control chip and port of the given motor number
      if Reference_Num in 0 .. 7 then
         -- If reference number is between 0 and 7 address is Port A High
         Port_Address := Board_Base + 4;
      elsif Reference_Num in 8 .. 15 then
         -- Elsf reference number is between 8 and 15 address is Port C High
         Port_Address := Board_Base + 6;
      else
         -- Else address is Port B Low
         Port_Address := Board_Base + 1;
      end if;

      -- Read from the port and change the direction of the proper motor in the
      -- IO_Port array and write it to the memory
      Control_Port.Change_Bit (Address => Port_Address,
                               Bit     => Control_Bit,
                               Data    => Direction);
   end Set;

   function In_Position (Motor : in Layout.Turnout_ID) return Boolean is

      -- Represents the base address of the board the chip is on
      Board_Base    : constant Port_IO.Address_Range :=
        Port_IO.Address_Range (Motor / 25 * 8) + Base;
      -- Represents a reference number for a motor to an address on the board
      Reference_Num : constant Integer := Integer ((Motor - 1) rem 24);
      -- Represents the bit value in the register to be written to
      Sense_Bit     : constant Integer := Integer ((Motor - 1) rem 8);

      -- Represents the address for the correspnding motor
      Port_Address  : Port_IO.Address_Range;
      -- Represents the values that were in the register at the given address
      Sense_Port    : Sense_Port_Array;

   begin

      -- Determine the control chip and port of the given motor number
      if Reference_Num in 0 .. 7 then
         -- If reference number is between 0 and 7 address is Port B High
         Port_Address := Board_Base + 5;
      elsif Reference_Num in 8 .. 15 then
         -- Elsif reference number is between 8 and 15 address is Port A Low
         Port_Address := Board_Base;
      else
         -- Else address is Port C Low
         Port_Address := Board_Base + 2;
      end if;

      -- Read the values at the corresponding address to the given motor
      Sense_Port := To_Sense_Port (Port_IO.In_Byte (Port_Address));

      -- Return the value of the bit at the given register
      return Sense_Port (Sense_Bit) = In_Position;

   end In_Position;

begin

   -- Initialize and configure registers accordingly
   -- Each iteration, initailize/configure one register
   for Address in 0 .. 15 loop
      -- Configure the registers based on their addresses
      if Address = 3 or Address = 11 then
         -- If the address is 3 or 11 data to be written to the port is 153
         -- 153 initializes the ports input and output correctly and can be
         -- found on page 9 of the CIO-DAS08/JR User's Manual
         Port_IO.Out_Byte (Address => Base + Port_IO.Address_Range (Address),
                           Data    => 153);
      elsif Address = 7 or Address = 15 then
         -- Elsif the address is 7 or 15 data to be written to the port is 130
         -- 130 initializes the ports input and output correctly and can be
         -- found on page 9 of the CIO-DAS08/JR User's Manual
         Port_IO.Out_Byte (Address => Base + Port_IO.Address_Range (Address),
                           Data    => 130);
      else
         -- Else data to be written to the port is 128
         -- 128 initializes the ports input and output correctly and can be
         -- found on page 9 of the CIO-DAS08/JR User's Manual
         Port_IO.Out_Byte (Address => Base + Port_IO.Address_Range (Address),
                           Data    => 128);
      end if;
   end loop;

end Motors;
