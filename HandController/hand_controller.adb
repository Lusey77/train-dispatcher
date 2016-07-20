--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested  By : Kaleb Luse -----------------------------------------------------
-- Updated    : 17 Feb 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Port_IO;
with Ada.Unchecked_Conversion;
with ADC;
with Common_Units;
use type Port_IO.Address_Range;
use type Common_Units.Volts;

package body Hand_Controller is

   -- Represents the components of a hand controller
   type Controller_Rec is
      record
         Red_Button       : Button_State;
         Black_Button     : Button_State;
         Two_Way_Switch   : Switch_State;
         Three_Way_Toggle : Toggle_State;
      end record;

   -- Represents how the components of the hand controller should be stored
   for Controller_Rec use
      record
         Red_Button       at 0 range 0 .. 0;
         Black_Button     at 0 range 1 .. 1;
         Two_Way_Switch   at 0 range 2 .. 2;
         Three_Way_Toggle at 0 range 3 .. 4;
   end record;

   -- Represents how much space the components of hand controller should take up
   for Controller_Rec'Size use 8;

   -- Instantiate a package for unchecked conversions to a record type
   function To_Rec is new Ada.Unchecked_Conversion (Source => Port_IO.Byte,
                                                    Target => Controller_Rec);

   -- Describes the base address for the port_io
   Base : constant Port_IO.Address_Range := 16#240#;

   --
   type Address_Array is array (H_Controller) of Port_IO.Address_Range;

   --
   type Channel_Array is array (H_Controller) of ADC.Channel_Number;

   --------------------------
   -- Get_Controller_State --
   --------------------------

   procedure Get_Controller_State (Controller       : in  H_Controller;
                                   Red_Button       : out Button_State;
                                   Black_Button     : out Button_State;
                                   Two_Way_Switch   : out Switch_State;
                                   Three_Way_Toggle : out Toggle_State) is

      -- Represents the hand controller address
      Hand_Controller_Address : constant Address_Array := (12, 13, 14);
      -- Represents the piece of data to be converted to the record
      Controller_Byte         : Port_IO.Byte;
      -- Represents the hand controller record
      H_Controller_Rec        : Controller_Rec;

   begin
      --------------------------------------------------------------------------
      -- Read in the input from the hand controller using port_io             --
      --------------------------------------------------------------------------
      Controller_Byte := Port_IO.In_Byte
        (Address => Base + Hand_Controller_Address (Controller));

      --------------------------------------------------------------------------
      -- Use the unchecked conversion to convert the byte to a record         --
      --------------------------------------------------------------------------
      H_Controller_Rec := To_Rec (Controller_Byte);

      --------------------------------------------------------------------------
      -- Add the state of the record to the out variables                     --
      --------------------------------------------------------------------------
      Red_Button       := H_Controller_Rec.Red_Button;
      Black_Button     := H_Controller_Rec.Black_Button;
      Two_Way_Switch   := H_Controller_Rec.Two_Way_Switch;
      Three_Way_Toggle := H_Controller_Rec.Three_Way_Toggle;

   end Get_Controller_State;

   --------------------
   -- Get_Black_Knob --
   --------------------

   procedure Get_Black_Knob (Controller : in  H_Controller;
                             Black_Knob : out Common_Units.Percent) is

      -- Represents the lookup table for channels based on the hand controller
      Hand_Controller_Channel : constant Channel_Array := (0, 1, 2);

      -- Represents the volts read in from the ADC
      Volts : ADC.Input_Volts;

   begin
      --------------------------------------------------------------------------
      -- Read from the proper channel based on the given hand controller      --
      --------------------------------------------------------------------------
      ADC.Read (Channel => Hand_Controller_Channel (Controller),
                Value   => Volts);

      --------------------------------------------------------------------------
      -- Check for constraint error condition for when elctromagnetic waves   --
      -- cause fluctuations in the volts and result in volts dropping below 0 --
      --------------------------------------------------------------------------
      if Volts < 0.0 then
         -----------------------------------------------------------------------
         -- If volts is less than 0 then 0 % throttle                         --
         -----------------------------------------------------------------------
         Black_Knob := 0.00;
      else
         -----------------------------------------------------------------------
         -- Else convert the volts to a percent                               --
         -----------------------------------------------------------------------
         Black_Knob := 20.0 * Common_Units.Percent (Volts);
      end if;
   end Get_Black_Knob;
end Hand_Controller;
