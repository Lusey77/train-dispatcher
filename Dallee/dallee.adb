--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested  By : Travis Sullivan ------------------------------------------------
-- Updated    : 2 Mar 15 -------------------------------------------------------
--------------------------------------------------------------------------------
with Port_IO;
use type Port_IO.Address_Range;
with Ada.Unchecked_Conversion;
package body Dallee is

   -----------------------------------------------------------------------------
   -- Represents the two types of sounds the dallee horn makes                --
   -----------------------------------------------------------------------------
   type Sound_Unit is (Horn, Bell);

   -----------------------------------------------------------------------------
   -- Represents the states of the 4 horns and 4 bells                        --
   -----------------------------------------------------------------------------
   type Dallee_Array is array (Dallee_Num, Sound_Unit) of Off_On;

   -----------------------------------------------------------------------------
   -- Represents how much space in memory the components of the array take up --
   -----------------------------------------------------------------------------
   for Dallee_Array'Component_Size use 1;

   -----------------------------------------------------------------------------
   -- Represents how much space in memory the array should take up            --
   -----------------------------------------------------------------------------
   for Dallee_Array'Size use 8;

   -----------------------------------------------------------------------------
   -- Function that will convert a dallee array into a byte                   --
   -----------------------------------------------------------------------------
   function To_Byte is new Ada.Unchecked_Conversion (Source => Dallee_Array,
                                                     Target => Port_IO.Byte);

   -----------------------------------------------------------------------------
   -- Represnets the address the register used to turn on the sounds          --
   -----------------------------------------------------------------------------
   Dallee_Address : constant Port_IO.Address_Range := 16#260# + 3;

   -----------------------------------------------------------------------------
   -- Represents the protected object that will hold the states of the 4 horns--
   -- and bells                                                               --
   -----------------------------------------------------------------------------
   protected Dallee_Operations is
      procedure Sound (Dallee : in Dallee_Num;
                       Sound  : in Sound_Unit;
                       Power  : in Off_On);
   private
      Dallee_Unit : Dallee_Array;
   end Dallee_Operations;

   protected body Dallee_Operations is
      procedure Sound (Dallee : in Dallee_Num;
                       Sound  : in Sound_Unit;
                       Power  : in Off_On) is
      begin

         -- Change the bit to power the unit and write it to memory
         Dallee_Unit (Dallee, Sound) := Power;
         Port_IO.Out_Byte (Address => Dallee_Address,
                           Data    => To_Byte (Dallee_Unit));

      end Sound;
   end Dallee_Operations;

   --------------------
   -- Sound_Air_Horn --
   --------------------

   procedure Sound_Air_Horn (Dallee : in Dallee_Num;
                             Power  : in Off_On) is

   begin
      --------------------------------------------------------------------------
      -- Assign the proper value to the proper array index and write it to    --
      -- memory                                                               --
      --------------------------------------------------------------------------
      Dallee_Operations.Sound (Dallee => Dallee,
                               Sound  => Horn,
                               Power  => Power);
   end Sound_Air_Horn;

   ----------------
   -- Sound_Bell --
   ----------------

   procedure Sound_Bell (Dallee : in Dallee_Num;
                         Power  : in Off_On) is

   begin
      --------------------------------------------------------------------------
      -- Assign the proper value to the proper array index and write it to    --
      -- memory                                                               --
      --------------------------------------------------------------------------
      Dallee_Operations.Sound (Dallee => Dallee,
                               Sound  => Bell,
                               Power  => Power);
   end Sound_Bell;

begin
   -----------------------------------------------------------------------------
   -- Instantiate all the horns and bells to off                              --
   -----------------------------------------------------------------------------
   Port_IO.Out_Byte (Address => Dallee_Address,
                     Data    => 0);
end Dallee;
