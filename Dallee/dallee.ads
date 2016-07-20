--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested  By : Travis Sullivan ------------------------------------------------
-- Updated    : 2 Mar 15 -------------------------------------------------------
--------------------------------------------------------------------------------
package Dallee is

   -----------------------------------------------------------------------------
   -- Type for specifying whether a device should be powered on or off        --
   -----------------------------------------------------------------------------
   type Off_On is (Off, On);

   -----------------------------------------------------------------------------
   -- Represents the number of Dallee sound units installed on the interface  --
   -- board                                                                   --
   -----------------------------------------------------------------------------
   type Dallee_Num is range 1 .. 4;

   -- Procedure for sounding the air horn
   --
   -- Preconditions  : None
   --
   -- Postconditions : Airhorn is turned on or off
   procedure Sound_Air_Horn (Dallee : in Dallee_Num;
                             Power  : in Off_On);

   -- Procedure for sounding the bell
   --
   -- Preconditions  : None
   --
   -- Postconditions : Bell is turned on or off
   procedure Sound_Bell (Dallee : in Dallee_Num;
                         Power  : in Off_On);
private

   -----------------------------------------------------------------------------
   -- Describes how in memory to store the state of the sounds                --
   -----------------------------------------------------------------------------
   for Off_On use (Off => 2#0#,
                   On  => 2#1#);

end Dallee;
