--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullivan ------------------------------------------------
-- Tested  By : Kaleb Luse -----------------------------------------------------
-- Updated    : 20 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Layout;
with Trains;
with Cabs;
package Blocks is

   -- Purpose        : The purpose of this funtion is to supply power to a given
   --                  given block in the given direction
   -- Preconditions  : None
   -- Postconditions : Block is powered with the correct direction
   procedure Power_Block (Block     : in Layout.Block_ID;
                          Direction : in Layout.Block_Polarity;
                          Cab       : in Cabs.Cab_ID);

   -- Purpose        : The purpose of this funtion is to allow the train to
   --                  reserve a block it intends to occupy
   -- Preconditions  : Block is not already reserved
   -- Postconditions : Block is reserved to the requestor

   procedure Reserve (Block     : in  Layout.Block_ID;
                      Requestor : in  Trains.Request_ID;
                      Success   : out Boolean);

   -- Purpose        : The purpose of this funtion is to allow the trains
   --                  or the dispather to free a block reserved
   -- Preconditions  : Block is reserved by tain (Dispatcher main free any)
   -- Postconditions : Block is freed and available for reservation
   procedure Free (Block     : in Layout.Block_ID;
                   Requestor : in Trains.Request_ID);

   -- Purpose        : Checks whether a block is powered or not
   -- Preconditions  : None
   -- Postconditions : None
   function Is_Powered (Block : in Layout.Block_ID) return Boolean;

   -- Purpose        : Obtains the lenght of the block
   -- Preconditions  : None
   -- Postconditions : None
   function Length (Block : in Layout.Block_ID) return Natural;

   -- Purpose        : Prints current blocks to the Dispatcher's Display
   -- Preconditions  : None
   -- Postconditions : None
   procedure Print_Blocks;

end Blocks;
