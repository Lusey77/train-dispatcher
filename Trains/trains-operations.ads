--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Ethan Morisette & Kaleb Luse -----------------------------------
-- Tested By  : Travis Sullivan ------------------------------------------------
-- Updated    : 27 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Common_Units;
with Locomotives;
with Layout.Search;
package Trains.Operations is

   -- initializes train knowledge of location
   -- Reserve blocks, etc
   procedure Initialize_Train (Train       : in  Train_ID;
                               My_Blocks   : in  Layout.Block_List;
                               My_Turnouts : in  Layout.Search.Turnout_List;
                               Loco        : in  Locomotives.Loco_Rec;
                               Successful  : out Boolean);

   procedure Release_Train (Train : in Train_ID);
   -- Preconditions: none
   -- Postconditions: train is enabled
   -- Purpose: enables the train

   procedure Hold_Train (Train : in Train_ID);
   -- Preconditions: none
   -- Postconditions: train is disabled
   -- Purpose: disables the train

   procedure Stop_Train (Train : in Train_ID);
   -- Preconditions: none
   -- Postconditions: train is locked by dispatcher
   -- Purpose: locks the train through dispatcher or engineer

   procedure Dispatcher_Unlock (Train : in Train_ID);
   -- Preconditions: none
   -- Postconditions: train is not locked by dispatcher (may still be locked)
   -- Purpose: unlocks the train through dispatcher

   procedure Lost_Rolling_Stock (Train : in Train_ID);
   -- Preconditions: none
   -- Postconditions: train is locked by dispatcher and system
   -- Purpose: locks the train through system and dispatcher

   procedure Retrieved_Stock (Train : in Train_ID);
   -- Preconditions: none
   -- Postconditions: train is not locked by system (may still be locked)
   -- Purpose: unlocks the train through system

   procedure Turnout_Failed (Train   : in Request_ID;
                             Turnout : in Layout.Turnout_ID);
   -- Preconditions: none
   -- Postconditions: train is locked by turnout
   -- Purpose: locks the train through turnout

   procedure Turnout_Fixed (Train   : in Request_ID;
                            Turnout : in Layout.Turnout_ID);
   -- Preconditions: none
   -- Postconditions: train is not locked by turnout (may still be locked)
   -- Purpose: unlocks the train through turnout

   procedure Block_Reserved (Train : in Train_ID;
                             Block : in Layout.Block_ID);
   -- Preconditions: none
   -- Postconditions: train is locked by block
   -- Purpose: locks the train through block

   procedure Block_Freed (Train : in Request_ID;
                          Block : in Layout.Block_ID);
   -- Preconditions: none
   -- Postconditions: train is not locked by block (may still be locked)
   -- Purpose: unlocks the train through block

   procedure Set_Backward (Train : in Train_ID);
   -- Preconditions: none
   -- Postconditions: train is in backward
   -- Purpose: change the train's direction to backward

   procedure Set_Forward (Train : in Train_ID);
   -- Preconditions: none
   -- Postconditions: train is in forward
   -- Purpose: change the train's direction to forward

   -- change the next choice turnout of the train
   procedure Change_Turnout (Train     : in Train_ID;
                             Direction : in Layout.Turn_Choice);

   -- sound the trains horn
   procedure Sound_Horn (Train  : in Train_ID);

   function Get_Min_Throttle (Train : in Train_ID) return Common_Units.Percent;

   procedure Set_Speed (Train : in Train_ID;
                        Speed : in Common_Units.Percent);

   function Get_Blocks (Train : in Train_ID) --return Layout.Block_Array;
                        return Layout.Block_List;

   procedure Hall_Triggered (Hall : in Layout.Hall_ID);

   function Get_Loco (Train : in Train_ID) return Locomotives.Loco_Rec;

   function Get_Direction (Train : in Train_ID) return Direction_Type;

   procedure Restart;

end Trains.Operations;
