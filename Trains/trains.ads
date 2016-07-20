--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Ethan Morisette & Kaleb Luse -----------------------------------
-- Tested By  : Travis Sullivan ------------------------------------------------
-- Updated    : 27 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Layout;
package Trains is

   -- This package provides the basic types for trains.
   --
   -- Written by John McCormick, April 2002 as a single package.
   -- Broken into parent and child packages to separate the basic types
   -- from the operations in February 2008.

   -- IDs for the trains.
   -- Zero is reserved for dispatcher making requests for an engineer
   Max_Trains : constant := 3;
   type    Request_ID is            range 0 .. Max_Trains;
   subtype Train_ID   is Request_ID range 1 .. Max_Trains;

   Dispatcher : constant Request_ID := 0;

   -- the possible ability a train can be in determined by the dispatcher
   type Ability_Type is (Disabled, Enabled);
   -- the possible status of the train
   type Status_Type is (Unlocked, Locked);
   type Direction_Type is (Forward, Backward);   -- Direction of train travel

   Max_Length_Start   : constant := 3;   -- Max number of blocks beneath a train
   Max_Length_Running : constant := 5;   -- at startup and running

------------------------------------------------------------------------------

   -- Types for keeping track of train stops

   type Stop_Reason is (Dispatcher_Request,  Turnout_Failure,
                        Reservation_Failure, Lost_Caboose);
   type Stop_Set    is array (Stop_Reason) of Boolean;
   type Turnout_Set is array (Layout.Turnout_ID) of Boolean;
   type Stop_Rec is
      record
         Reasons   : Stop_Set;
         Block     : Layout.Block_ID;
         Turnouts  : Turnout_Set;
      end record;

--     type Initializing_Fail_Type is (Success, Loco_Conflict, Block_Conflict);

end Trains;
