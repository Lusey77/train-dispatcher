with Layout;
with Trains;
with Ada.Real_Time; use Ada.Real_Time;
package Turnouts is

   -- This package provides operations for the turnouts

   -- Written by John W. McCormick, April 2001, modified May 2008
   -- Callback setting added March 2011

   -- Maximum time a turnout can take to complete a change of direction
   Time_Limit : constant Time_Span := To_Time_Span (4.0);  -- seconds

   type Turnout_State_Type is (Fully_Left, Fully_Right, Moving_Left,
                               Moving_Right);

   -- Turnout status
   type Status_Rec is
      record
         Desired : Layout.Turn_Choice;  -- Requested direction
         Current : Turnout_State_Type;
      end record;

   -----------------------------------------------------------------------------
   type Failure_Ptr is access procedure (Requestor : in Trains.Request_ID;
                                         Turnout   : in Layout.Turnout_ID);
   procedure Set_Failure_Callback (To : in Failure_Ptr);

   -- This procedure sets the procedure that the Turnout package will call
   --   when a turnout fails
   -- If no callback is set, no call is made when a turnout fails
   -- Calling this procedure with a null value for To removes any previous
   --   setting of the callback procedure for a failed turnout

   -----------------------------------------------------------------------------
   type Recover_Ptr is access procedure (Requestor : in Trains.Request_ID;
                                         Turnout   : in Layout.Turnout_ID);
   procedure Set_Recovery_Callback (To : in Recover_Ptr);

   -- This procedure sets the procedure that the Turnout package will call
   --   when a turnout successfully recovers from failure mode
   -- If no callback is set, no call is made when a turnout recovers
   -- Calling this procedure with a null value for To removes any previous
   --   setting of the callback procedure for a recovered turnout

   -----------------------------------------------------------------------------
   type Change_Ptr is access procedure (Turnout   : in Layout.Turnout_ID;
                                        Direction : in Layout.Turn_Choice;
                                        Moving    : in Boolean);
   procedure Set_Change_Callback (To : in Change_Ptr);

   -- This procedure sets the procedure that the Turnout package will call
   --   when a turnout changes its state
   -- If no callback is set, no call is made when a turnout changes state
   -- Calling this procedure with a null value for To removes any previous
   --   setting of the callback procedure for a changed state

   -----------------------------------------------------------------------------
   procedure Set (Requestor : in Trains.Request_ID;
                  Turnout   : in Layout.Turnout_ID;
                  Direction : in Layout.Turn_Choice);
   -- Set the direction of a turnout
   --
   -- Potentially blocking (rendezvous with a turnout task)
   --
   -- Preconditions  : None
   --
   -- Postconditions : An attempt is made to set Turnout to the given Direction.
   --
   --                  With every change of the turnout's state, the callback
   --                  procedure set by Set_Change_Callback is called.
   --
   --                  If the turnout does not change within the Time_Limit,
   --                     the callback procedure set by Set_Failure_Callback
   --                     is called.
   --
   --                     When the failed turnout succesfully reaches the
   --                     desired direction, the callback procedure set by
   --                     Set_Recovery_Callback is called.

   -----------------------------------------------------------------------------
   function Status (Turnout : in  Layout.Turnout_ID) return Status_Rec;
   -- Returns the status of a turnout
   --
   -- Non blocking
   --
   -- Preconditions  : none
   --

   -----------------------------------------------------------------------------
   function Direction_Of (Turnout : in Layout.Turnout_ID)
                                                     return Layout.Turn_Choice;
   -- Returns the direction to which the turnout was last set
   -- (the desired direction)
   --
   -- Non blocking
   --
   -- Preconditions : None

   -----------------------------------------------------------------------------
   procedure Shut_Down;
   -- Sets all turnouts to their power up direction.  Only 5 turnouts are
   -- changed simultaneously with a Time_Limit seconds delay between groups.
   -- Thus, this procedure may takes almost 30 seconds to complete.
   --
   -- Preconditions  : none
   --
   -- Postconditions : All Turnouts are set to the left

end Turnouts;
