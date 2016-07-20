--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested By  : Ethan Morisette ------------------------------------------------
-- Updated    : 07 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Motors;
with Layout;
use type Layout.Turn_Choice;
use type Layout.Turnout_ID;
with Ada.Text_IO;
package body Turnouts is

   Failure_Process : Failure_Ptr;

   Recovery_Process : Recover_Ptr;

   Change_Process   : Change_Ptr;

   type Status_State_Type is (No_Error, Error);

   type Turnout_Status_Array is array (Layout.Turnout_ID) of Status_Rec;

   Turnouts_Status : Turnout_Status_Array;

   -- Represents the protected turnout states
   protected type Turnout_State is
      procedure Set_Desired (Turnout   : in Layout.Turnout_ID;
                             Direction : in Layout.Turn_Choice);

      procedure Set_State (Turnout : in Layout.Turnout_ID;
                           State   : in Turnout_State_Type);

   end Turnout_State;

   protected body Turnout_State is
      procedure Set_Desired (Turnout   : in Layout.Turnout_ID;
                             Direction : in Layout.Turn_Choice) is
      begin
         Turnouts_Status (Turnout).Desired := Direction;
      end Set_Desired;

      procedure Set_State (Turnout : in Layout.Turnout_ID;
                           State   : in Turnout_State_Type) is
      begin
         Turnouts_Status (Turnout).Current := State;
      end Set_State;
   end Turnout_State;

   Turnouts : Turnout_State;

   -- Turnotus tasks
   task type Control is
      entry Assign_Turnout (Turnout_Num : Layout.Turnout_ID);
      entry Move_Right (Requestor : in Trains.Request_ID);
      entry Move_Left (Requestor : in Trains.Request_ID);
   end Control;

   task body Control is

      -- Represents the status of the turnout
      My_Status    : Status_State_Type := No_Error;
      -- Represents the ID of the turnout
      My_ID        : Layout.Turnout_ID := 1;
      -- Represents the requestor ID
      My_Requestor : Trains.Request_ID;
      -- Represents when the turnout is finished moving
      Done         : Ada.Real_Time.Time;

   begin
      -- Accept the turnout ID
      accept Assign_Turnout (Turnout_Num : Layout.Turnout_ID) do
         My_ID := Turnout_Num;
      end Assign_Turnout;
      loop
         case Turnouts_Status (My_ID).Current is
            when Fully_Left =>
               select
                  accept Move_Left (Requestor : in Trains.Request_ID) do
                     -- Set the desired position of the turnout to the left
                     Turnouts.Set_Desired (Turnout   => My_ID,
                                           Direction => Layout.Left);
                     My_Requestor := Requestor;
                  end Move_Left;
               or
                  accept Move_Right (Requestor : in Trains.Request_ID) do
                     -- Set the desired position of the turnout to the right
                     Turnouts.Set_Desired (Turnout   => My_ID,
                                           Direction => Layout.Right);
                     My_Requestor := Requestor;
                  end Move_Right;
                  -- Set the state of the turnout to moving right
                  Turnouts.Set_State (Turnout => My_ID,
                                      State   => Moving_Right);
                  -- Move the turnout to the Right
                  Motors.Set (Motor     => My_ID,
                              Direction => Layout.Right);
                  -- If there exists a pointer call it
                  if Change_Process /= null then
                     Change_Process.all (Turnout   => My_ID,
                                         Direction => Layout.Right,
                                         Moving    => True);
                  end if;
                  -- Start the timer
                  Done := Ada.Real_Time.Clock + Time_Limit;
               end select;
            when Fully_Right =>
               select
                  accept Move_Left (Requestor : in Trains.Request_ID) do
                     -- Set the desired position of the turnout to the left
                     Turnouts.Set_Desired (Turnout   => My_ID,
                                           Direction => Layout.Left);
                     My_Requestor := Requestor;
                  end Move_Left;
                  -- Set the state of the turnout to moving
                  Turnouts.Set_State (Turnout => My_ID,
                                      State   => Moving_Left);
                  -- Move the turnout to the left
                  Motors.Set (Motor     => My_ID,
                              Direction => Layout.Left);
                  -- If there exists a pointer call it
                  if Change_Process /= null then
                     Change_Process.all (Turnout   => My_ID,
                                         Direction => Layout.Left,
                                         Moving    => True);
                  end if;
                  -- Start the timer
                  Done := Ada.Real_Time.Clock + Time_Limit;
               or
                  accept Move_Right (Requestor : in Trains.Request_ID) do
                     -- Set the desired position of the turnout to the right
                     Turnouts.Set_Desired (Turnout   => My_ID,
                                           Direction => Layout.Right);
                     My_Requestor := Requestor;
                  end Move_Right;
               end select;
            when Moving_Left =>
               select
                  accept Move_Left (Requestor : in Trains.Request_ID) do
                     -- Set the desired position of the turnout to the left
                     Turnouts.Set_Desired (Turnout   => My_ID,
                                           Direction => Layout.Left);
                     My_Requestor := Requestor;
                  end Move_Left;
               or
                  accept Move_Right (Requestor : in Trains.Request_ID) do
                     -- Set the desired position of the turnout to the right
                     Turnouts.Set_Desired (Turnout   => My_ID,
                                           Direction => Layout.Right);
                     My_Requestor := Requestor;
                  end Move_Right;
                  -- Set the state to moving right
                  Turnouts.Set_State (Turnout => My_ID,
                                      State   => Moving_Right);
                                    -- Move the turnout to the left
                  -- move the turnout to the right
                  Motors.Set (Motor     => My_ID,
                              Direction => Layout.Right);
                  -- If there exists a pointer call it
                  if Change_Process /= null then
                     Change_Process.all (Turnout   => My_ID,
                                         Direction => Layout.Right,
                                         Moving    => True);
                  end if;
                  -- Restart the timer
                  Done := Ada.Real_Time.Clock + Time_Limit;
               or
                  -- wait until done
                  delay until Done;
                  if not Motors.In_Position (Motor => My_ID) or
                    Turnouts_Status (My_ID).Desired = Layout.Right then
                     -- If the turnout isn't in position change the error state
                     -- and call the failure process if one exists
                     if My_Status = No_Error then
                        My_Status := Error;
                        if Failure_Process /= null then
                           Failure_Process.all (Requestor => My_Requestor,
                                                Turnout   => My_ID);
                        end if;
                     end if;
                     -- Set the state of the tunrout to moving right
                     Turnouts.Set_State (Turnout => My_ID,
                                         State   => Moving_Right);
                     -- Turn the turnoout to the right
                     Motors.Set (Motor     => My_ID,
                                 Direction => Layout.Right);
                     -- Restart the timer
                     Done := Ada.Real_Time.Clock + Time_Limit;
                  else
                     if My_Status = Error then
                        -- If the turnout has recovered call the recovery proces
                        My_Status := No_Error;
                        if Recovery_Process /= null then
                           Recovery_Process.all (Requestor => My_Requestor,
                                                 Turnout   => My_ID);
                        end if;
                     end if;
                     if Change_Process /= null then
                        -- Call the change process and tell it the turnout isn't
                        -- moving
                        Change_Process.all (Turnout   => My_ID,
                                            Direction => Layout.Left,
                                            Moving    => False);
                     end if;
                     -- Set the state of the turnout to fully left
                     Turnouts.Set_State (Turnout => My_ID,
                                         State   => Fully_Left);
                  end if;
               end select;
            when Moving_Right =>
               select
                  accept Move_Left (Requestor : in Trains.Request_ID) do
                     -- Set the desired position of the turnout to the left
                     Turnouts.Set_Desired (Turnout   => My_ID,
                                           Direction => Layout.Left);
                     My_Requestor := Requestor;
                  end Move_Left;
                  -- Set the state of the turnout to moving left
                  Turnouts.Set_State (Turnout => My_ID,
                                      State   => Moving_Left);
                  -- Turnout the turnout to the left
                  Motors.Set (Motor     => My_ID,
                              Direction => Layout.Left);
                  -- If a change process exists call it
                  if Change_Process /= null then
                     Change_Process.all (Turnout   => My_ID,
                                     Direction => Layout.Left,
                                     Moving    => True);
                  end if;
                  -- restart the timer
                  Done := Ada.Real_Time.Clock + Time_Limit;
               or
                  accept Move_Right (Requestor : in Trains.Request_ID) do
                     -- Set the desired postion of the turnout to the right
                     Turnouts.Set_Desired (Turnout   => My_ID,
                                           Direction => Layout.Right);
                     My_Requestor := Requestor;
                  end Move_Right;
               or
                  -- wait until the turnout is done
                  delay until Done;
                  if not Motors.In_Position (Motor => My_ID) or
                    Turnouts_Status (My_ID).Desired = Layout.Left then
                     -- If the turnout isn't in position change the error state
                     -- and call the failure process if one exists
                     if My_Status = No_Error then
                        My_Status := Error;
                        if Failure_Process /= null then
                           Failure_Process.all (Requestor => My_Requestor,
                                                Turnout   => My_ID);
                        end if;
                     end if;
                     -- Set the state of the turnout to tmoving left
                     Turnouts.Set_State (Turnout => My_ID,
                                         State   => Moving_Left);
                     -- Set the turnout to the left
                     Motors.Set (Motor     => My_ID,
                                 Direction => Layout.Left);
                     -- Restart the timer
                     Done := Ada.Real_Time.Clock + Time_Limit;
                  else
                     if My_Status = Error then
                        -- If the turnout has recovered call the recovery proces
                        My_Status := No_Error;
                        if Recovery_Process /= null then
                           Recovery_Process.all (Requestor => My_Requestor,
                                                 Turnout   => My_ID);
                        end if;
                     end if;
                     if Change_Process /= null then
                        -- Call the change process and tell it the turnout isn't
                        -- moving
                        Change_Process.all (Turnout   => My_ID,
                                            Direction => Layout.Right,
                                            Moving    => False);
                     end if;
                     -- Set the state of the turnout to fully right
                     Turnouts.Set_State (Turnout => My_ID,
                                         State   => Fully_Right);
                  end if;
               end select;
         end case;
      end loop;
   end Control;

   type Task_Array is array (Layout.Turnout_ID) of Control;

   Turnout_Tasks : Task_Array;

   --------------------------
   -- Set_Failure_Callback --
   --------------------------

   procedure Set_Failure_Callback (To : in Failure_Ptr) is
   begin
      Failure_Process := To;
   end Set_Failure_Callback;

   ---------------------------
   -- Set_Recovery_Callback --
   ---------------------------

   procedure Set_Recovery_Callback (To : in Recover_Ptr) is
   begin
      Recovery_Process := To;
   end Set_Recovery_Callback;

   -------------------------
   -- Set_Change_Callback --
   -------------------------

   procedure Set_Change_Callback (To : in Change_Ptr) is
   begin
      Change_Process := To;
   end Set_Change_Callback;

   ---------
   -- Set --
   ---------

   procedure Set (Requestor : in Trains.Request_ID;
                  Turnout   : in Layout.Turnout_ID;
                  Direction : in Layout.Turn_Choice) is
      --pragma Unreferenced (Requestor);
   begin
      -- Set the turnout to the given direction
      if Direction = Layout.Left then
         Turnout_Tasks (Turnout).Move_Left (Requestor => Requestor);
      else
         Turnout_Tasks (Turnout).Move_Right (Requestor => Requestor);
      end if;
   end Set;

   ------------
   -- Status --
   ------------

   function Status (Turnout : in  Layout.Turnout_ID) return Status_Rec is
   begin
      -- returns the status of a turnout
      return Turnouts_Status (Turnout);
   end Status;

   ------------------
   -- Direction_Of --
   ------------------

   function Direction_Of (Turnout : in Layout.Turnout_ID)
                          return Layout.Turn_Choice is
   begin
      -- returns the direction of a turnout
      if Turnouts_Status (Turnout).Current = Fully_Left or
        Turnouts_Status (Turnout).Current = Moving_Left then
         return Layout.Left;
      else
         return Layout.Right;
      end if;
   end Direction_Of;

   ---------------
   -- Shut_Down --
   ---------------

   procedure Shut_Down is

      Turnout : Layout.Turnout_ID := 1;

   begin
      -- Sets all the turnouts to the left
      -- Each iteration, sets 5 turnouts to the left and waits 3 seconds
      loop
         Ada.Text_IO.Put_Line ("Shutting Down Turnout "
                               & Layout.Turnout_ID'Image (Turnout));
         Motors.Set (Motor     => Turnout,
                     Direction => Layout.Left);
         if Turnout rem 5 = 0 then
            delay 3.0;
         end if;
         exit when Turnout = Layout.Turnout_ID'Last;
         Turnout := Turnout + 1;
      end loop;
   end Shut_Down;

begin

   for ID in Layout.Turnout_ID loop
      Turnout_Tasks (ID).Assign_Turnout (Turnout_Num => ID);
   end loop;

end Turnouts;
