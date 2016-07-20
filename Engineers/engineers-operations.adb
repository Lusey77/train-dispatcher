--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested By  : Travis Sullivan ------------------------------------------------
-- Updated    : 26 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Hand_Controller;
with Layout;
use type Hand_Controller.Switch_State;
use type Hand_Controller.Toggle_State;
use type Hand_Controller.Button_State;
with Common_Units;
use type Common_Units.Percent;
with Trains;
with Trains.Operations;
use type Trains.Train_ID;
with DoubleTalk;
with Display;
pragma Elaborate (Display);
pragma Elaborate (Trains.Operations);
with Ada.Exceptions;
package body Engineers.Operations is

   -- Represents the fastest a human can press a button
   Max_Button_Speed : constant Duration := 0.1;

   -- Represents an array of engineers and their states
   type Engineer_State_Array is array (Engineer_ID) of Skill;

   -- Represents the protected type that holds the array of engineer states
   protected Engineer_States is
      function  Get_Skill (Engineer : in Engineer_ID) return Skill;
      procedure Change_Skill (Engineer : in Engineer_ID);
   private
      Protected_Engineer_States : Engineer_State_Array;
   end Engineer_States;

   protected body Engineer_States is
      procedure Change_Skill (Engineer : in Engineer_ID) is
      begin
         -- Determine the state of the engineer to change it
         if Protected_Engineer_States (Engineer) = Novice then
            -- If novice then switch to expert
            Protected_Engineer_States (Engineer) := Expert;
            Display.Put (Train => Trains.Train_ID (Engineer),
                         Skill => Expert);
         else
            -- Else change to novice
            Protected_Engineer_States (Engineer) := Novice;
            Display.Put (Train => Trains.Train_ID (Engineer),
                         Skill => Novice);
         end if;
      end Change_Skill;

      function Get_Skill (Engineer : in Engineer_ID) return Skill is
      begin
         -- Return the skill of the given engineer
         return Protected_Engineer_States (Engineer);
      end Get_Skill;
   end Engineer_States;

   -- Represents a task to keep track of each engineer and their skills
   task type Moniter_Hand_Controller is
      -- Assigns the IDs to each engineer
      entry Assign_IDs (Train      : in Trains.Train_ID;
                        Engineer   : in Engineer_ID;
                        Control    : in Hand_Controller.H_Controller);
      -- Enables an engineer
      entry Enable;
      -- Disables an engineer
      entry Disable;
      -- Changes the skill of an engineer
      entry Change_Skill;
   end Moniter_Hand_Controller;

   task body Moniter_Hand_Controller is

      -- Represents the ID of the train the engineer is controlling
      My_Train         : Trains.Train_ID;

      -- Represents the ID of the engineer
      My_ID            : Engineer_ID;

      -- Represents the controller sending signals to the engineer
      My_Controller    : Hand_Controller.H_Controller;

      -- Represents the different buttons/switches on the hand controller
      Red_Button       : Hand_Controller.Button_State;
      Black_Button     : Hand_Controller.Button_State;
      Direction_Switch : Hand_Controller.Switch_State;
      Direction_Toggle : Hand_Controller.Toggle_State;
      Knob_Percent     : Common_Units.Percent;

      -- Represents the previous states of the hand controllers switches/buttons
      BButton_State    : Hand_Controller.Button_State :=
                           Hand_Controller.Released;
      RButton_State    : Hand_Controller.Button_State :=
                           Hand_Controller.Released;
      DSwitch_State    : Hand_Controller.Switch_State :=
                           Hand_Controller.Up;
      DToggle_State    : Hand_Controller.Toggle_State :=
                           Hand_Controller.Center;

      -- Represents whether the train is stopped
      Stopped          : Boolean := True;
      -- Represents whether the engineer has been alerted or not to stop
      Alerted          : Boolean := False;
      -- Represents how long train has been stopped
      Time_Stopped     : Duration := 0.0;
      -- Represents the treshold of min throttle to be considered stopped
      Min_Throttle     : Common_Units.Percent;

      procedure Moniter_Stopped_State is
      begin
         -- Determine the minimum throttle to determine if stopped
         Min_Throttle := Trains.Operations.Get_Min_Throttle (My_Train) / 2.0;
         -- Determine if the train is still stopped
         if Stopped and Knob_Percent > Min_Throttle then
            -- If the train was stopped but it is now above the min
            -- throttle change train to not stopped and reset the timer
            Stopped := False;
            Time_Stopped := 0.0;
         elsif not Stopped and Knob_Percent <= Min_Throttle then
            -- If the train was not stopped but has now reached the
            -- min throttle range then change the train to stopped
            Stopped      := True;
         elsif Stopped and Knob_Percent <= Min_Throttle then
            -- If the train is stopped and still under the min throttle
            -- threshold then add to the timer the amount of time that
            -- has passed
            Time_Stopped := Time_Stopped + Max_Button_Speed;
         end if;
      end Moniter_Stopped_State;

      procedure Moniter_Black_Knob is
      begin
         Trains.Operations.Set_Speed (Train => My_Train,
                                      Speed => Knob_Percent);
      end Moniter_Black_Knob;

      procedure Moniter_Black_Button is
      begin
         -- Check if the state of the black button has changed
         if BButton_State = Hand_Controller.Depressed and
           Black_Button = Hand_Controller.Released then
            -- If the black button was depressed sound the trains horn
            Trains.Operations.Sound_Horn (Train => My_Train);
            BButton_State := Hand_Controller.Released;
         elsif BButton_State = Hand_Controller.Released and
           Black_Button = Hand_Controller.Depressed then
            -- Elsif the button was released then change the state of
            -- the button
            BButton_State := Hand_Controller.Depressed;
         end if;
      end Moniter_Black_Button;

      procedure Moniter_Red_Button is
      begin
         -- Check if the state of the red button has changed
         if RButton_State = Hand_Controller.Depressed and
           Red_Button = Hand_Controller.Released then
            -- If the red button was depressed alert the train and
            -- change the state of the button
            Trains.Operations.Stop_Train (Train => My_Train);
            RButton_State := Hand_Controller.Released;
         elsif RButton_State = Hand_Controller.Released and
           Red_Button = Hand_Controller.Depressed then
            -- Elsif the button was released then change the state of
            -- the button
            RButton_State := Hand_Controller.Depressed;
         end if;
      end Moniter_Red_Button;

      procedure Moniter_Direction_Switch is
      begin
         -- Check if the state of the direction switch has changed
         if Direction_Switch = Hand_Controller.Up and
           DSwitch_State = Hand_Controller.Down then
            DSwitch_State := Hand_Controller.Up;
            -- Check the state of the enginee and perform the correct
            -- operations accordingly
            if Engineer_States.Get_Skill (My_ID) = Novice then
               -- If novice check for stopped conditions
               if Stopped and Time_Stopped >= 3.0 then
                  -- If stopped for more than 3 seconds perform change
                  Trains.Operations.Set_Forward (Train => My_Train);
                  Alerted := False;
               elsif Stopped and Time_Stopped < 3.0 then
                  -- elsif stopped but not for 3 seconds then alert
                  -- engineer
                  if not Alerted then
                     DoubleTalk.Speak (Phrase =>
                        DoubleTalk.Phrase_Strings.To_Bounded_String (
                         "You have not been stopped for 3 seconds."),
                                       Voice  => DoubleTalk.Vader);
                     DoubleTalk.Speak (Phrase =>
                        DoubleTalk.Phrase_Strings.To_Bounded_String (
                         "Please wait a moment longer and try again."),
                                       Voice  => DoubleTalk.Vader);
                     Alerted := True;
                  end if;
               else
                  -- else alert engineer they are not stopped
                  if not Alerted then
                     DoubleTalk.Speak (Phrase =>
                        DoubleTalk.Phrase_Strings.To_Bounded_String (
                         "You must be stopped to switch directions."),
                                       Voice  => DoubleTalk.Vader);
                     Alerted := True;
                  end if;
               end if;
            else
               if not Stopped then
                  -- else alert engineer they are not stopped
                  if not Alerted then
                     DoubleTalk.Speak (Phrase =>
                        DoubleTalk.Phrase_Strings.To_Bounded_String (
                         "You must be stopped to switch directions."),
                                       Voice  => DoubleTalk.Vader);
                     Alerted := True;
                  end if;
                  -- Else engineer is expert, so perform the change
               else
                  Trains.Operations.Set_Forward (Train => My_Train);
               end if;
            end if;
         elsif Direction_Switch = Hand_Controller.Down and
           DSwitch_State = Hand_Controller.Up then
            DSwitch_State := Hand_Controller.Down;
            -- Check the state of the enginee and perform the correct
            -- operations accordingly
            if Engineer_States.Get_Skill (My_ID) = Novice then
               -- If novice check for stopped conditions
               if Stopped and Time_Stopped >= 3.0 then
                  -- If stopped for more than 3 seconds perform change
                  Trains.Operations.Set_Backward (Train => My_Train);
                  Alerted := False;
               elsif Stopped and Time_Stopped < 3.0 then
                  -- elsif stopped but not for 3 seconds then alert
                  -- engineer
                  if not Alerted then
                     DoubleTalk.Speak (Phrase =>
                        DoubleTalk.Phrase_Strings.To_Bounded_String (
                         "You have not been stopped for 3 seconds."),
                                       Voice  => DoubleTalk.Vader);
                     DoubleTalk.Speak (Phrase =>
                        DoubleTalk.Phrase_Strings.To_Bounded_String (
                         "Please wait a moment longer and try again."),
                                       Voice  => DoubleTalk.Vader);
                     Alerted := True;
                  end if;
               else
                  -- else alert engineer they are not stopped
                  if not Alerted then
                     DoubleTalk.Speak (Phrase =>
                        DoubleTalk.Phrase_Strings.To_Bounded_String (
                         "You must be stopped to switch directions."),
                                       Voice  => DoubleTalk.Vader);
                     Alerted := True;
                  end if;
               end if;
            else
               if not Stopped then
                  -- else alert engineer they are not stopped
                  if not Alerted then
                     DoubleTalk.Speak (Phrase =>
                        DoubleTalk.Phrase_Strings.To_Bounded_String (
                         "You must be stopped to switch directions."),
                                       Voice  => DoubleTalk.Vader);
                     Alerted := True;
                  end if;
                  -- Else engineer is expert, so perform the change
               else
                  -- Else engineer is expert, so perform the change
                  Trains.Operations.Set_Backward (Train => My_Train);
               end if;
            end if;
         end if;
      end Moniter_Direction_Switch;

      procedure Moniter_Toggle_Switch is
      begin
         if Direction_Toggle = Hand_Controller.Left
           and DToggle_State /= Hand_Controller.Left then
            DToggle_State := Hand_Controller.Left;
            -- Check the state of the enginee and perform the correct
            -- operations accordingly
            if Engineer_States.Get_Skill (My_ID) = Novice then
               -- If novice check for stopped conditions
               if Stopped and Time_Stopped >= 3.0 then
                  -- If stopped for more than 3 seconds perform change
                  Trains.Operations.Change_Turnout (Train => My_Train,
                                            Direction => Layout.Left);
               elsif Stopped and Time_Stopped < 3.0 then
                  -- elsif stopped but not for 3 seconds then alert
                  -- engineer
                  DoubleTalk.Speak (Phrase =>
                     DoubleTalk.Phrase_Strings.To_Bounded_String (
                      "You have not been stopped for 3 seconds."),
                                    Voice  => DoubleTalk.Vader);
                  DoubleTalk.Speak (Phrase =>
                     DoubleTalk.Phrase_Strings.To_Bounded_String (
                      "Please wait a moment longer and try again."),
                                     Voice  => DoubleTalk.Vader);
               else
                  -- else alert engineer they are not stopped
                  DoubleTalk.Speak (Phrase =>
                     DoubleTalk.Phrase_Strings.To_Bounded_String (
                      "You must be stopped for 3 seconds before " &
                      "changing directions."),
                                    Voice  => DoubleTalk.Vader);
               end if;
            else
               -- Else engineer is expert, so perform the change
               Trains.Operations.Change_Turnout (Train     => My_Train,
                                      Direction => Layout.Left);
            end if;
         elsif Direction_Toggle = Hand_Controller.Right
           and DToggle_State /= Hand_Controller.Right then
            DToggle_State := Hand_Controller.Right;
            -- Check the state of the engineer and perform the correct
            -- operations accordingly
            if Engineer_States.Get_Skill (My_ID) = Novice then
               -- If novice check for stopped conditions
               if Stopped and Time_Stopped >= 3.0 then
                  -- If stopped for more than 3 seconds perform change
                  Trains.Operations.Change_Turnout (Train => My_Train,
                                         Direction => Layout.Right);
               elsif Stopped and Time_Stopped < 3.0 then
                  -- elsif stopped but not for 3 seconds then alert
                  -- engineer
                  DoubleTalk.Speak (Phrase =>
                     DoubleTalk.Phrase_Strings.To_Bounded_String (
                      "You have not been stopped for 3 seconds."),
                                    Voice  => DoubleTalk.Vader);
                  DoubleTalk.Speak (Phrase =>
                     DoubleTalk.Phrase_Strings.To_Bounded_String (
                      "Please wait a moment longer and try again."),
                                     Voice  => DoubleTalk.Vader);
               else
                  -- else alert engineer they are not stopped
                  DoubleTalk.Speak (Phrase =>
                     DoubleTalk.Phrase_Strings.To_Bounded_String (
                      "You must be stopped for 3 seconds before " &
                      "changing directions."),
                                    Voice  => DoubleTalk.Vader);
               end if;
            else
               -- Else engineer is expert, so perform the change
               Trains.Operations.Change_Turnout (Train     => My_Train,
                                      Direction => Layout.Right);
            end if;
         elsif Direction_Toggle = Hand_Controller.Center
           and DToggle_State /= Hand_Controller.Center then
            DToggle_State := Hand_Controller.Center;
         end if;
      end Moniter_Toggle_Switch;

   begin
      accept Assign_IDs (Train    : in Trains.Train_ID;
                         Engineer : in Engineer_ID;
                         Control  : in Hand_Controller.H_Controller) do
         My_Train      := Train;
         My_ID         := Engineer;
         My_Controller := Control;
      end Assign_IDs;
      loop
         select
            accept Enable;
            loop
               select
                  accept Enable;
               or
                  accept Disable;
                  exit;
               or
                  accept Change_Skill;
                  Engineer_States.Change_Skill (Engineer => My_ID);
               or
                  delay Max_Button_Speed;
                  -- Get the current state of the hand controller
                  Hand_Controller.Get_Controller_State (My_Controller,
                                                        Red_Button,
                                                        Black_Button,
                                                        Direction_Switch,
                                                        Direction_Toggle);
                  Hand_Controller.Get_Black_Knob (Controller => My_Controller,
                                                  Black_Knob => Knob_Percent);
                  -- Account for the electromagnetic waves that cause it to go
                  -- out of range
                  if Knob_Percent > 100.0 then
                     Knob_Percent := 100.0;
                  end if;
                  Moniter_Stopped_State;
                  Moniter_Black_Knob;
                  Moniter_Black_Button;
                  Moniter_Red_Button;
                  Moniter_Direction_Switch;
                  Moniter_Toggle_Switch;
               end select;
            end loop;
         or
            accept Disable;
         or
            accept Change_Skill;
         or
            delay 0.2;
         end select;
      end loop;
   exception
      when Except : others =>
         Display.Put_Error ("Task Engineer_" & Engineer_ID'Image (My_ID) &
              " with the exception " & Ada.Exceptions.Exception_Name (Except));
   end Moniter_Hand_Controller;

   -- Represents the type for an array of engineer tasks
   type My_Task_Array is array (Engineer_ID) of Moniter_Hand_Controller;

   -- Represents an array of engineer tasks
   My_Task  : My_Task_Array;

   procedure Change_Skill (Engineer : in Engineer_ID) is
   begin
      -- Rendezvous or abandon
      select
         My_Task (Engineer).Change_Skill;
      or
         delay 0.001;
      end select;
   end Change_Skill;

   function Get_Skill (Engineer : Engineer_ID) return Skill is
   begin
      return Engineer_States.Get_Skill (Engineer);
   end Get_Skill;

   procedure Enable (Engineer : in Engineer_ID) is
   begin
      My_Task (Engineer).Enable;
   end Enable;

   procedure Disable (Engineer : in Engineer_ID) is
   begin
      My_Task (Engineer).Disable;
   end Disable;

   -- Initialize loop variables
   Control  : Hand_Controller.H_Controller := Hand_Controller.A;
   Train    : Trains.Train_ID              := 1;
   Engineer : Engineer_ID                  := 1;

begin

   -- Assign all the engineers their respecitve ID's
   -- Each iteration, assign one engineer's ID's
   loop
      My_Task (Engineer).Assign_IDs (Train    => Train,
                                     Engineer => Engineer,
                                     Control  => Control);
      exit when Engineer = 3;
      Train    := Train + 1;
      Control  := Hand_Controller.H_Controller'Succ (Control);
      Engineer := Engineer + 1;
   end loop;


end Engineers.Operations;
