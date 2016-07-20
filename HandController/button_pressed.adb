--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tests      : Hand_Controller ------------------------------------------------
-- Updated    : 17 Feb 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Hand_Controller;
use type Hand_Controller.Button_State;
with Ada.Text_IO;

procedure Button_Pressed is

   -- Represents Red Button for Hand Controller B
   R_Button : Hand_Controller.Button_State;

   -- Represents Black Button for Hand Controller B
   B_Button : Hand_Controller.Button_State;

   -- Represents Two Way Switch for Hand Controller B
   TW_Switch : Hand_Controller.Switch_State;

   -- Represents Three Way Toggle for Hand Controller B
   THW_Toggle : Hand_Controller.Toggle_State;

   -- Represents the state of the black_button
   BButton_State : Hand_Controller.Button_State := Hand_Controller.Released;

   -- Represents the state of the red_button
   RButton_State : Hand_Controller.Button_State := Hand_Controller.Released;

   -- Represets the number of times the Red button is pressed before reset
   Count : Integer := 0;

begin
   Ada.Text_IO.Put_Line ("Button Testing beginning... ");
   -----------------------------------------------------------------------------
   -- Determine the states of the red and black button and increment or reset --
   -- count accordingly                                                       --
   -----------------------------------------------------------------------------
   -- Each iteration, check the states of the red and black buttons           --
   -----------------------------------------------------------------------------
   loop
      --------------------------------------------------------------------------
      -- Delay statement to account for switch jumping                        --
      --------------------------------------------------------------------------
      delay 0.01;
      --------------------------------------------------------------------------
      -- Get the state of the hand controller                                 --
      --------------------------------------------------------------------------
      Hand_Controller.Get_Controller_State (Controller => Hand_Controller.B,
                                            Red_Button => R_Button,
                                            Black_Button => B_Button,
                                            Two_Way_Switch => TW_Switch,
                                            Three_Way_Toggle => THW_Toggle);
      --------------------------------------------------------------------------
      -- Determine if the black button had been depressed                     --
      --------------------------------------------------------------------------
      if BButton_State = Hand_Controller.Depressed and
         B_Button = Hand_Controller.Released then
         -----------------------------------------------------------------------
         -- If black button has been depressed increment count                --
         -----------------------------------------------------------------------
         Count := Count + 1;
         BButton_State := Hand_Controller.Released;
      elsif BButton_State = Hand_Controller.Released and
      B_Button = Hand_Controller.Depressed then
         -----------------------------------------------------------------------
         -- Elsif the black button is released change the state of the button --
         -----------------------------------------------------------------------
         BButton_State := Hand_Controller.Depressed;
      end if;


      --------------------------------------------------------------------------
      -- Determine if the red button has been depressed                       --
      --------------------------------------------------------------------------
      if RButton_State = Hand_Controller.Depressed and
         R_Button = Hand_Controller.Released then
         -----------------------------------------------------------------------
         -- If red button has been depressed output how many times the black  --
         -- button has been depressed before reset then reset the count       --
         -----------------------------------------------------------------------
         Ada.Text_IO.Put ("You have pressed the black button ");
         Ada.Text_IO.Put (Integer'Image (Count));
         Ada.Text_IO.Put (" times.");
         Count := 0;
         RButton_State := Hand_Controller.Released;
      elsif RButton_State = Hand_Controller.Released and
      R_Button = Hand_Controller.Depressed then
         -----------------------------------------------------------------------
         -- Elsif the red button is released change the state of the button   --
         -----------------------------------------------------------------------
         RButton_State := Hand_Controller.Depressed;
      end if;
   end loop;

end Button_Pressed;


