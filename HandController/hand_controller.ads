--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested  By : Kaleb Luse -----------------------------------------------------
-- Updated    : 17 Feb 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Common_Units;
package Hand_Controller is

   -- Represents the states a switch can be in
   type Switch_State is (Down, Up);

   -- Represents the states a toggle can be in
   type Toggle_State is (Left, Right, Center);

   -- Represents the states a button can be in
   type Button_State is (Depressed, Released);

   -- Represents the 3 hand controllers
   type H_Controller is (A, B, C);

   -----------------------------------------------------------------------------
   -- Procedure that gets the current state of the controller                 --
   -----------------------------------------------------------------------------
   procedure Get_Controller_State (Controller       : in  H_Controller;
                                   Red_Button       : out Button_State;
                                   Black_Button     : out Button_State;
                                   Two_Way_Switch   : out Switch_State;
                                   Three_Way_Toggle : out Toggle_State);

   -----------------------------------------------------------------------------
   -- Procedure that gets the state of the black knob on the hand controller  --
   -----------------------------------------------------------------------------
   procedure Get_Black_Knob (Controller : in  H_Controller;
                             Black_Knob : out Common_Units.Percent);

private

   -- Describes how in memory the button state should be stored
   for Button_State use (Depressed => 2#0#,
                         Released  => 2#1#);

   -- Describes how much memory a button state should take up
   for Button_State'Size use 1;

   -- Describes how in memory the button state should be stored
   for Switch_State use (Down => 2#0#,
                         Up   => 2#1#);

   -- Describes how much memory a switch state should take up
   for Switch_State'Size use 1;

   -- Describes how in memory the toggle state should be stored
   for Toggle_State use (Left   => 2#01#,
                         Right  => 2#10#,
                         Center => 2#11#);

   -- Describes how much memory a toggle state should take up
   for Toggle_State'Size use 2;

end Hand_Controller;
