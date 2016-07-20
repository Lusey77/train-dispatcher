--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullivan ------------------------------------------------
-- Tests      : Hand_Controller ------------------------------------------------
-- Updated    : 17 Feb 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Ada.Text_IO;
with Hand_Controller;
with Common_Units;
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);

---------------------------------------------
-- A Procedure to test the hand controller --
---------------------------------------------

procedure Hand_Controller_Test is
   Red_Button        : Hand_Controller.Button_State;
   Black_Button      : Hand_Controller.Button_State;
   Direction_Switch  : Hand_Controller.Switch_State;
   Direction_Toggle  : Hand_Controller.Toggle_State;
   Knob_Percent      : Common_Units.Percent;
begin
   -----------------------------------------------------------------------------
   -- Displays the state of the controller upon the user hitting enter        --
   -----------------------------------------------------------------------------
   -- Each iteration, display the state of the controller at that given time  --
   -----------------------------------------------------------------------------
   loop
      Ada.Text_IO.Put_Line ("Please press enter to continue test.");
      Ada.Text_IO.Skip_Line;
      -----------------------------
      --Testing Hand Controller A--
      -----------------------------

      --------------------------------------------------------------------------
      -- Get Controller button/switch states                                  --
      --------------------------------------------------------------------------
      Hand_Controller.Get_Controller_State (Hand_Controller.A,
                                            Red_Button,
                                            Black_Button,
                                            Direction_Switch,
                                            Direction_Toggle);

      --------------------------------------------------------------------------
      -- Get controller knob percentage                                       --
      --------------------------------------------------------------------------
      Hand_Controller.Get_Black_Knob (Controller => Hand_Controller.A,
                                      Black_Knob => Knob_Percent);

      --------------------------------------------------------------------------
      -- Output results to the console                                        --
      --------------------------------------------------------------------------
      Ada.Text_IO.Put_Line ("The states on Controller A are: ");
      Ada.Text_IO.Put      ("Red button: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Button_State'Image (Red_Button));
      Ada.Text_IO.Put      ("Black_Botton: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Button_State'Image (Black_Button));
      Ada.Text_IO.Put      ("Switch direction: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Switch_State'Image
                            (Direction_Switch));
      Ada.Text_IO.Put      ("Toggle Direction: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Toggle_State'Image
                            (Direction_Toggle));
      Ada.Text_IO.Put      ("Knob Percent: ");
      Ada.Text_IO.Put_Line (Common_Units.Percent'Image (Knob_Percent));

      -----------------------------
      --Testing Hand Controller B--
      -----------------------------

      --------------------------------------------------------------------------
      -- Get Controller button/switch states                                  --
      --------------------------------------------------------------------------
      Hand_Controller.Get_Controller_State (Hand_Controller.B,
                                	    Red_Button,
                                            Black_Button,
                                            Direction_Switch,
                                            Direction_Toggle);

      --------------------------------------------------------------------------
      -- Get controller knob percentage                                       --
      --------------------------------------------------------------------------
      Hand_Controller.Get_Black_Knob (Controller => Hand_Controller.B,
                                      Black_Knob => Knob_Percent);

      --------------------------------------------------------------------------
      -- Output results to the console                                        --
      --------------------------------------------------------------------------
      Ada.Text_IO.Put_Line ("The states on Controller B are: ");
      Ada.Text_IO.Put      ("Red button: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Button_State'Image (Red_Button));
      Ada.Text_IO.Put      ("Black_Botton: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Button_State'Image (Black_Button));
      Ada.Text_IO.Put      ("Switch direction: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Switch_State'Image
                            (Direction_Switch));
      Ada.Text_IO.Put      ("Toggle Direction: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Toggle_State'Image
                            (Direction_Toggle));
      Ada.Text_IO.Put      ("Knob Percent: ");
      Ada.Text_IO.Put_Line (Common_Units.Percent'Image (Knob_Percent));


      -------------------------------
      -- Testing Hand Controller C --
      -------------------------------

      --------------------------------------------------------------------------
      -- Get Controller button/switch states                                  --
      --------------------------------------------------------------------------
      Hand_Controller.Get_Controller_State (Hand_Controller.C,
                                            Red_Button,
                                            Black_Button,
                                            Direction_Switch,
                                            Direction_Toggle);

      --------------------------------------------------------------------------
      -- Get controller knob percentage                                       --
      --------------------------------------------------------------------------
      Hand_Controller.Get_Black_Knob (Controller => Hand_Controller.C,
                                      Black_Knob => Knob_Percent);

      --------------------------------------------------------------------------
      -- Output results to the console                                        --
      --------------------------------------------------------------------------
      Ada.Text_IO.Put_Line ("The states on Controller C are: ");
      Ada.Text_IO.Put      ("Red button: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Button_State'Image (Red_Button));
      Ada.Text_IO.Put      ("Black_Botton: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Button_State'Image (Black_Button));
      Ada.Text_IO.Put      ("Switch direction: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Switch_State'Image
                            (Direction_Switch));
      Ada.Text_IO.Put      ("Toggle Direction: ");
      Ada.Text_IO.Put_Line (Hand_Controller.Toggle_State'Image
                            (Direction_Toggle));
      Ada.Text_IO.Put      ("Knob Percent: ");
      Ada.Text_IO.Put_Line (Common_Units.Percent'Image (Knob_Percent));
   end loop;

end Hand_Controller_Test;

