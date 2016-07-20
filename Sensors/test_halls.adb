--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tests      : Halls ----------------------------------------------------------
-- Updated    : 22 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Halls;
with Callback_Package;
with MaRTE_OS;
with Ada.Text_IO;
procedure Test_Halls is

   Hall_Interrupt : constant Halls.Callback_Ptr :=
                      Callback_Package.Moniter_Halls'Access;

begin
   Ada.Text_IO.Put_Line ("Starting testing of hall sensors");
   Ada.Text_IO.Put_Line ("Testing the initialization and intterupt of sensors");
   Halls.Initialize;
   Halls.Enable (Callback => Hall_Interrupt);
   Ada.Text_IO.Put_Line ("Press [enter] to test the disable function");
   Ada.Text_IO.Skip_Line;
   Ada.Text_IO.Put_Line ("Testing the disable funtion");
   Halls.Disable;
   Ada.Text_IO.Put_Line ("Press [enter] to test the Is_Triggered function");
   Ada.Text_IO.Skip_Line;
   Ada.Text_IO.Put_Line ("Testing the Is_Triggered funtion on hall sensor 1");
   Ada.Text_IO.Put_Line ("Ensure the magnet is on the hall sensor so it will " &
                           "trigger.");
   Ada.Text_IO.Put_Line ("Press [enter] to continue the test");
   Ada.Text_IO.Skip_Line;
   if Halls.Is_Triggered (Hall => 1) then
      Ada.Text_IO.Put_Line ("Hall sensor is triggered");
   else
      Ada.Text_IO.Put_Line ("Hall sensor is not triggered");
   end if;
   Ada.Text_IO.Put_Line ("Press [enter] to continue the test");
   Ada.Text_IO.Skip_Line;
   Ada.Text_IO.Put_Line ("Ensure the magnet is off the hall sensor so it will" &
                           " not trigger.");
   Ada.Text_IO.Put_Line ("Press [enter] to continue the test");
   Ada.Text_IO.Skip_Line;
   if Halls.Is_Triggered (Hall => 1) then
      Ada.Text_IO.Put_Line ("Hall sensor is triggered");
   else
      Ada.Text_IO.Put_Line ("Hall sensor is not triggered");
   end if;

   Ada.Text_IO.Put_Line ("End of testing");

end Test_Halls;
