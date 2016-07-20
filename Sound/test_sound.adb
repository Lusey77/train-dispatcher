--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tests      : Sound ----------------------------------------------------------
-- Updated    : 12 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with MaRTE_OS;
with Ada.Text_IO;
with Sound;
procedure Test_Sound is

begin

   -- Test all the units and their signals individually
   for Unit in Sound.Installed_Range loop
      for Signal in Sound.Horn_Signal loop
         Ada.Text_IO.Put_Line ("Press enter to test next horn");
         Ada.Text_IO.Skip_Line;
         Ada.Text_IO.Put_Line ("Testing Unit " & Sound.Installed_Range'Image
                               (Unit) & " Signal" & Sound.Horn_Signal'Image
                               (Signal));
         Sound.Sound_Horn (Unit   => Unit,
                           Signal => Signal);
      end loop;
   end loop;

   -- Test all the units at once with their different signals
   for Signal in Sound.Horn_Signal loop
      Ada.Text_IO.Put_Line ("Press enter to test next horn");
      Ada.Text_IO.Skip_Line;
      Ada.Text_IO.Put_Line ("Sounding all units with " &
                       Sound.Horn_Signal'Image (Signal) & " Signal");
      Sound.Sound_Horn (Unit   => 1,
                        Signal => Signal);
      Sound.Sound_Horn (Unit   => 2,
                        Signal => Signal);
      Sound.Sound_Horn (Unit   => 3,
                        Signal => Signal);
   end loop;

   Ada.Text_IO.Skip_Line;

   -- Tests the abandoning of a rendezvous
   Ada.Text_IO.Put_Line ("Sending Start signal");
   Sound.Sound_Horn (Unit   => 1,
                     Signal => Sound.Start);
   Ada.Text_IO.Put_Line ("Sent stop");
   Sound.Sound_Horn (Unit   => 1,
                     Signal => Sound.Stop);
   Ada.Text_IO.Put_Line ("Sent approaching station");
   Sound.Sound_Horn (Unit   => 1,
                     Signal => Sound.Approach_Station);

   Ada.Text_IO.Skip_Line;

   --Tests the bells individually
   for Unit in Sound.Installed_Range loop
      Sound.Bell_On (Unit => Unit);
      delay 2.0;
      Sound.Bell_Off (Unit => Unit);
   end loop;

   -- Tests the bells all at once
   Sound.Bell_On (Unit => 1);
   Sound.Bell_On (Unit => 2);
   Sound.Bell_On (Unit => 3);
   delay 2.0;
   Sound.Bell_Off (Unit => 1);
   Sound.Bell_Off (Unit => 2);
   Sound.Bell_Off (Unit => 3);

end Test_Sound;
