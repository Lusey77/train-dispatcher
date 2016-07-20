--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullin --------------------------------------------------
-- Tests      : Dallee ---------------------------------------------------------
-- Updated    : 03 Mar 15 ------------------------------------------------------
-- Updated    : 08 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------

with Dallee;
with Ada.Text_IO;
with MaRTE_OS;

procedure Dallee_Test is
begin
   -- Loop through the possible Dallee_Num and play sounds
   for Dallee_Sound in Dallee.Dallee_Num loop
      Ada.Text_IO.Put_Line ("Testing Dallee Sound: " &
                              Dallee.Dallee_Num'Image (Dallee_Sound));
      -- Air horn On
      Dallee.Sound_Air_Horn (Dallee => Dallee_Sound,
                      Power  => Dallee.On);
      -- On for 2 seconds
      delay 2.0;
      -- Air horn Off
      Dallee.Sound_Air_Horn (Dallee => Dallee_Sound,
                      Power  => Dallee.Off);
      -- Bell On
      Dallee.Sound_Bell (Dallee => Dallee_Sound,
                         Power  => Dallee.On);
      -- On for 2 seconds
      delay 2.0;
      -- Bell Off.
      Dallee.Sound_Bell (Dallee => Dallee_Sound,
                  Power  => Dallee.Off);
   end loop;

   -- Test all three at the same time.
   Ada.Text_IO.Put_Line ("Testing Dallee Sound: Air Horn 1-3");
   Ada.Text_IO.Put_Line ("Startined Air Horn 1");
   Dallee.Sound_Air_Horn (Dallee => 1,
                          Power  => Dallee.On);
   delay 2.0;

   Ada.Text_IO.Put_Line ("Startined Air Horn 2");
   Dallee.Sound_Air_Horn (Dallee => 2,
                          Power  => Dallee.On);
   delay 2.0;

   Ada.Text_IO.Put_Line ("Startined Air Horn 3");
   Dallee.Sound_Air_Horn (Dallee => 3,
                          Power  => Dallee.On);
   delay 2.0;

   Ada.Text_IO.Put_Line ("Stopping Air Horn 1");
   Dallee.Sound_Air_Horn (Dallee => 1,
                          Power  => Dallee.Off);
   delay 2.0;
   Ada.Text_IO.Put_Line ("Stopping Air Horn 2");
   Dallee.Sound_Air_Horn (Dallee => 2,
                          Power  => Dallee.Off);
   delay 2.0;
   Ada.Text_IO.Put_Line ("Stopping Air Horn 3");
   Dallee.Sound_Air_Horn (Dallee => 3,
                          Power  => Dallee.Off);

   -- Testing all bells and one horn at the same time.
   for Dallee_Sound in Dallee.Dallee_Num loop
      Ada.Text_IO.Put_Line ("Testing Bell 1,2,3 Dallee Sound: " &
                                    Dallee.Dallee_Num'Image (Dallee_Sound));

      Ada.Text_IO.Put_Line ("Starting Bell 1");

      Dallee.Sound_Bell (Dallee => 1,
                         Power  => Dallee.On);

      Ada.Text_IO.Put_Line ("Starting Bell 2");

      Dallee.Sound_Bell (Dallee => 2,
                         Power  => Dallee.On);

      Ada.Text_IO.Put_Line ("Starting Bell 3");

      Dallee.Sound_Bell (Dallee => 3,
                         Power  => Dallee.On);

      delay 2.0;

      Ada.Text_IO.Put_Line ("Starting Air Horn: " &
                              Dallee.Dallee_Num'Image (Dallee_Sound));

      Dallee.Sound_Air_Horn (Dallee => Dallee_Sound,
                             Power  => Dallee.On);


      delay 2.0;

      Ada.Text_IO.Put_Line ("Stopping Air Horn: " &
                              Dallee.Dallee_Num'Image (Dallee_Sound));

      Dallee.Sound_Air_Horn (Dallee => Dallee_Sound,
                             Power  => Dallee.Off);

      delay 2.0;
      Ada.Text_IO.Put_Line ("Stopping Bell 1");

      Dallee.Sound_Bell (Dallee => 1,
                         Power  => Dallee.Off);


      Ada.Text_IO.Put_Line ("Stopping Bell 2");

      Dallee.Sound_Bell (Dallee => 2,
                         Power  => Dallee.Off);

      Ada.Text_IO.Put_Line ("Stopping Bell 3");

      Dallee.Sound_Bell (Dallee => 3,
                         Power  => Dallee.Off);
   end loop;
end Dallee_Test;
