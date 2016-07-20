--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : John McCormick -------------------------------------------------
-- Tests      : Display --------------------------------------------------------
-- Updated    : May 2008 -------------------------------------------------------
--------------------------------------------------------------------------------
with Ada.Text_IO;  use Ada.Text_IO;
with Display;
with Engineers;
with Layout;
with Locomotives;
with Trains;
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
procedure Display_Test is

begin
   Put_Line ("Press Enter to begin (and again after initial box)");
   Skip_Line;

   -- Set up initial layout (without data)
   Display.Enable;
   Skip_Line;

   -- Display all possible items on the display

   -- Turnouts
   for Turnout in Layout.Turnout_ID loop
      Display.Put (Turnout   => Turnout,
                   Direction => Layout.Left,
                   Moving    => True);
   end loop;

   -- Train #1
   Display.Put (Train => 1,
                Name => Locomotives.Available_Locos (1).Name);
   Display.Put (Train => 1,
                Skill => Engineers.Novice);
   Display.Put (Train => 1,
                Direction => Trains.Forward);
   Display.Put (Train => 1,
                Throttle => 45);
   Display.Put (Train => 1,
                Blocks => ((3, Layout.Normal),
                           (4, Layout.Normal),
                           (5, Layout.Normal)));
   Display.Put (Train => 1,
                Status => (Reasons => (True, False, True, False),
                           Block    => 5,
                           Turnouts => (others => False)));

   -- Train #2
   Display.Put (Train => 2,
                Name => Locomotives.Available_Locos (2).Name);
   Display.Put (Train => 2,
                Skill => Engineers.Expert);
   Display.Put (Train => 2,
                Direction => Trains.Backward);
   Display.Put (Train => 2,
                Throttle => 95);
   Display.Put (Train => 2,
                Blocks => ((7, Layout.Normal),
                           (6, Layout.Normal),
                           (14, Layout.Normal),
                           (21, Layout.Normal)));
   Display.Put (Train => 2,
                Status => (Reasons => (True, True, False, True),
                           Block    => 9,
                           Turnouts     => (5 | 6 | 12 | 19 | 21 => True,
                                            others               => False)));

   -- Train #3
   Display.Put (Train => 3,
                Name => Locomotives.Available_Locos (3).Name);
   Display.Put (Train => 3,
                Skill => Engineers.Novice);
   Display.Put (Train => 3,
                Direction => Trains.Forward);
   Display.Put (Train => 3,
                Throttle => 0);
   Display.Put (Train => 3,
                Blocks => (1 => (24, Layout.Normal)));
   Display.Put (Train => 3,
                Status => (Reasons => (False, True, True, False),
                           Block    => 7,
                           Turnouts     => (15 | 16 => True,
                                            others  => False)));

   -- Go throught the remaining turnout states
   delay 3.0;
   for Turnout in Layout.Turnout_ID loop
      Display.Put (Turnout   => Turnout,
                   Direction => Layout.Left,
                   Moving    => False);
      delay 0.2;
   end loop;
   delay 3.0;
   for Turnout in Layout.Turnout_ID loop
      Display.Put (Turnout   => Turnout,
                   Direction => Layout.Right,
                   Moving    => True);
      delay 0.2;
   end loop;

      delay 3.0;
   for Turnout in Layout.Turnout_ID loop
      Display.Put (Turnout   => Turnout,
                   Direction => Layout.Right,
                   Moving    => False);
      delay 0.2;
   end loop;

   --Check the general error message display
   for Message in 0 .. 5 loop
      Display.Put_Error ("Error message number #" & Integer'Image (Message));
      delay 1.0;
   end loop;
   for Message in 6 .. 500 loop
      Display.Put_Error ("Error message number #" & Integer'Image (Message));
      delay 0.5;
   end loop;

end Display_Test;
