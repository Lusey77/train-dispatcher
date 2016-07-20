--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullivan ------------------------------------------------
-- Tests      : Motors ---------------------------------------------------------
-- Updated    : 07 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Layout;
with Motors;
with Ada.Text_IO;
with MaRTE_OS;
with DoubleTalk;
procedure Motors_Test is

   package Turn_IO is new Ada.Text_IO.Enumeration_IO
     (Enum => Layout.Turn_Choice);

   package Turnout_IO is new Ada.Text_IO.Integer_IO
     (Num => Layout.Turnout_ID);

   Turnout   : Layout.Turnout_ID;
   Direction : Layout.Turn_Choice;

begin

   Ada.Text_IO.Put ("Beginning Test:");

   -- Turn all the turnouts to the right
   for Turnout_Right in Layout.Turnout_ID loop
      Motors.Set (Motor     => Turnout_Right,
                  Direction => Layout.Right);
      delay 4.0;
      if not Motors.In_Position (Motor => Turnout_Right) then
         Ada.Text_IO.Put ("Turning turnout "
                      & Layout.Turnout_ID'Image (Turnout_Right) &
                      " to the right: ");
         Ada.Text_IO.Put_Line ("Failed.");
      else
         DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                           ("Turnout " & Layout.Turnout_ID'Image
                           (Turnout_Right) & " was successful."),
                           Voice  => DoubleTalk.Paul);
      end if;
   end loop;
   Ada.Text_IO.Put_Line ("End puting in the right.");
   Ada.Text_IO.Skip_Line;
   -- Turn all the turnouts to the left
   for Turnout_Left in Layout.Turnout_ID loop
      Motors.Set (Motor     => Turnout_Left,
                  Direction => Layout.Left);
      delay 4.0;
      if not Motors.In_Position (Motor => Turnout_Left) then
         Ada.Text_IO.Put ("Turning turnout "
                            & Layout.Turnout_ID'Image (Turnout_Left) &
                            " to the right: ");
         Ada.Text_IO.Put_Line ("Failed.");
      else
         DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                     ("Turnout " & Layout.Turnout_ID'Image
                     (Turnout_Right) & " was successful."),
                           Voice  => DoubleTalk.Paul);
      end if;
   end loop;
   -- Get a turnout number and direction and turn the turnout
   -- Used to test a failed turnout
   loop
      Ada.Text_IO.Put ("Enter a turnout number");
      Turnout_IO.Get (Turnout);
      Ada.Text_IO.Put ("Enter a direction");
      Turn_IO.Get (Direction);
      Motors.Set (Motor     => Turnout,
                  Direction => Direction);
      loop
         delay 0.2;
         exit when Motors.In_Position (Motor => Turnout);
         Ada.Text_IO.Put ('.');
      end loop;
   end loop;
end Motors_Test;
