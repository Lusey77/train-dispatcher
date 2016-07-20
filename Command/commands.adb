--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested By  : Travis Sullivan ------------------------------------------------
-- Updated    : 10 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Ada.Characters.Handling;
with Ada.Text_IO;
package body Commands is

   -----------------------------------------------------------------------------
   -- Represents the states of the command procedure                          --
   -----------------------------------------------------------------------------
   type State_Type is (First_Char, Second_Char, Final_Char, Done);

   ---------
   -- Get --
   ---------

   procedure Get (Command : out Command_Rec) is

      State : State_Type := First_Char;
      Char  : Character;
      ID    : Integer;

   begin
      --------------------------------------------------------------------------
      -- Get a command                                                        --
      --------------------------------------------------------------------------
      -- Each iteration, get one character in a state                         --
      --------------------------------------------------------------------------
      loop
         case State is
            when First_Char =>
               Ada.Text_IO.Get_Immediate (Char);
               if Ada.Characters.Handling.Is_Digit (Char) then
                  ID := Integer'Value (' ' & Char);
                  State := Second_Char;
               elsif Char = ' ' then
                  Command := (Which => Stop_All);
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'r' then
                  Command := (Which => Restart);
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'q' then
                  Command := (Which => Quit);
                  State   := Done;
               else
                  Command := (Which => Error);
                  State   := Done;
               end if;
            when Second_Char =>
               Ada.Text_IO.Get_Immediate (Char);
               if Ada.Characters.Handling.Is_Digit (Char) then
                  ID := 10 * ID + Integer'Value (' ' & Char);
                  State := Final_Char;
               elsif Ada.Characters.Handling.To_Lower (Char) = 's' then
                  Command := (Which => Stop,
                              Train => Trains.Train_ID (ID));
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'g' then
                  Command := (Which => Go,
                              Train => Trains.Train_ID (ID));
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'r' then
                  Command := (Which   => Right,
                              Turnout => Layout.Turnout_ID (ID));
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'l' then
                  Command := (Which   => Left,
                              Turnout => Layout.Turnout_ID (ID));
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'f' then
                  Command := (Which => Free,
                              Block => Layout.Block_ID (ID));
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'e' then
                  Command := (Which    => Skill,
                              Engineer => Engineers.Engineer_ID (ID));
                  State   := Done;
               else
                  Command := (Which => Error);
                  State   := Done;
               end if;
            when Final_Char =>
               Ada.Text_IO.Get_Immediate (Char);
               if Ada.Characters.Handling.To_Lower (Char) = 's' then
                  Command := (Which => Stop,
                              Train => Trains.Train_ID (ID));
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'g' then
                  Command := (Which => Go,
                              Train => Trains.Train_ID (ID));
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'r' then
                  Command := (Which   => Right,
                              Turnout => Layout.Turnout_ID (ID));
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'l' then
                  Command := (Which   => Left,
                              Turnout => Layout.Turnout_ID (ID));
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'f' then
                  Command := (Which => Free,
                              Block => Layout.Block_ID (ID));
                  State   := Done;
               elsif Ada.Characters.Handling.To_Lower (Char) = 'e' then
                  Command := (Which    => Skill,
                              Engineer => Engineers.Engineer_ID (ID));
                  State   := Done;
               else
                  Command := (Which => Error);
                  State   := Done;
               end if;
            when Done =>
               exit;
         end case;
      end loop;
   exception
      when Constraint_Error =>
         Command := (Which => Error);
   end Get;

end Commands;
