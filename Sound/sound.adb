--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullivan ------------------------------------------------
-- Tested  By : Kaleb Luse -----------------------------------------------------
-- Updated    : 31 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------
package body Sound is
   -- Represents the types of blasts to be processed or done if none left
   type Blasts       is (Short, Long, Done);
   -- Represents a list of blasts
   type Blast_List   is array (1 .. 5)      of Blasts;
   -- Represents an array of blast lists indexed by the type of signals
   type Signal_Array is array (Horn_Signal) of Blast_List;

   -- Constant that holds the  blast list for each of the designated signals
   Signal_List : constant Signal_Array := ((Short, Done, Done, Done, Done),
                                           (Long, Done, Done, Done, Done),
                                           (Long, Short, Done, Done, Done),
                                           (Long, Long, Done, Done, Done),
                                           (Long, Long, Short, Long, Done),
                                           (Long, Long, Short, Done, Done),
                                           (Short, Short, Short, Short, Done));

   -- Represents a task to blow a horn
   task type Sound_Horn_Task is
      entry Assign_ID (Unit : in Installed_Range);
      entry Train_Signal (Signal : in Horn_Signal);
   end Sound_Horn_Task;

   task body Sound_Horn_Task is

      -- Variable that hold the information about how to index the horn blast
      -- array
      My_Signal      : Horn_Signal;
      My_Unit        : Installed_Range;
      Counter        : Natural;

      -- These variables hold the horn durations as constants.
      Long_Duration  : constant Duration := 3.0;
      Short_Duration : constant Duration := 0.8;
      Delay_Duration : constant Duration := 0.5;

   begin
      -- Loops through and waits for a renezvous
      -- Each iteration, perform the code associated with the rendezvous
      accept Assign_ID (Unit : in Installed_Range) do
         My_Unit := Unit;
      end Assign_ID;
      loop
         accept Train_Signal (Signal : in Horn_Signal) do
            My_Signal := Signal;
         end Train_Signal;
         -- Initializa LCV
         Counter := 0;
         -- Loops through the blast list and performs the correct blasts
         -- Each iteration, sound one blast or exit the loop
         loop
            Counter := Counter + 1;
            -- Determine whether the signal is short, long, or if there are
            -- no blasts left
            if Signal_List (My_Signal) (Counter) = Short then
               -- If blast in blast list is short then sound for short duration
               Dallee.Sound_Air_Horn (Dallee => My_Unit,
                                      Power  => Dallee.On);

               delay Short_Duration;
               Dallee.Sound_Air_Horn (Dallee => My_Unit,
                                      Power  => Dallee.Off);
               delay Delay_Duration;
            elsif Signal_List (My_Signal) (Counter) = Long then
               -- If blast in blast list is long then sound for long duration
               Dallee.Sound_Air_Horn (Dallee => My_Unit,
                                      Power  => Dallee.On);

               delay Long_Duration;
               Dallee.Sound_Air_Horn (Dallee => My_Unit,
                                      Power  => Dallee.Off);
               delay Delay_Duration;
            else
               exit;
            end if;
         end loop;
      end loop;
   end Sound_Horn_Task;

   -- Represents the type for an array of horn tasks
   type Sound_Horn_Array is array (Installed_Range) of Sound_Horn_Task;
   -- Represents the variable that holds the array of tasks
   Horns : Sound_Horn_Array;

   ----------------
   -- Sound_Horn --
   ----------------

   procedure Sound_Horn (Unit   : in Installed_Range;
                         Signal : in Horn_Signal) is

   begin
      select
         Horns (Unit).Train_Signal (Signal => Signal);
      or
         delay 0.01;
         null;
      end select;
   end Sound_Horn;

   -------------
   -- Bell_On --
   -------------

   procedure Bell_On (Unit : in Installed_Range) is
   begin
      Dallee.Sound_Bell (Dallee => Unit,
                         Power  => Dallee.On);

   end Bell_On;

   --------------
   -- Bell_Off --
   --------------

   procedure Bell_Off (Unit : in Installed_Range) is
   begin
      Dallee.Sound_Bell (Dallee => Unit,
                         Power  => Dallee.Off);
   end Bell_Off;

begin
   -- Give the corresponding Horn tasks the unit they are responsible for
   for Index in Installed_Range loop
      Horns (Index).Assign_ID (Unit => Index);
   end loop;

end Sound;
