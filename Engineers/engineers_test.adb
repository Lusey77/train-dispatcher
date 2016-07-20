--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullivan ------------------------------------------------
-- Tests      : Engineer -------------------------------------------------------
-- Updated    : 04 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------

with Engineers;
with Ada.Text_IO;
use type Engineers.State_Type;

procedure Engineers_Test is
   Test_State : Engineers.State_Type;
begin
   for Test_Engineer in Engineers.Engineer_ID loop
      Engineers.Signal_Event (Event    => Engineers.Initialize_Train,
                              Engineer => Test_Engineer);

      Test_State := Engineers.Get_Current_State (Test_Engineer);

      if Test_State /= Engineers.Novice then
         Ada.Text_IO.Put_Line ("Initializing Engineer #"
                               & Engineers.Engineer_ID'Image (Test_Engineer)
                               & ": Failed");

      end if;

      Engineers.Signal_Event (Event    => Engineers.Change_Skill,
                              Engineer => Test_Engineer);

      Test_State := Engineers.Get_Current_State (Test_Engineer);

      if Test_State /= Engineers.Expert then
         Ada.Text_IO.Put_Line ("Changing skill of Engineer #"
                               & Engineers.Engineer_ID'Image (Test_Engineer)
                               & ": Failed");
      end if;

      Engineers.Signal_Event (Event    => Engineers.Deinitialize_Train,
                              Engineer => Test_Engineer);

      Test_State := Engineers.Get_Current_State (Test_Engineer);

      if Test_State /= Engineers.Inactive then
         Ada.Text_IO.Put_Line ("Deinitializing Engineer #"
                               & Engineers.Engineer_ID'Image (Test_Engineer)
                               & ": Failed");
      end if;
   end loop;
end Engineers_Test;
