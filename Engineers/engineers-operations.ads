--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested By  : Travis Sullivan ------------------------------------------------
-- Updated    : 08 May 15 ------------------------------------------------------
--------------------------------------------------------------------------------
package Engineers.Operations is

   procedure Change_Skill (Engineer : in Engineer_ID);

   function Get_Skill (Engineer : Engineer_ID) return Skill;

   procedure Enable (Engineer : in Engineer_ID);

   procedure Disable (Engineer : in Engineer_ID);

end Engineers.Operations;
