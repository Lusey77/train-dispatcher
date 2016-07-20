--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Ethan Morisette ------------------------------------------------
-- Tested By  : Travis Sullivan ------------------------------------------------
-- Updated    : 04 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with DAC;
with Common_Units;
use type Common_Units.Volts;
package body Cabs is

   -- Represents an array of percentages
   type Percent_Array is array (Cab_ID) of Percent;

   -- Protected array of cab percent values
   protected type Cab_Percents is
      -- Set the percent of the cab
      procedure Set_Percent (Cab : in Control_Cab_ID; Value : in Percent);
      -- Get the percentage of the cab
      function Get_Percent (Cab : in Cab_ID) return Percent;
   private
      -- Represents an array of cab values
      Cab_Array : Percent_Array := (0, 0, 0, 0, 0, 0, 0, 0);
   end Cab_Percents;

   protected body Cab_Percents is
      procedure Set_Percent (Cab : in Control_Cab_ID; Value : in Percent) is
      begin
         Cab_Array (Cab) := Value;
      end Set_Percent;

      function Get_Percent (Cab : in Cab_ID) return Percent is
      begin
         return Cab_Array (Cab);
      end Get_Percent;
   end Cab_Percents;

   -- A protected array of the current speed percentages of cabs
   Cab_Values : Cab_Percents;
   -- A protected array of the current speed limit percentages of cabs
   Cab_Limits : Cab_Percents;

--------------------------------------------------------------------------------

   -- We only want to be able to set the control cabs, not the ground cabs
   procedure Set (Cab : in Control_Cab_ID; Value : in Percent) is
      -- Represents the converted volts to be outputed to the cab's channel
      Cab_Volts : DAC.Output_Volts;
   begin
      -- If the limit for the indicated cab is less than value, set to limit
      if Cab_Limits.Get_Percent (Cab => Cab) < Value then
         Cab_Values.Set_Percent (Cab   => Cab,
                                 Value => Cab_Limits.Get_Percent (Cab => Cab));
      -- If the limit for the indicated cab is not less than value, set to value
      else
         Cab_Values.Set_Percent (Cab => Cab, Value => Value);
      end if;
      -- Convert percentage chosen to volts
      Cab_Volts := (DAC.Output_Volts'Last) *
        	   (Cab_Values.Get_Percent (Cab => Cab)) / 100.0;
      -- Write the volts to the DAC
      DAC.Write (Channel => DAC.Channel_Number (Cab - 1), Value => Cab_Volts);
   end Set;

   procedure Get (Cab : in Cab_ID; Value : out Percent) is
   begin
      -- Get the value from the value array for indicated cab
      Value := Cab_Values.Get_Percent (Cab => Cab);
   end Get;

   procedure Set_Limit (Cab : in Control_Cab_ID; Value : in Percent) is
   begin
      -- Set the limit to the new value
      Cab_Limits.Set_Percent (Cab => Cab, Value => Value);
      -- If the current percentage of the cab is higher than the new limit
      if Value < Cab_Values.Get_Percent (Cab => Cab) then
         -- Set the percentage of the cab to the new limit
         Set (Cab => Cab, Value => Value);
      end if;
   end Set_Limit;

   procedure Get_Limit (Cab : in Cab_ID; Value : out Percent) is
   begin
      -- Get the value from the limit array for indicated cab
      Value := Cab_Limits.Get_Percent (Cab => Cab);
   end Get_Limit;

begin
   -- Initialize all control cab values to 0 and control cab limits to 100
   for Cab_Num in Control_Cab_ID loop
      Set (Cab => Cab_Num, Value => 0);
      Set_Limit (Cab => Cab_Num, Value => 100);
   end loop;

end Cabs;
