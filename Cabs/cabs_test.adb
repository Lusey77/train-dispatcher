--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullivan ------------------------------------------------
-- Tests      : Cabs -----------------------------------------------------------
-- Updated    : 04 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Cabs;
with Ada.Text_IO;
with MaRTE_OS;

procedure Cabs_Test is
   type Test_Inputs_Type is array (1 .. 5) of Cabs.Percent;

   Test_Inputs : constant Test_Inputs_Type :=
                   (1 => 1, 2 => 25, 3 => 32, 4 => 6, 5 => 100);
   Test_Output : Cabs.Percent;
   Char : Character;
   Cab_Num   : Cabs.Control_Cab_ID;
   Cab_Value : Cabs.Percent;
   Cab_Limit : Cabs.Percent;

   package Percent_IO is new Ada.Text_IO.Integer_IO (Num => Cabs.Percent);
   package Cab_IO     is new Ada.Text_IO.Integer_IO (Num => Cabs.Cab_ID);

begin
   -- Begin testing
   Ada.Text_IO.Put_Line ("---- Tests Begin ----");
   -- Run through a quick verification test to ensure its writing and reading
   -- properly
   for Test_Cab in Cabs.Control_Cab_ID loop
      for Cab_Percent in 1 .. 5 loop
         Cabs.Set (Cab   => Test_Cab,
                   Value => Test_Inputs (Cab_Percent));

         Cabs.Get (Cab   => Test_Cab,
                   Value => Test_Output);

         if Test_Output /= Test_Inputs (Cab_Percent) then
            Ada.Text_IO.Put_Line ("Setting cab #"
                                  & Cabs.Control_Cab_ID'Image (Test_Cab)
                                  & " to " & Cabs.Percent'Image
                                  (Test_Inputs (Cab_Percent))
                                  & "%: Failed");
         else
            Ada.Text_IO.Put_Line ("Setting cab #"
                      & Cabs.Control_Cab_ID'Image (Test_Cab)
                      & " to " & Cabs.Percent'Image
                      (Test_Inputs (Cab_Percent)) & "%.");
            Ada.Text_IO.Put_Line ("Cab Value = " & Cabs.Percent'Image
                                  (Test_Output) & "%.");
         end if;
         Ada.Text_IO.Put_Line ("Press [enter] to continue");
         Ada.Text_IO.Skip_Line;
      end loop;
   end loop;

   -- Give the user the option to run self tests and access to all cab functions
   loop
      Ada.Text_IO.Put_Line ("What would you like to do?");
      Ada.Text_IO.Put_Line ("(s) Set Cab Value");
      Ada.Text_IO.Put_Line ("(l) Set Cab Limit");
      Ada.Text_IO.Put_Line ("Anything else to quit");
      Ada.Text_IO.Get (Char);
      Ada.Text_IO.Put_Line ("Enter the cab number");
      Cab_IO.Get (Cab_Num);
      case Char is
         when 's' =>
            Cabs.Get (Cab   => Cab_Num,
                      Value => Cab_Value);

            Ada.Text_IO.Put_Line ("The current value for cab " &
                                    Cabs.Cab_ID'Image (Cab_Num) & " is " &
                                    Cabs.Percent'Image (Cab_Value) & ".");

            Ada.Text_IO.Put_Line ("Enter in a value for the cab");
            Percent_IO.Get (Cab_Value);
            Cabs.Set (Cab   => Cab_Num,
                      Value => Cab_Value);
         when 'l' =>
            Cabs.Get_Limit (Cab   => Cab_Num,
                            Value => Cab_Limit);
            Ada.Text_IO.Put_Line ("The current limit for cab " &
                                    Cabs.Cab_ID'Image (Cab_Num) & " is " &
                                    Cabs.Percent'Image (Cab_Limit) & ".");
            Ada.Text_IO.Put_Line ("Enter in the limit for the cab");
            Percent_IO.Get (Cab_Limit);
            Cabs.Set_Limit (Cab   => Cab_Num,
                            Value => Cab_Limit);
         when others =>
            exit;
      end case;
   end loop;
   Ada.Text_IO.Put_Line ("---- Tests End ----");
end Cabs_Test;
