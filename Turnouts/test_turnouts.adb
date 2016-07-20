--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Ethan Morisette ------------------------------------------------
-- Tests      : Turnouts -------------------------------------------------------
-- Updated    : 07 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Turnouts;
with Layout;
with Ada.Text_IO;
with Callback_Package;
with MaRTE_OS;
procedure Test_Turnouts is

   package Turn_IO is new Ada.Text_IO.Enumeration_IO
     (Enum => Layout.Turn_Choice);

   package Turnout_IO is new Ada.Text_IO.Integer_IO
     (Num => Layout.Turnout_ID);

   Turnout   : Layout.Turnout_ID;
   Direction : Layout.Turn_Choice;
   Char      : Character;
   Status    : Turnouts.Status_Rec;


begin

   Turnouts.Set_Failure_Callback (To => Callback_Package.Put_Failure'Access);
   Turnouts.Set_Recovery_Callback (To => Callback_Package.Put_Recovery'Access);
   Turnouts.Set_Change_Callback (To => Callback_Package.Put_Change'Access);

   loop
      Ada.Text_IO.Put_Line ("What would you like to do");
      Ada.Text_IO.Put_Line ("(G)et the status of a turnout");
      Ada.Text_IO.Put_Line ("(D)irection of a turnout");
      Ada.Text_IO.Put_Line ("(C)hange a turnout");
      Ada.Text_IO.Put_Line ("(S)hutdown");
      Ada.Text_IO.Get (Char);
      if Char /= 'S' then
         Ada.Text_IO.Put_Line ("What turnout would you like to perform this " &
                                 "operation on?");
         Turnout_IO.Get (Turnout);
      end if;
      case Char is
         when 'G' =>
            Status := Turnouts.Status (Turnout => Turnout);
            Ada.Text_IO.Put ("The current state of the turnout selected is: ");
            Ada.Text_IO.Put_Line (Turnouts.Turnout_State_Type'Image
                                  (Status.Current));
            Ada.Text_IO.Put ("The desired direction of the turnout is: ");
            Ada.Text_IO.Put_Line (Layout.Turn_Choice'Image (Status.Desired));
         when 'D' =>
            Ada.Text_IO.Put ("The direction of the turnout selected is: ");
            Ada.Text_IO.Put_Line (Layout.Turn_Choice'Image
                                  (Turnouts.Direction_Of (Turnout => Turnout)));
         when 'C' =>
            Ada.Text_IO.Put ("Enter a direction");
            Turn_IO.Get (Direction);
            Turnouts.Set (Requestor => 1,
                          Turnout   => Turnout,
                          Direction => Direction);
         when 'S' =>
            Turnouts.Shut_Down;
            exit;
         when others =>
            null;
      end case;
   end loop;

end Test_Turnouts;
