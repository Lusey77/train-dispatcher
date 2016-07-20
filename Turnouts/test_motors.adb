with Ada.Text_IO;
with Layout;
with Motors;
with MaRTE_OS;
procedure Test_Motors is

   package Turn_IO is new Ada.Text_IO.Enumeration_IO
     (Enum => Layout.Turn_Choice);

   package Turnout_IO is new Ada.Text_IO.Integer_IO
     (Num => Layout.Turnout_ID);

   Turnout   : Layout.Turnout_ID;
   Direction : Layout.Turn_Choice;

begin

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


end Test_Motors;
