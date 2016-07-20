with Ada.Text_IO;
with Commands;
with Trains;
with Layout;
with Engineers;
procedure Test_Command is

   Command : Commands.Command_Rec;

   -- Packages for IO of the different ids in the record
   package Train_IO    is new Ada.Text_IO.Integer_IO
     (Num => Trains.Train_ID);

   package Block_IO    is new Ada.Text_IO.Integer_IO
     (Num => Layout.Block_ID);

   package Engineer_IO is new Ada.Text_IO.Integer_IO
     (Num => Engineers.Engineer_ID);

   package Turnout_IO  is new Ada.Text_IO.Integer_IO
     (Num => Layout.Turnout_ID);

begin
   -- Gets a command from the keyboard and displays its corresponding record
   loop
      Ada.Text_IO.Put ("Please enter a command");

      Commands.Get (Command => Command);
      Ada.Text_IO.Put (" ");
      Ada.Text_IO.Put_Line (Commands.Command_Type'Image (Command.Which));
      Ada.Text_IO.Put (" ");
      case Command.Which is
         when Commands.Stop_All | Commands.Restart |
              Commands.Quit | Commands.Error =>
               null;
            when Commands.Stop | Commands.Go =>
               Train_IO.Put (Command.Train);
               Ada.Text_IO.Put (" ");
               Ada.Text_IO.New_Line;
            when Commands.Left | Commands.Right =>
            Turnout_IO.Put (Command.Turnout);
            Ada.Text_IO.Put (" ");
               Ada.Text_IO.New_Line;
            when Commands.Free =>
            Block_IO.Put (Command.Block);
            Ada.Text_IO.Put (" ");
               Ada.Text_IO.New_Line;
            when Commands.Skill =>
            Engineer_IO.Put (Command.Engineer);
            Ada.Text_IO.Put (" ");
               Ada.Text_IO.New_Line;
      end case;
   end loop;

end Test_Command;
