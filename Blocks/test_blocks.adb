--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullivan ------------------------------------------------
-- Tests      : Engineers ------------------------------------------------------
-- Updated    : 22 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Ada.Text_IO;
with Blocks;
with Layout;
with Cabs;
with Trains;
with MaRTE_OS;
procedure Test_Blocks is

   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);
   package Dir_IO is new Ada.Text_IO.Enumeration_IO (Layout.Block_Polarity);
   package Cab_IO is new Ada.Text_IO.Integer_IO (Cabs.Cab_ID);
   package Train_IO is new Ada.Text_IO.Integer_IO (Trains.Request_ID);

   Char     : Character;
   Block    : Layout.Block_ID;
   Cab      : Cabs.Cab_ID;
   Dir      : Layout.Block_Polarity;
   Train    : Trains.Request_ID;
   Successful : Boolean;

begin

   Ada.Text_IO.Put_Line ("Please ensure that there are no trains on the track" &
                           " and make sure the power block switch is off!");
   Ada.Text_IO.Put_Line ("Press [enter] to begin");
   Ada.Text_IO.Skip_Line;

   loop
      Ada.Text_IO.Put_Line ("(R)eserve a block");
      Ada.Text_IO.Put_Line ("(F)ree a block");
      Ada.Text_IO.Put_Line ("(S)how reservations");
      Ada.Text_IO.Put_Line ("(P)ower a block");
      Ada.Text_IO.Put_Line ("(C)heck block power");
      Ada.Text_IO.Get (Char);
      Ada.Text_IO.Put_Line ("What block would you like to perform this " &
                              "operation on?");
      case Char is
         when 'R' =>
            Ada.Text_IO.Put_Line ("What block would you like to perform this " &
                                    "operation on?");
            Block_IO.Get (Block);
            Ada.Text_IO.Put_Line ("Who is requesting(enter a number between " &
                                    "1 - 3)");
            Train_IO.Get (Train);
            Blocks.Reserve (Block     => Block,
                            Requestor => Train,
                            Success   => Successful);
            if Successful then
               Ada.Text_IO.Put_Line ("The reservation was successful");
            else
               Ada.Text_IO.Put_Line ("The reservation failed");
            end if;
         when 'F' =>
            Ada.Text_IO.Put_Line ("What block would you like to perform this " &
                                    "operation on?");
            Block_IO.Get (Block);
            Ada.Text_IO.Put_Line ("Who is requesting(enter a number between " &
                                    "0 - 3)");
            Train_IO.Get (Train);
            Blocks.Free (Block     => Block,
                         Requestor => Train);
         when 'S' =>
            Blocks.Print_Blocks;
         when 'P' =>
            Ada.Text_IO.Put_Line ("What block would you like to perform this " &
                                    "operation on?");
            Block_IO.Get (Block);
            Ada.Text_IO.Put_Line ("What direction would you like to power the" &
                                    " block");
            Dir_IO.Get (Dir);
            Ada.Text_IO.Put_Line ("Who is requesting(enter a number between " &
                                    "0 - 7)");
            Cab_IO.Get (Cab);
            Blocks.Power_Block (Block     => Block,
                                Direction => Dir,
                                Cab       => Cab);
         when 'C' =>
            Ada.Text_IO.Put_Line ("What block would you like to perform this " &
                                    "operation on?");
            Block_IO.Get (Block);
            if Blocks.Is_Powered (Block) then
               Ada.Text_IO.Put_Line ("Block " & Layout.Block_ID'Image (Block) &
                                       " is powered!");
            else
               Ada.Text_IO.Put_Line ("Block " & Layout.Block_ID'Image (Block) &
                                       " is NOT powered!");
            end if;
         when others =>
            exit;
      end case;
   end loop;

end Test_Blocks;
