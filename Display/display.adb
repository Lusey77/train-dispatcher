--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : John McCormick -------------------------------------------------
-- Updated    : May 2008 -------------------------------------------------------
--------------------------------------------------------------------------------
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;  -- For '*' operator
with Ada.Text_IO;             use Ada.Text_IO;
with Console_Management;      use Console_Management;
with Bounded_Queue;
with Common_Units;
use type Common_Units.Percent;
with Layout;
use type Layout.Block_Polarity;
pragma Elaborate_All (Console_Management);

package body Display is

   use Trains;  -- Make stop reasons directly visible

   use type Layout.Turn_Choice;

   package Direction_IO is new
     Ada.Text_IO.Enumeration_IO (Trains.Direction_Type);


   type Message_Type is (Engineer_Skill, Train_Name, Train_Direction,
                         Train_Throttle, Train_Blocks, Train_Stop,
                         Turnout_Change, Error);
   Max_List : constant := 10;
--     subtype Block_Range is Integer range 0 .. Max_List;
--     subtype Block_Array is Layout.Block_Array (1 .. Max_List);

   type Message_Rec (Which : Message_Type := Error) is
      record
         Train : Trains.Train_ID;
         case Which is
            when Engineer_Skill =>
               Skill          : Engineers.Skill;
            when Train_Name =>
               Name           : Locomotives.Loco_String;
            when Train_Direction =>
               Direction      : Trains.Direction_Type;
            when Train_Throttle =>
               Throttle       : Common_Units.Percent;
            when Train_Blocks =>
               Blocks         : Layout.Block_List (6);
            when Train_Stop =>
               Status         : Trains.Stop_Rec;
            when Turnout_Change =>
               Turnout        : Layout.Turnout_ID;
               Turn_Direction : Layout.Turn_Choice;
               Moving         : Boolean;
            when Error =>
               Error_Message  : String (1 .. 75);
         end case;
      end record;

   -----------------------------------------------------------------------------
   -- Define a protected buffer of messages
   -----------------------------------------------------------------------------
   package Message_Queue is new Bounded_Queue (Element_Type => Message_Rec);
   use Message_Queue;

   protected Message_Buffer is
      procedure Put (Message : in  Message_Rec);
      procedure Enable;
      procedure Disable;
      entry Get (Message : out Message_Rec);
   private
      Enabled      : Boolean := False;
      The_Messages : Message_Queue.Queue_Type (Max_Size => 64);
   end Message_Buffer;
   ----------------------------------------------
   protected body Message_Buffer is
      ---------------------------------
      procedure Put (Message : in  Message_Rec) is
         Trash : Message_Rec;
      begin
         if Enabled then -- Only enqueue if buffer is enabled
            if Full (The_Messages) then  -- Is the queue full?
               -- Throw out the oldest message
               Dequeue (Queue => The_Messages,
                        Item  => Trash);
            end if;
            Enqueue (Queue => The_Messages,
                     Item  => Message);
         end if;
      end Put;
      ---------------------------------
      procedure Enable is
      begin
         Clear (The_Messages);
         Enabled := True;
      end Enable;
      ---------------------------------
      procedure Disable is
      begin
         Enabled := False;
         Clear (The_Messages);
      end Disable;
      ---------------------------------
      entry Get (Message : out Message_Rec) when not Empty (The_Messages) is
      begin
         Dequeue (Queue => The_Messages,
                  Item  => Message);
      end Get;
   end Message_Buffer;


   -----------------------------------------------------------------------------
   function To_String (Item : in Engineers.Skill) return String is
   -- Convert a skill level to a string with 1st character capitalized
      Result : String := Engineers.Skill'Image (Item);
   begin
      Result := To_Lower (Result);
      Result (1) := To_Upper (Result (1));
      return Result;
   end To_String;

   -----------------------------------------------------------------------------
   task Display_Task;
   task body Display_Task is
      type Err_Num_Type is mod 256;
      -- The message with information to display
      Message : Message_Rec;
      -- Row and Column on the screen
      Row : Console_Management.Rows;
      Col : Console_Management.Columns;

      Err_Num : Err_Num_Type;       -- For general error messages
      Count   : Natural;            -- For listing failed turnouts

   begin
      Message_Buffer.Disable;
      Err_Num := 0;
      loop
         -- Get a message from the buffer
         Message_Buffer.Get (Message);

         -- Display the message information
         case Message.Which is

            when Engineer_Skill =>
               Set_Text_Color (LightBlue);
               Row := 0 + 7 * (Natural (Message.Train) - 1);
               Col := 28;
               Set_Cursor (Row, Col);
               Put ("   " & To_String (Message.Skill));
               Set_Text_Color (White);

            when Train_Name =>
               Row := 1 + 7 * (Natural (Message.Train) - 1);
               Col := 2;
               Set_Cursor (Row, Col);
               Put (Message.Name);

            when Train_Direction =>
               Row := 2 + 7 * (Natural (Message.Train) - 1);
               Col := 2;
               Set_Cursor (Row, Col);
               Put ("Running ");
               if Message.Direction = Trains.Forward then
                  Set_Text_Color (LightGreen);
               else
                  Set_Text_Color (LightRed);
               end if;
               Direction_IO.Put (Item  => Message.Direction,
                                 Width => 9,
                                 Set   => Ada.Text_IO.Lower_Case);
               Set_Text_Color (White);

            when Train_Throttle =>
               Row := 2 + 7 * (Natural (Message.Train) - 1);
               Col := 19;
               Set_Cursor (Row, Col);
--                 if Message.Throttle > 95.0 then
--                    Put ("Balls to the Wall");
--                 else
                  Put ("at ");
                  Put (Item  => Integer (Message.Throttle),
                       Width => 3);
                  Put ("% throttle ");
--               end if;

            when Train_Blocks =>
               Row := 3 + 7 * (Natural (Message.Train) - 1);
               Col := 2;
               Set_Cursor (Row, Col);
               Put ("Blocks ");
               -- Display all the block numbers in the list
               for Index in 1 .. Message.Blocks.Size loop
                  Put (Item  => Integer (Message.Blocks.Items (Index).Block),
                       Width => 2);
                  if Message.Blocks.Items (Index).Direction = Layout.Normal then
                     Set_Text_Color (LightGreen);
                     Ada.Text_IO.Put ("N ");
                  else
                     Set_Text_Color (LightRed);
                     Ada.Text_IO.Put ("R ");
                  end if;
                  Set_Text_Color (White);
               end loop;
               -- Fill the rest of the line with blanks
               Put ((Max_List - Message.Blocks.Size) * "  ");

            when Train_Stop =>
               Set_Text_Color (LightRed);
               -- First message line
               Row := 4 + 7 * (Natural (Message.Train) - 1);
               Col := 2;
               Set_Cursor (Row, Col);
               if Message.Status.Reasons (Lost_Caboose) then
                  Put ("Waiting for Lost Caboose and Dispatcher");
               elsif Message.Status.Reasons (Dispatcher_Request) then
                  Put ("Waiting for Dispatcher                 ");
               else -- Nothing to display on this line, erase any old message
                  Put (40 * ' ');
               end if;

               -- Second message line
               Row := Row + 1;
               Set_Cursor (Row, Col);
               if Message.Status.Reasons (Reservation_Failure) then
                  Put ("Waiting for block ");
                  Put (Item  => Integer (Message.Status.Block),
                       Width => 1);
               else -- Nothing to display on this line, erase any old message
                  Put (40 * ' ');
               end if;

               -- Third message line
               Row := Row + 1;
               Set_Cursor (Row, Col);
               if Message.Status.Reasons (Turnout_Failure) then
                  Put ("Waiting for failed turnout");
                  Count := 0;
                  for Turnout in Layout.Turnout_ID loop
                     if Message.Status.Turnouts (Turnout) then
                        Put (Item  => Integer (Turnout),
                             Width => 3);
                        Count := Count + 1;
                        exit when Count = 4;  -- maximum we can display
                     end if;
                  end loop;
               else -- Nothing to display on this line, erase any old message
                  Ada.Text_IO.Put (40 * ' ');
               end if;
               Set_Text_Color (White);

            when Turnout_Change =>
               -- Determine the Row and Column for this turnout's label
               Row := 1  + 2 * ((Natural (Message.Turnout) - 1) / 6);
               Col := 47 + 6 * ((Natural (Message.Turnout) - 1) rem 6);
               Set_Cursor (Row, Col);
               if Message.Moving then
                  Set_Blink;
               end if;
               Set_Text_Color (LightBlue);
               if Message.Turn_Direction = Layout.Right then
                  Put ('R');
               else
                  Put ('L');
               end if;
               Set_Text_Color (White);
               if Message.Moving then
                  Cancel_Blink;
               end if;

            when Error =>
               Row := 22 + Integer (Err_Num) rem 3;
               Set_Cursor (Row, 0);
               Put (Item  => Integer (Err_Num),
                    Width => 3);
               Put (' ' & Message.Error_Message);
               Err_Num := Err_Num + 1;
         end case;
      end loop;
   end Display_Task;


   ----------------------------------------------------------------------------
   -- Local subprograms
   ----------------------------------------------------------------------------
   procedure Put_Box is
      -- Put up the lines and labels on the screeen

      -- Constants for IBM extended ASCII line drawing characters
      UL : constant Character := Character'Val (201);  -- Upper left corner
      UR : constant Character := Character'Val (187);  -- Upper right corner
      LL : constant Character := Character'Val (200);  -- Lower left corner
      LR : constant Character := Character'Val (188);  -- Lower right corner
      LT : constant Character := Character'Val (185);  -- Left side Tee
      RT : constant Character := Character'Val (204);  -- Right side Tee
      TT : constant Character := Character'Val (203);  -- Top side Tee
      BT : constant Character := Character'Val (202);  -- Bottom side Tee
      VL : constant Character := Character'Val (186);  -- Vertical Line
      HL : constant Character := Character'Val (205);  -- Horizontal Line

      -- Location of first turnout number
      Turnout_Base_Row : constant Console_Management.Rows    := 1;
      Turnout_Base_Col : constant Console_Management.Columns := 43;

      -- Row and Column on the screen
      Row : Console_Management.Rows;
      Col : Console_Management.Columns;
   begin
      Clear_Screen;
      Set_Cursor (0, 0);
      Set_LowVideo;
      Set_Text_Background_Color (Black);

      -- The top line
      Put (UL & 41 * HL & TT & 36 * HL & UR);
      -- The three vertical lines
      for Row in 1 .. 20 loop
         Set_Cursor (Row, 0);
         Put (VL);
         Set_Cursor (Row, 42);
         Put (VL);
         Set_Cursor (Row, 79);
         Put (VL);
      end loop;
      -- The bottom line
      Set_Cursor (Row => 21, Column => 0);
      Put (LL & 41 * HL & BT & 36 * HL & LR);
      -- The train separator lines
      Set_Cursor (Row => 7, Column => 0);
      Put (RT & 41 * HL & LT);
      Set_Cursor (Row => 14, Column => 0);
      Put (RT & 41 * HL & LT);
      -- The turnout separater line
      Set_Cursor (Row => 10, Column => 42);
      Put (RT & 36 * HL & LT);

      -- Put the controller labels
      Set_Text_Color (LightBlue);
      Set_Cursor (Row => 0, Column => 5);
      Put ("Train #1   Controller A");
      Set_Cursor (Row => 7, Column => 5);
      Put ("Train #2   Controller B");
      Set_Cursor (Row => 14, Column => 5);
      Put ("Train #3   Controller C");
      Set_Cursor (Row => 0, Column => 56);
      Put ("Turnouts");
      Set_Text_Color (White);

      -- Put the turnout labels
      for Turnout in Layout.Turnout_ID loop
         -- Determine the Row and Column for this turnout's label
         Row := Turnout_Base_Row + 2 * ((Natural (Turnout) - 1) / 6);
         Col := Turnout_Base_Col + 6 * ((Natural (Turnout) - 1) rem 6);
         Set_Cursor (Row, Col);
         Put (Item  => Natural (Turnout),
              Width => 3);
      end loop;

      -- Put the command list
      Set_Cursor (Row => 11, Column => 44);
      Put ("Space - Stop all trains");
      Set_Cursor (Row => 12, Column => 44);
      Put ("nS    - Stop train n");
      Set_Cursor (Row => 13, Column => 44);
      Put ("nG    - Start train n");
      Set_Cursor (Row => 14, Column => 44);
      Put ("nR    - Change turnout n to right");
      Set_Cursor (Row => 15, Column => 44);
      Put ("nL    - Change turnout n to left");
      Set_Cursor (Row => 16, Column => 44);
      Put ("nF    - Free block n reservation");
      Set_Cursor (Row => 17, Column => 44);
      Put ("nE    - Change engineer n skill");
      Set_Cursor (Row => 18, Column => 44);
      Put ("R     - Restart (reinitialize)");
      Set_Cursor (Row => 19, Column => 44);
      Put ("Q     - Quit (shutdown system)");

      -- Put Not In Service messages
      Row := 2;
      Col := 14;
      Set_Text_Color (LightBlue);
      Set_Text_Background_Color (Black);
      for Train in 1 .. 3 loop
         Set_Cursor (Row, Col);
         Put ("Not In Service");
         Row := Row + 7;
      end loop;

      Set_Text_Color (White);
      Set_HighVideo;
   end Put_Box;


   ----------------------------------------------------------------------------
   -- Externally defined subprograms
   ----------------------------------------------------------------------------
   procedure Enable is
   begin
      Put_Box;
      Message_Buffer.Enable;
   end Enable;

   ----------------------------------------------------------------------------
   procedure Disable is
   begin
      Message_Buffer.Disable;
   end Disable;

   ----------------------------------------------------------------------------
   procedure Put (Turnout   : in Layout.Turnout_ID;
                  Direction : in Layout.Turn_Choice;
                  Moving    : in Boolean) is
      Message : Message_Rec (Which => Turnout_Change);
   begin
      Message.Turnout        := Turnout;
      Message.Turn_Direction := Direction;
      Message.Moving         := Moving;
      Message_Buffer.Put (Message);
   end Put;

   ----------------------------------------------------------------------------
   procedure Put (Train : in Trains.Train_ID;
                  Name  : in Locomotives.Loco_String) is
      Message : Message_Rec (Which => Train_Name);
   begin
      Message.Train := Train;
      Message.Name  := Name;
      Message_Buffer.Put (Message);
   end Put;

   ----------------------------------------------------------------------------
   procedure Put (Train : in Trains.Train_ID;
                  Skill : in Engineers.Skill) is
      Message : Message_Rec (Which => Engineer_Skill);
   begin
      Message.Train := Train;
      Message.Skill := Skill;
      Message_Buffer.Put (Message);
   end Put;

   ----------------------------------------------------------------------------
   procedure Put (Train     : in Trains.Train_ID;
                  Direction : in Trains.Direction_Type) is
      Message : Message_Rec (Which => Train_Direction);
   begin
      Message.Train     := Train;
      Message.Direction := Direction;
      Message_Buffer.Put (Message);
   end Put;

   ----------------------------------------------------------------------------
   procedure Put (Train    : in Trains.Train_ID;
                  Throttle : in Common_Units.Percent) is
      Message : Message_Rec (Which => Train_Throttle);
   begin
      Message.Train    := Train;
      Message.Throttle := Throttle;
      Message_Buffer.Put (Message);
   end Put;

   ----------------------------------------------------------------------------
   procedure Put (Train  : in Trains.Train_ID;
                  Status : in Trains.Stop_Rec) is
      Message : Message_Rec (Which => Train_Stop);
   begin
      Message.Train  := Train;
      Message.Status := Status;
      Message_Buffer.Put (Message);
   end Put;

   ----------------------------------------------------------------------------
   procedure Put (Train  : in Trains.Train_ID;
                  Blocks : in Layout.Block_List) is
      Message : Message_Rec (Which => Train_Blocks);
   begin
      Message.Train  := Train;
      Message.Blocks := Blocks;
      Message_Buffer.Put (Message);
   end Put;

   ----------------------------------------------------------------------------
   procedure Put_Error (Error_Message : in String) is
      Message : Message_Rec (Which => Error);
   begin
      Ada.Strings.Fixed.Move (Source  => Error_Message,
                              Target  => Message.Error_Message,
                              Drop    => Ada.Strings.Right,
                              Justify => Ada.Strings.Left);
      Message_Buffer.Put (Message);
   end Put_Error;

end Display;
