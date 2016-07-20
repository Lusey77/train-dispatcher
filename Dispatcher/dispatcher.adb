--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullivan ------------------------------------------------
-- Tested  By : Ethan Morisette ------------------------------------------------
-- Updated    : 12 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------

with Commands;
with Turnouts;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
with Locomotives;
with Layout;
with Trains;
with Trains.Operations;
use type Trains.Request_ID;
with Layout.Search;
with DoubleTalk;
use type Layout.Block_ID;
with Display;
with Engineers;
with Engineers.Operations;
with Blocks;
with Console_Management;
with Halls;

procedure Dispatcher is

   -- Represents the states of the dispatcher
   type State_Type is (Start_Dispatcher, Setup_Trains, Run_Dispatcher,
                       Shutdown_Dispatcher);

   type Setup_State_Type is (Pick, Locate, Search);

   -- Represents the user options for the polarity of the train
   type Polarity is (N, R);

   -- Represents the user options if they enter the correct info for their train
   type Info_Correct is (Y, N, R, Q);

   -- Represents the user options it they enter invalid info for their train
   type Info_Incorrect is (Y, N, Q);

   -- Represents the input locomotive an caboose when choosing a train
   type User_Train_Rec is
      record
         Locomotive : Layout.Block_ID;
         Caboose    : Layout.Block_ID;
      end record;


   -----------------------------------------------------------------------------
   -- Represents all the packages for IO of enumeration types and integer types-
   -----------------------------------------------------------------------------
   package Polarity_IO  is new Ada.Text_IO.Enumeration_IO
     (Enum => Polarity);
   package Correct_IO   is new Ada.Text_IO.Enumeration_IO
     (Enum => Info_Correct);
   package Incorrect_IO is new Ada.Text_IO.Enumeration_IO
     (Enum => Info_Incorrect);
   package Block_IO     is new Ada.Text_IO.Integer_IO
     (Num => Layout.Block_ID);
   package Percent_IO is new Ada.Text_IO.Integer_IO
     (Num => Locomotives.Percent);
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   procedure Output_Selection_Screen (Train_Num : in Trains.Train_ID) is

   begin
      -- Sets up the screen
      Console_Management.Clear_Screen;
      Console_Management.Set_Text_Color (C => Console_Management.LightGreen);
      Console_Management.Set_Blink;
      Console_Management.Set_Cursor (Row    => 0,
                                     Column => 25);
      Ada.Text_IO.Put_Line ("Locomotive Choice #" & Trains.Train_ID'Image
                            (Train_Num));
      Console_Management.Cancel_Blink;
      Console_Management.Set_Cursor (Row    => 3,
                                     Column => 0);
      Console_Management.Set_Text_Color (C => Console_Management.LightBlue);
      Ada.Text_IO.Put_Line ("The following locomotives are available");
      Console_Management.Set_Cursor (Row    => 5,
                                     Column => 9);
      -- Locomotive headers
      Ada.Text_IO.Put ("Road Name");

      Console_Management.Set_Cursor (Row    => 5,
                                     Column => 30);
      Ada.Text_IO.Put ("Model");

      Console_Management.Set_Cursor (Row    => 5,
                                     Column => 44);
      Ada.Text_IO.Put_Line ("Number");
      Console_Management.Set_Cursor (Row    => 6,
                                     Column => 0);
      Console_Management.Set_Text_Color (C => Console_Management.White);
      -- Outputs the list of locomotives available
      for Train in Locomotives.Available_Locos'Range loop
         Ada.Text_IO.Put (' ');
         Ada.Integer_Text_IO.Put (Train, 1);
         Ada.Text_IO.Set_Col (10);
         Ada.Text_IO.Put_Line
           (Locomotives.Available_Locos (Train).Name);
      end loop;
      -- Allows for the option of "Other"
      Ada.Text_IO.Put (' ');
      Ada.Integer_Text_IO.Put (11, 1);
      Ada.Text_IO.Set_Col (10);
      Ada.Text_IO.Put_Line ("Other");

      Ada.Text_IO.New_Line;
      Console_Management.Set_Text_Color (C => Console_Management.LightBlue);
      -- Gets the number input
      Ada.Text_IO.Put_Line ("Enter the line number from the above" &
                            " table of the locomotive pulling Train #"
                            & Trains.Train_ID'Image (Train_Num));
      Console_Management.Set_Text_Color (C => Console_Management.White);
   end Output_Selection_Screen;

   procedure Get_Num_Trains (State      : out State_Type;
                             Num_Trains : out Trains.Train_ID) is
      -- Outputs screen and validates the users input
      -- Each iteration, output screen and get one input value
   begin
      DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                        ("Welcome to the Redding Railroad Software."),
                        Voice  => DoubleTalk.Vader);
      loop
         -- Sets up the screen
         Console_Management.Clear_Screen;
         Console_Management.Set_Text_Color (C => Console_Management.LightBlue);
         Console_Management.Set_Cursor (Row    => 10,
                                        Column => 20);
         Ada.Text_IO.Put_Line ("Reading Railroad Software");
         Console_Management.Set_Cursor (Row    => 12,
                                        Column => 0);
         -- Asks for the amount of trains needed (Num_Trains)
         Ada.Text_IO.Put_Line
           ("Please choose whether you are running 1, 2, or 3 trains.");

         Ada.Text_IO.Put_Line
           ("There must be at least one unoccupied block" &
              " between each pair of trains");

         Console_Management.Set_Cursor (Row    => 16,
                                        Column => 0);
         Ada.Text_IO.Put_Line
           ("How many trains do you wish to run? (1, 2, or 3)");

         Console_Management.Set_Text_Color (C => Console_Management.White);


         -----------------------------------------------------------------
         -- Validates input                                             --
         -----------------------------------------------------------------
         Num_Trains_Validation_Block :
         begin
            Ada.Integer_Text_IO.Get (Integer (Num_Trains));
            State := Setup_Trains;
            exit;
         exception
            when Constraint_Error =>
               Console_Management.Clear_Screen;
               Console_Management.Set_Cursor (Row    => 10,
                                              Column => 0);
               Ada.Text_IO.Put_Line
                 ("Invalid range. Please enter a number between"
                  & " 1 and 3");
            when Ada.IO_Exceptions.Data_Error =>
               Ada.Text_IO.Skip_Line;
               Console_Management.Clear_Screen;
               Console_Management.Set_Cursor (Row    => 10,
                                              Column => 0);
               Ada.Text_IO.Put_Line
                 ("Invalid Data. Please enter a number between"
                  & " 1 and 3");

            when others =>
               Ada.Text_IO.Skip_Line;
               Ada.Text_IO.New_Page;
         end Num_Trains_Validation_Block;
      end loop;
   end Get_Num_Trains;

   procedure Get_Location (Train_Num  : in     Trains.Train_ID;
                           Location   :    out User_Train_Rec) is
      Loco : Layout.Block_ID;
      Cabo : Layout.Block_ID;
   begin
      -- Sets up the location screen
      Console_Management.Clear_Screen;
      Console_Management.Set_Text_Color (C => Console_Management.LightGreen);
      Console_Management.Set_Blink;
      Console_Management.Set_Cursor (Row    => 0,
                                     Column => 25);
      Ada.Text_IO.Put_Line ("Location of Train #" & Trains.Train_ID'Image
                            (Train_Num));
      Console_Management.Cancel_Blink;
      Console_Management.Set_Text_Color (C => Console_Management.LightBlue);
      -----------------------------------------------------------------
      -- Prompt user for the location of thier locomotive            --
      -----------------------------------------------------------------
      Console_Management.Set_Cursor (Row    => 10,
                                     Column => 0);
      Ada.Text_IO.Put_Line ("On which block is the locomotive" &
                              " pulling train" & Trains.Train_ID'Image
                              (Train_Num) & " located?");
      Console_Management.Set_Text_Color (C => Console_Management.White);

      -- Validation Loop for Locomotive (Location.Locomotive)
      loop
         Train_One_Validation_Block :
         begin
            Block_IO.Get (Loco);
            exit;
         exception
            when Constraint_Error =>
               Ada.Text_IO.Put_Line
                 ("Invalid range. Please enter a number between"
                  & " 1 and 40");
            when Ada.IO_Exceptions.Data_Error =>
               Ada.Text_IO.Skip_Line;
               Ada.Text_IO.Put_Line
                 ("Invalid Data. Please enter a number between"
                  & " 1 and 40");

            when others =>
               Ada.Text_IO.Skip_Line;
               Ada.Text_IO.New_Page;
         end Train_One_Validation_Block;
      end loop;
      Ada.Text_IO.New_Line (2);
      -----------------------------------------------------------------
      -- Prompt user for the location of their caboose               --
      -----------------------------------------------------------------
      Console_Management.Set_Text_Color (C => Console_Management.LightBlue);
      Ada.Text_IO.Put_Line ("On which block is the Caboose" &
                              " pulling train" & Trains.Train_ID'Image
                              (Train_Num) & " located?");
      Console_Management.Set_Text_Color (C => Console_Management.White);

      -- Validation Loop for Caboose (Location.Caboose)
      loop
         Train_Two_Validation_Block :
         begin
            Block_IO.Get (Cabo);
            exit;
         exception
            when Constraint_Error =>
               Ada.Text_IO.New_Page;
               Ada.Text_IO.New_Line (10);
               Ada.Text_IO.Put_Line
                 ("Invalid range. Please enter a number between"
                  & " 1 and 40");
            when Ada.IO_Exceptions.Data_Error =>
               Ada.Text_IO.Skip_Line;
               Ada.Text_IO.New_Page;
               Ada.Text_IO.New_Line (10);
               Ada.Text_IO.Put_Line
                 ("Invalid Data. Please enter a number between"
                  & " 1 and 40");

            when others =>
               Ada.Text_IO.Skip_Line;
               Ada.Text_IO.New_Page;
         end Train_Two_Validation_Block;
      end loop;
      Location.Locomotive := Loco;
      Location.Caboose    := Cabo;
   end Get_Location;

   procedure Pick_Trains (Train_Num   : in     Trains.Train_ID;
                          Train_Input :    out Locomotives.Loco_Rec) is
      Other_Train_Loco     : String (1 .. 21) := (others => ' ');
      Other_Train_Model    : String (1 .. 14) := (others => ' ');
      Other_Train_Number   : String (1 .. 5) := (others => ' ');
      Loco_Length          : Natural;
      Model_Length         : Natural;
      Number_Length        : Natural;
      Other_Train_Throttle : Locomotives.Percent;
      Other_Train_Name     : Locomotives.Loco_String;
      User_Input           : Integer;
   begin
      -----------------------------------------------------------------
      -- Outputs a list of the available locomotive and prompts the  --
      -- user for a selection                                        --
      -----------------------------------------------------------------
      Output_Selection_Screen (Train_Num);
      -----------------------------------------------------------------
      -- Validation loop for user input                              --
      -----------------------------------------------------------------
      loop
         Train_Choice_Validation_Block :
         begin
            Ada.Integer_Text_IO.Get (User_Input);

            -- If the user picked "Other"
            if User_Input = 11 then
               Ada.Text_IO.Skip_Line;
               Console_Management.Clear_Screen;
               Console_Management.Clear_Screen;
               Console_Management.Set_Text_Color
                 (C => Console_Management.LightGreen);
               Console_Management.Set_Blink;
               Console_Management.Set_Cursor
                 (Row    => 0,
                  Column => 25);
               Ada.Text_IO.Put_Line
                 ("Setting up custom train");
               Console_Management.Cancel_Blink;

               -- Gathers a Locomotive, Model number, and Throttle for
               -- Custom Train
               Console_Management.Set_Text_Color
                 (C => Console_Management.LightBlue);
               Ada.Text_IO.Put_Line ("Please give the road name: ");
               Console_Management.Set_Text_Color
                 (C => Console_Management.White);
               Ada.Text_IO.Get_Line (Other_Train_Loco, Loco_Length);
               Console_Management.Set_Text_Color
                 (C => Console_Management.LightBlue);
               Ada.Text_IO.Put_Line ("Please enter the model: ");
               Console_Management.Set_Text_Color
                 (C => Console_Management.White);
               Ada.Text_IO.Get_Line (Other_Train_Model, Model_Length);
               Console_Management.Set_Text_Color
                 (C => Console_Management.LightBlue);
               Ada.Text_IO.Put_Line ("Please enter the locomotive number: ");
               Console_Management.Set_Text_Color
                 (C => Console_Management.White);
               Ada.Text_IO.Get_Line (Other_Train_Number, Number_Length);
               Console_Management.Set_Text_Color
                 (C => Console_Management.LightBlue);
               Ada.Text_IO.Put_Line ("Please enter the throttle percent: ");
               Console_Management.Set_Text_Color
                 (C => Console_Management.White);
               Percent_IO.Get (Other_Train_Throttle);




               -- Set the train_rec equal to other train
               Other_Train_Name := Other_Train_Loco & Other_Train_Model &
                 Other_Train_Number;

               Train_Input.Name := Other_Train_Name;
               Train_Input.Minimum_Throttle := Other_Train_Throttle;
            else
               -- Otherwise set the train rec equal to train chosen
               Train_Input := Locomotives.Available_Locos (User_Input);
            end if;

            exit;
         -- Validation block (Must be between 1 and 11)
         exception
            when Constraint_Error =>
               Ada.Text_IO.New_Page;
               Ada.Text_IO.New_Line (10);
               Ada.Text_IO.Put_Line
                 ("Invalid range. Please enter a number between"
                  & " 1 and 11");
            when Ada.IO_Exceptions.Data_Error =>
               Ada.Text_IO.Skip_Line;
               Ada.Text_IO.New_Page;
               Ada.Text_IO.New_Line (10);
               Ada.Text_IO.Put_Line
                 ("Invalid Data. Please enter a number between"
                  & " 1 and 11");

            when others =>
               Ada.Text_IO.Skip_Line;
               Ada.Text_IO.New_Page;
         end Train_Choice_Validation_Block;
      end loop;
   end Pick_Trains;

   procedure Search_Tracks (Location       : in     User_Train_Rec;
                            Train_Blocks   :    out Layout.Block_List;
                            Train_Turnouts :    out Layout.Search.Turnout_List;
                            Train_Success  :    out Boolean) is
      Train_Polarity : Polarity;
   begin
      -- If Loco = Caboose prompt user for the direction of the train
      if Location.Locomotive = Location.Caboose then
         Console_Management.Set_Text_Color
                 (C => Console_Management.LightBlue);
         Ada.Text_IO.Put_Line ("What is the polarity of the train?"
                               & " (N)ormal or (R)everse");
         Console_Management.Set_Text_Color
                 (C => Console_Management.White);
         loop
            Train_Polarity_Validation_Block :
            begin
               -- Get the direction from the user and add the block
               -- to the block list
               Polarity_IO.Get (Train_Polarity);
               Train_Blocks.Size := 1;
               if Train_Polarity = N then
                  Train_Blocks.Items (1) := (Location.Caboose, Layout.Normal);
               else
                  Train_Blocks.Items (1) := (Location.Caboose, Layout.Reverze);
               end if;
               Train_Turnouts.Size := 0;
               Train_Success := True;
               exit;
            exception
               when others =>
                  Ada.Text_IO.New_Page;
                  Ada.Text_IO.New_Line (10);
                  Ada.Text_IO.Put_Line
                    ("Invalid polarity. Please enter"
                     & " N for Normal or R for Reverse");
            end Train_Polarity_Validation_Block;
         end loop;
      else
         -- Else run the search and find all the blocks beneath the
         -- train as well as all the turnouts
         Layout.Search.Blocks_Beneath (Loco     => Location.Locomotive,
                                       Caboose  => Location.Caboose,
                                       Blocks   => Train_Blocks,
                                       Turnouts => Train_Turnouts,
                                       Success  => Train_Success);

      end if;
   end Search_Tracks;

   procedure Verify_Train (Location       : in     User_Train_Rec;
                           Train_Blocks   : in     Layout.Block_List;
                           Train_Num      : in     Trains.Train_ID;
                           User_Pick      : in     Locomotives.Loco_Rec;
                           Verified       :    out Info_Correct) is
   begin
      -- Set up output screen
      Console_Management.Clear_Screen;
      Console_Management.Set_Text_Color (C => Console_Management.LightGreen);
      Console_Management.Set_Blink;
      Console_Management.Set_Cursor (Row    => 0,
                                     Column => 25);
      Ada.Text_IO.Put_Line ("Confirmation of Train #"
                            & Trains.Train_ID'Image (Train_Num));
      Console_Management.Cancel_Blink;
      Ada.Text_IO.New_Line (4);

      Console_Management.Set_Text_Color
        (C => Console_Management.LightBlue);

      Ada.Text_IO.Put_Line ("Train #" & Trains.Train_ID'Image
                           (Train_Num));
      Ada.Text_IO.Put_Line ("Confirmation of Information");
      Ada.Text_IO.New_Line;

      Console_Management.Set_Text_Color
                 (C => Console_Management.White);
      Ada.Text_IO.Put_Line (User_Pick.Name);


      -- Give the user information on the screen
      -- For visual confirmation
      Ada.Text_IO.Put_Line ("Locomotive on block " &
                            Layout.Block_ID'Image
                           (Location.Locomotive));
      Ada.Text_IO.Put_Line ("Caboose on block " &
                            Layout.Block_ID'Image
                           (Location.Caboose));
      Ada.Text_IO.Put ("Train occcupies blocks ");

      -- Output the blocks the train should be occupying
      -- According to the location the user input
      for Count in 1 .. (Train_Blocks.Size - 1) loop
         Ada.Text_IO.Put (Layout.Block_ID'Image
                         (Train_Blocks.Items
                         (Count).Block) & ",");
      end loop;
      -- Gather response (Info_Correct)
      Ada.Text_IO.Put_Line (Layout.Block_ID'Image
                           (Train_Blocks.Items
                           (Train_Blocks.Size).Block));
      Ada.Text_IO.New_Line;
      Console_Management.Set_Text_Color
                 (C => Console_Management.LightBlue);
      Ada.Text_IO.Put_Line ("Is this information correct? " &
                              "Enter one of the following:");
      Console_Management.Set_Text_Color
                 (C => Console_Management.White);
      Ada.Text_IO.Put_Line ("   Y = yes, the information for " &
                            "this train is correct");
      Ada.Text_IO.Put_Line ("   N = no,  I wish to enter " &
                            "different information for this " &
                            "train");
      Ada.Text_IO.Put_Line ("   R = no,  I wish to restart " &
                            "setting up from the beginning");
      Ada.Text_IO.Put_Line ("   Q = no,  I wish to terminate " &
                              "this operating session");
      Console_Management.Set_Text_Color
        (C => Console_Management.White);

      -- Validation Block
      loop
         User_Option_Validation_Block_1 :
         begin
            Correct_IO.Get (Verified);
            exit;
         exception
            when others =>
               Ada.Text_IO.New_Page;
               Ada.Text_IO.New_Line (10);
               Ada.Text_IO.Put_Line
                 ("Invalid data. Please enter Y, N, R, or Q");
         end User_Option_Validation_Block_1;
      end loop;
   end Verify_Train;

   procedure Invalid_Train (Verified : out Info_Incorrect) is
   begin
      -- Set up output screen
      Console_Management.Clear_Screen;
      Console_Management.Set_Text_Color (C => Console_Management.LightGreen);
      Console_Management.Set_Blink;
      Console_Management.Set_Cursor (Row    => 0,
                                     Column => 25);
      Ada.Text_IO.Put_Line ("Error with train choice");
      Console_Management.Cancel_Blink;
      Console_Management.Clear_Screen;
      Console_Management.Set_Text_Color
                 (C => Console_Management.LightBlue);
      Ada.Text_IO.Put_Line ("The maximum number of blocks"
                           & " of blocks beneath a train"
                           & " is 3");
      Ada.Text_IO.Put_Line ("There are more than 3 blocks"
                           & " beneath your train");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Would you like to reenter the"
                           & " information?");
      Ada.Text_IO.New_Line (2);
      Console_Management.Set_Text_Color
        (C => Console_Management.White);

      -- Gather user response (Info_Incorrect)
      Ada.Text_IO.Put_Line ("   Y = yes, I wish to reenter"
                            & " this train's location.");
      Ada.Text_IO.Put_Line ("   N = no,  I wish to restart"
                           & " set up from the beginning");
      Ada.Text_IO.Put_Line ("   Q = no,  I wish to"
                            & " terminate"
                            & " this operating session.");

      -- Validation block
      loop
         User_Option_Validation_Block_2 :
         begin
            Incorrect_IO.Get (Verified);
            exit;
         exception
            when others =>
               Ada.Text_IO.New_Page;
               Ada.Text_IO.New_Line (10);
               Ada.Text_IO.Put_Line
                 ("Invalid data. Please enter Y, N, or Q");
         end User_Option_Validation_Block_2;
      end loop;
   end Invalid_Train;

   function Train_Conflict_Screen (Train : in Trains.Train_ID)
                                   return Info_Incorrect is

      Char : Info_Incorrect;

   begin
      -- Set up output screen (If conflicting train vs invalid train)
      Console_Management.Set_Text_Color
                 (C => Console_Management.LightBlue);
      Ada.Text_IO.Put_Line ("The location information for Train " &
                            Trains.Train_ID'Image (Train) & " is not " &
                              "valid because it conflicts with a train you " &
                              "entered earlier");
      Ada.Text_IO.New_Line (3);
      Ada.Text_IO.Put_Line ("Would you like to reenter the"
                            & " information?");
      Console_Management.Set_Text_Color
                 (C => Console_Management.White);
      Ada.Text_IO.New_Line (2);
      -- Gather user input (Info_Incorrect)
      Ada.Text_IO.Put_Line ("   Y = yes, I wish to reenter"
                            & " this train's location.");
      Ada.Text_IO.Put_Line ("   N = no,  I wish to restart"
                           & " set up from the beginning");
      Ada.Text_IO.Put_Line ("   Q = no,  I wish to"
                            & " terminate"
                            & " this operating session.");

      -- Validation block
      loop
         User_Option_Validation_Block_3 :
         begin
            Incorrect_IO.Get (Char);
            return Char;
         exception
            when others =>
               Ada.Text_IO.New_Page;
               Ada.Text_IO.New_Line (10);
               Ada.Text_IO.Put_Line
                 ("Invalid data. Please enter Y, N, or Q");
         end User_Option_Validation_Block_3;
      end loop;
   end Train_Conflict_Screen;

   procedure Generate_Initial_Display (Num_Trains   : in Trains.Train_ID)
   is
      Turnout_Direction : Layout.Turn_Choice;
      Turnout_Moving    : Boolean;
      Train_Rec         : Locomotives.Loco_Rec;
   begin
      -- Generations the initial display for information to output to
      delay 0.5;
      Display.Enable;
      delay 0.5;

      -- Sets the turnouts to there location
      for Turnout in Layout.Turnout_ID loop
         case Turnouts.Status (Turnout).Current is
            -- Fully left is left and not moving
            when Turnouts.Fully_Left =>
               Turnout_Direction := Layout.Left;
               Turnout_Moving    := False;
            -- Fully right is right and not moving
            when Turnouts.Fully_Right =>
               Turnout_Direction := Layout.Right;
               Turnout_Moving    := False;
            -- Moving left is left and moving (blinking)
            when Turnouts.Moving_Left =>
               Turnout_Direction := Layout.Left;
               Turnout_Moving    := True;
            -- Moving right is right and moving (blinking)
            when Turnouts.Moving_Right =>
               Turnout_Direction := Layout.Right;
               Turnout_Moving    := True;
         end case;
	 -- Gathered options go to MarTE screen using Display
         Display.Put (Turnout   => Turnout,
                      Direction => Turnout_Direction,
                      Moving    => Turnout_Moving);
      end loop;

      -- Gather information for Trains to output
      for Train in 1 .. Num_Trains loop
         Train_Rec := Trains.Operations.Get_Loco (Train);
         -- Locomotive name
         Display.Put (Train => Train,
                      Name => Train_Rec.Name);

         -- Engineer skill (Top right of box)
         Display.Put (Train => Train,
                      Skill => Engineers.Operations.Get_Skill
                        (Engineers.Engineer_ID (Train)));
         -- Direction (Going normal vs going reverse)
         Display.Put (Train => Train,
                      Direction => Trains.Operations.Get_Direction (Train));
         -- Blocks occupied by the train
         Display.Put (Train => Train,
                      Blocks => Trains.Operations.Get_Blocks (Train));
      end loop;

   end Generate_Initial_Display;

   -- Represents the current state of the dispatcher
   State              : State_Type  := Start_Dispatcher;
   -- Represents the current state of setup
   Setup_State        : Setup_State_Type := Pick;
   -- Represents the number of trains the user wishes to run
   Num_Trains         : Trains.Train_ID;
   -- Represents the train we are currently choosing
   Current_Train      : Trains.Train_ID;
   -- Represents the location of the current train
   Train_Location     : User_Train_Rec;

   -- Represents the chosen path in he case of correct or incorrect information
   Correct_Verified   : Info_Correct;
   Incorrect_Verified : Info_Incorrect;

   -- Represents the response to Layout.Search.Blocks_Beneath
   Train_Blocks       : Layout.Block_List (Max_Size => 3);
   Train_Turnouts     : Layout.Search.Turnout_List (Max_Size => 5);
   Train_Success      : Boolean;

   -- Represents commands the dispatcher inputs while in display
   Command            : Commands.Command_Rec;

   -- Represents the success of "reserving" the chosen train
   Initial_Success    : Boolean;

   -- Reresents the chosen locomotive
   Train_Input        : Locomotives.Loco_Rec;

begin
   -----------------------------------------------------------------------------
   -- Runs the systems                                                        --
   -----------------------------------------------------------------------------
   -- Each iteration, goes through dispatcher states                          --
   -----------------------------------------------------------------------------
   loop
      case State is
         when Start_Dispatcher =>
            Get_Num_Trains (State      => State,
                            Num_Trains => Num_Trains);
         when Setup_Trains =>
            Console_Management.Clear_Screen;
            Setup_State   := Pick;
            Current_Train := 1;
            DoubleTalk.Speak (Phrase =>
                              DoubleTalk.Phrase_Strings.To_Bounded_String
                                ("Please select your trains."),
                              Voice  => DoubleTalk.Vader);
            --------------------------------------------------------------------
            -- Runs the setup                                                 --
            --------------------------------------------------------------------
            -- Each iteration, goes through setup substates                   --
            --------------------------------------------------------------------
            loop
               Train_Blocks.Size := 0;
               Train_Turnouts.Size := 0;

               case Setup_State is
                  -- Get the rain picks from the user
                  when Pick =>
                     Pick_Trains (Train_Num   => Current_Train,
                                  Train_Input => Train_Input);
                     Setup_State := Locate;
                  -- Get the train pick location from the user
                  when Locate =>
                     Get_Location (Train_Num   => Current_Train,
                                   Location    => Train_Location);
                     Setup_State := Search;
                  -- Search for the train location in the layout
                  when Search =>
                     Search_Tracks (Location       => Train_Location,
                                    Train_Blocks   => Train_Blocks,
                                    Train_Turnouts => Train_Turnouts,
                                    Train_Success  => Train_Success);
                     if Train_Success then
                        Verify_Train (Location     => Train_Location,
                                      Train_Blocks => Train_Blocks,
                                      Train_Num    => Current_Train,
                                      User_Pick    => Train_Input,
                                      Verified     => Correct_Verified);
                        case Correct_Verified is

                           -- Move on in the train location
                           when Y =>
                              Trains.Operations.Initialize_Train
                               (Train   => Current_Train,
                                My_Blocks  => Train_Blocks,
                                My_Turnouts => Train_Turnouts,
                                Loco    => Train_Input,
                                Successful => Initial_Success);

                              if Initial_Success then
                                 if Current_Train = Num_Trains then
                                    State := Run_Dispatcher;
                                    DoubleTalk.Speak
                                      (Phrase => DoubleTalk.Phrase_Strings.
                                         To_Bounded_String
                                         ("Redding Railroad Software is " &
                                            "ready to run."),
                                       Voice  => DoubleTalk.Vader);
                                    exit;
                                 end if;
                                 Current_Train := Current_Train + 1;
                                 Setup_State   := Pick;
                              else

                                 case Train_Conflict_Screen
                                      (Train => Current_Train) is
                                    -- Restart entering train location
                                    when Y =>
                                       Setup_State := Pick;
                                    -- Restart dispatcher from the beginning
                                    when N =>
                                       State := Start_Dispatcher;
                                       exit;
                                    -- Exit
                                    when Q =>
                                       State := Shutdown_Dispatcher;
                                       exit;
                                 end case;
                              end if;
                           -- Restart train information
                           when N =>
                              Setup_State := Pick;

                           -- Restart from the beginning
                           when R =>
                              State := Start_Dispatcher;
                              exit;

                           -- Exit
                           when Q =>
                              State := Shutdown_Dispatcher;
                              exit;
                        end case;
                     else
                        Invalid_Train (Verified => Incorrect_Verified);
                        case Incorrect_Verified is

                           -- Restart entering train location
                           when Y =>
                              Setup_State := Pick;

                           -- Restart dispatcher from the beginning
                           when N =>
                              State       := Start_Dispatcher;
                              exit;

                           -- Exit
                           when Q =>
                              State       := Shutdown_Dispatcher;
                              exit;
                        end case;
                     end if;
               end case;
            end loop;
         when Run_Dispatcher =>
            Halls.Initialize;
            Halls.Enable (Callback => Trains.Operations.Hall_Triggered'Access);
            Generate_Initial_Display (Num_Trains   => Num_Trains);
            delay 0.5;
            for Engineer in 1 .. Num_Trains loop
               Engineers.Operations.Enable (Engineers.Engineer_ID (Engineer));
               Trains.Operations.Stop_Train (Engineer);
            end loop;
            Console_Management.Disable_Echo;

            --------------------------------------------------------------------
            -- Runs the commands                                              --
            --------------------------------------------------------------------
            -- Each iteration, goes through command states                    --
            --------------------------------------------------------------------

            loop
               Commands.Get (Command => Command);
               case Command.Which is
                  -- Stop all trains (Space)
                  when Commands.Stop_All =>
                     for Train_To_Stop in 1 .. Num_Trains loop
                        Trains.Operations.Stop_Train (Train => Train_To_Stop);
                     end loop;

		  -- Invalid input
                  when Commands.Error =>
                     Display.Put_Error (Error_Message => "Invalid Input");
                  -- Restart dispatcher (R)
                  when Commands.Restart =>
                     State := Start_Dispatcher;
                     for Engineer in Engineers.Engineer_ID'Range loop
                        Engineers.Operations.Disable (Engineer);
                     end loop;
                     Trains.Operations.Restart;
                     Halls.Disable;
                     delay 0.5;
                     Display.Disable;
                     delay 0.5;
                     Console_Management.Enable_Echo;
                     exit;

                  -- Quit program (Q)
                  when Commands.Quit =>
                     Console_Management.Clear_Screen;
                     State := Shutdown_Dispatcher;
                     exit;

                  -- Stop single train (nS))
                  when Commands.Stop =>
                     Trains.Operations.Stop_Train (Train => Command.Train);

                  -- Unlock single train (nG)
                  when Commands.Go =>
                     Trains.Operations.Dispatcher_Unlock
                       (Train => Command.Train);

                  -- Move turnout to the left (nL)
                  when Commands.Left =>
                     Turnouts.Set (Requestor => 0,
                                   Turnout   => Command.Turnout,
                                   Direction => Layout.Left);

                  -- Move turnout to the right (nR)
                  when Commands.Right =>
                     Turnouts.Set (Requestor => 0,
                                   Turnout   => Command.Turnout,
                                   Direction => Layout.Right);

                  -- Free block (nF)
                  when Commands.Free =>
                     Blocks.Free (Block     => Command.Block,
                                  Requestor => 0);
                     Trains.Operations.Block_Freed (Train => 0,
                                                    Block => Command.Block);

                  -- Change engineer skill (nE)
                  when Commands.Skill =>
                     Engineers.Operations.Change_Skill
                       (Engineer => Command.Engineer);
               end case;
            end loop;
         when Shutdown_Dispatcher =>
            -- Disable trains
            for Engineer in 1 .. Num_Trains loop
               Engineers.Operations.Disable (Engineers.Engineer_ID (Engineer));
            end loop;
            -- Shutdown turnouts
            Turnouts.Shut_Down;
            -- Disable halls callbacks
            Halls.Disable;
            -- Notify of shutdown
            DoubleTalk.Speak
              (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                 ("Shutting down the Redding Railroad Software."),
               Voice  => DoubleTalk.Vader);
            -- Exit the main loop
            exit;
      end case;
   end loop;

end Dispatcher;
