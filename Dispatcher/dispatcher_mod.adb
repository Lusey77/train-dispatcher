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
--  with Turnouts;
with Engineers;
with Blocks;

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

   subtype User_Choice is Integer range Locomotives.Available_Locos'Range;

   type User_Choices_Array is array (Trains.Train_ID) of User_Choice;

   type User_Train_Rec is
      record
         Locomotive : Layout.Block_ID;
         Caboose    : Layout.Block_ID;
      end record;

   type User_Train_Location is array (Trains.Train_ID) of User_Train_Rec;



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
   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   procedure Output_Selection_Screen (Train_Num : in Trains.Train_ID) is

   begin
      Ada.Text_IO.Put_Line ("The following locomotives are available");
      Ada.Text_IO.New_Line (3);
      Ada.Text_IO.Set_Col (10);
      Ada.Text_IO.Put ("Road Name");

      Ada.Text_IO.Set_Col (31);
      Ada.Text_IO.Put ("Model");

      Ada.Text_IO.Set_Col (45);
      Ada.Text_IO.Put_Line ("Number");
      Ada.Text_IO.New_Line (2);

--      Ada.Text_IO.Skip_Line;
      for Train in Locomotives.Available_Locos'Range loop
         Ada.Text_IO.Put (' ');
         Ada.Integer_Text_IO.Put (Train, 1);
         Ada.Text_IO.Set_Col (10);
         Ada.Text_IO.Put_Line
           (Locomotives.Available_Locos (Train).Name);
      end loop;
      Ada.Text_IO.Put (' ');
      Ada.Integer_Text_IO.Put (11, 1);
      Ada.Text_IO.Set_Col (10);
      Ada.Text_IO.Put_Line ("Other");

      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line ("Enter the line number from the above" &
                            " table of the locomotive pulling Train #"
                            & Trains.Train_ID'Image (Train_Num));
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
         ----------------------------------------
         -- Center and output the welcome page --
         ----------------------------------------
         Ada.Text_IO.New_Line (20);
         Ada.Text_IO.Set_Col (40);
         Ada.Text_IO.Put_Line ("Reading Railroad Software");
         Ada.Text_IO.New_Line (2);
         Ada.Text_IO.Put_Line
           ("How many trains do you wish to run? (1, 2, or 3)");
         Ada.Text_IO.New_Line (2);
         Ada.Text_IO.Put_Line
           ("There must be at least one unoccupied block" &
              " between each pair of trains");
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
               Ada.Text_IO.New_Page;
               Ada.Text_IO.New_Line (10);
               Ada.Text_IO.Put_Line
                 ("Invalid range. Please enter a number between"
                  & " 1 and 3");
            when Ada.IO_Exceptions.Data_Error =>
               Ada.Text_IO.Skip_Line;
               Ada.Text_IO.New_Page;
               Ada.Text_IO.New_Line (10);
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
      Ada.Text_IO.New_Line (3);

      -----------------------------------------------------------------
      -- Prompt user for the location of thier locomotive            --
      -----------------------------------------------------------------
      Ada.Text_IO.Put_Line ("On which block is the locomotive" &
         " pulling train" & Trains.Train_ID'Image (Train_Num) & " located?");

      -- Validation Loop
      loop
         Train_One_Validation_Block :
         begin
            Block_IO.Get (Loco);
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
         end Train_One_Validation_Block;
      end loop;
      Ada.Text_IO.New_Line (3);
      -----------------------------------------------------------------
      -- Prompt user for the location of their caboose               --
      -----------------------------------------------------------------
      Ada.Text_IO.Put_Line ("On which block is the Caboose" &
         " pulling train" & Trains.Train_ID'Image (Train_Num) & " located?");

      -- Validation Loop
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
                          User_Input  :    out User_Choice) is
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
            exit;
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
         Ada.Text_IO.Put_Line ("What is the polarity of the train?"
            & " (N)ormal or (R)everse");
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
                           User_Picks     : in     User_Choices_Array;
                           Verified       :    out Info_Correct) is
   begin

      Ada.Text_IO.Put_Line ("Train #" & Trains.Train_ID'Image
                           (Train_Num));
      Ada.Text_IO.Put_Line ("Confirmation of Information");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line (Locomotives.Available_Locos
                           (User_Picks (Train_Num)).Name);
      Ada.Text_IO.Put_Line ("Locomotive on block " &
                            Layout.Block_ID'Image
                           (Location.Locomotive));
      Ada.Text_IO.Put_Line ("Caboose on block " &
                            Layout.Block_ID'Image
                           (Location.Caboose));
      Ada.Text_IO.Put ("Train occcupies blocks ");

      for Count in 1 .. (Train_Blocks.Size - 1) loop
         Ada.Text_IO.Put (Layout.Block_ID'Image
                         (Train_Blocks.Items
                         (Count).Block) & ",");
      end loop;

      Ada.Text_IO.Put_Line (Layout.Block_ID'Image
                           (Train_Blocks.Items
                           (Train_Blocks.Size).Block));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Is this information correct? " &
                            "Enter one of the following:");
      Ada.Text_IO.Put_Line ("   Y = yes, the information for " &
                            "this train is correct");
      Ada.Text_IO.Put_Line ("   N = no,  I wish to enter " &
                            "different information for this " &
                            "train");
      Ada.Text_IO.Put_Line ("   R = no,  I wish to restart " &
                            "setting up from the beginning");
      Ada.Text_IO.Put_Line ("   Q = no,  I wish to terminate " &
                            "this operating session");
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
      Ada.Text_IO.Put_Line ("The maximum number of blocks"
                           & " of blocks beneath a train"
                           & " is 3");
      Ada.Text_IO.Put_Line ("There are more than 3 blocks"
                           & " beneath your train");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Would you like to reenter the"
                           & " information?");
      Ada.Text_IO.New_Line (2);
      Ada.Text_IO.Put_Line ("   Y = yes, I wish to reenter"
                            & " this train's location.");
      Ada.Text_IO.Put_Line ("   N = no,  I wish to restart"
                           & " set up from the beginning");
      Ada.Text_IO.Put_Line ("   Q = no,  I wish to"
                            & " terminate"
                            & " this operating session.");

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

   procedure Train_Conflict_Screen (Verified : out Info_Incorrect) is
   begin
      Ada.Text_IO.Put_Line ("The location information for Train 1 is not " &
                              "valid because it conflicts with a train you " &
                              "entered earlier");
      Ada.Text_IO.New_Line (3);
      Ada.Text_IO.Put_Line ("Is this information correct? " &
                            "Enter one of the following:");
      Ada.Text_IO.Put_Line ("   Y = yes, the information for " &
                            "this train is correct");
      Ada.Text_IO.Put_Line ("   N = no,  I wish to enter " &
                            "different information for this " &
                            "train");
      Ada.Text_IO.Put_Line ("   R = no,  I wish to restart " &
                            "setting up from the beginning");
      Ada.Text_IO.Put_Line ("   Q = no,  I wish to terminate " &
                              "this operating session");
      loop
         User_Option_Validation_Block_3 :
         begin
            Incorrect_IO.Get (Verified);
            exit;
         exception
            when others =>
               Ada.Text_IO.New_Page;
               Ada.Text_IO.New_Line (10);
               Ada.Text_IO.Put_Line
                 ("Invalid data. Please enter Y, N, or Q");
         end User_Option_Validation_Block_3;
      end loop;
   end Train_Conflict_Screen;

   procedure Generate_Initial_Display (Num_Trains   : in Trains.Train_ID;
                                       Choice_Array : in User_Choices_Array) is
      Turnout_Direction : Layout.Turn_Choice;
      Turnout_Moving    : Boolean;
   begin

      delay 1.0;
      Display.Enable;
      delay 1.0;

      for Turnout in Layout.Turnout_ID loop
         case Turnouts.Status (Turnout).Current is
            when Turnouts.Fully_Left =>
               Turnout_Direction := Layout.Left;
               Turnout_Moving    := False;
            when Turnouts.Fully_Right =>
               Turnout_Direction := Layout.Right;
               Turnout_Moving    := False;
            when Turnouts.Moving_Left =>
               Turnout_Direction := Layout.Left;
               Turnout_Moving    := True;
            when Turnouts.Moving_Right =>
               Turnout_Direction := Layout.Right;
               Turnout_Moving    := True;
         end case;

         Display.Put (Turnout   => Turnout,
                      Direction => Turnout_Direction,
                      Moving    => Turnout_Moving);
      end loop;

      for Train in 1 .. Num_Trains loop
         Display.Put (Train => Train,
                      Name => Locomotives.Available_Locos
                        (Choice_Array (Train)).Name);
         Display.Put (Train => Train,
                      Skill => Engineers.Get_Skill
                        (Engineers.Engineer_ID (Train)));
         Display.Put (Train => Train,
                      Direction => Trains.Operations.Get_Direction (Train));
         Display.Put (Train => Train,
                      Throttle => 45);
         Display.Put (Train => Train,
                      Blocks => Trains.Operations.Get_Blocks (Train));
      end loop;
      Turnouts.Set_Change_Callback (To => Display.Put'Access);
   end Generate_Initial_Display;

   -- Represents the current state of the dispatcher
   State              : State_Type  := Start_Dispatcher;
   -- Represents the current state of setup
   Setup_State        : Setup_State_Type := Pick;
   -- Represents the number of trains the user wishes to run
   Num_Trains         : Trains.Train_ID;

   Current_Train      : Trains.Train_ID;

   Trains_Picked      : User_Choices_Array;

   Trains_Location    : User_Train_Location;

   Correct_Verified   : Info_Correct;
   Incorrect_Verified : Info_Incorrect;

   Train_Blocks       : Layout.Block_List (Max_Size => 3);
   Train_Turnouts     : Layout.Search.Turnout_List (Max_Size => 5);
   Train_Success      : Boolean;

   Command            : Commands.Command_Rec;

   Success            : Boolean;

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
               if Current_Train > Num_Trains then
                  State := Run_Dispatcher;
                  DoubleTalk.Speak
                    (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                       ("Redding Railroad Software is ready to run."),
                     Voice  => DoubleTalk.Vader);
                  exit;
               end if;
               Train_Blocks.Size := 0;
               Train_Turnouts.Size := 0;

               case Setup_State is
                  -- Get the rain picks from the user
                  when Pick =>
                     Pick_Trains (Train_Num   => Current_Train,
                                  User_Input  => Trains_Picked (Current_Train));
                     Setup_State := Locate;
                  -- Get the train pick location from the user
                  when Locate =>
                     Get_Location (Train_Num   => Current_Train,
                                   Location    => Trains_Location
                                                  (Current_Train));
                     Setup_State := Search;
                  -- Search for the train location in the layout
                  when Search =>
                     Search_Tracks (Location       => Trains_Location
                                    		     (Current_Train),
                                    Train_Blocks   => Train_Blocks,
                                    Train_Turnouts => Train_Turnouts,
                                    Train_Success  => Train_Success);
                     if Train_Success then
                        Verify_Train (Location     => Trains_Location
                                      		      (Current_Train),
                                      Train_Blocks => Train_Blocks,
                                      Train_Num    => Current_Train,
                                      User_Picks   => Trains_Picked,
                                      Verified     => Correct_Verified);
                        case Correct_Verified is

                           -- Move on in the train location
                           when Y =>
                              Ada.Text_IO.Put_Line ("1");
                              Trains.Operations.Initialize_Train
                                (Train   => Current_Train,
                                 Blocks  => Train_Blocks,
                                 Loco    => Locomotives.Available_Locos
                                   (Trains_Picked (Current_Train)),
                                 Success => Success);
                              Ada.Text_IO.Put_Line ("2");
                              if Train_Success then
                                 Current_Train := Current_Train + 1;
                              else
                                 Train_Conflict_Screen
                                   (Verified => Incorrect_Verified);
                              end if;
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
                              Setup_State   := Pick;
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
            Generate_Initial_Display (Num_Trains   => Num_Trains,
                                      Choice_Array => Trains_Picked);
            loop
               Commands.Get (Command => Command);
               case Command.Which is
                  when Commands.Stop_All | Commands.Error =>
                     null;
                  when Commands.Restart =>
                     State := Start_Dispatcher;
                     exit;
                  when Commands.Quit =>
                     State := Shutdown_Dispatcher;
                     exit;
                  when Commands.Stop | Commands.Go =>
                     null;
                  when Commands.Left =>
                     Turnouts.Set (Requestor => 0,
                                   Turnout   => Command.Turnout,
                                   Direction => Layout.Left);
                  when Commands.Right =>
                     Turnouts.Set (Requestor => 0,
                                   Turnout   => Command.Turnout,
                                   Direction => Layout.Right);
                  when Commands.Free =>
                     Blocks.Free (Block     => Command.Block,
                                  Requestor => 0);
                  when Commands.Skill =>
                     Engineers.Change_Skill (Engineer => Command.Engineer);
               end case;
            end loop;
         when Shutdown_Dispatcher =>
            DoubleTalk.Speak
              (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                 ("Shutting down the Redding Railroad Software."),
               Voice  => DoubleTalk.Vader);
            exit;
      end case;
   end loop;

end Dispatcher;
