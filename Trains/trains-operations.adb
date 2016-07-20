--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Ethan Morisette & Kaleb Luse -----------------------------------
-- Tested By  : Travis Sullivan ------------------------------------------------
-- Updated    : 27 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Turnouts;
with Sound;
with DoubleTalk;
with Blocks;
with Layout;
with Display;
with Common_Units;
with Halls;
with Dallee;
with Cabs;
use type Layout.Block_ID;
use type Layout.Terminator_Type;
use type Layout.Turnout_ID;
use type Layout.Hall_ID;
use type Layout.Turn_Choice;
use type Turnouts.Turnout_State_Type;
use type Common_Units.Percent;
package body Trains.Operations is

   -- the protected state of the ability of a train
   protected type Ability_Protected is
      -- sets the train's state to enable
      procedure Enable;
      -- sets the train's state to disable
      procedure Disable;
      -- returns the ability of a train
      function Get_Ability return Ability_Type;
   private
      Ability_State : Ability_Type := Disabled;
   end Ability_Protected;

   protected body Ability_Protected is
      procedure Enable is
      begin
         Ability_State := Enabled;
      end Enable;
      procedure Disable is
      begin
         Ability_State := Disabled;
      end Disable;
      function Get_Ability return Ability_Type is
      begin
         return Ability_State;
      end Get_Ability;
   end Ability_Protected;

   -- the protected state of the status of a train
   protected type Status_Protected is
      -- sets the status state of train to locked
      procedure Lock;
      -- sets the status state of train to unlocked
      procedure Unlock;
      -- returns the status of the train
      function Get_Status return Status_Type;
   private
      Status_State : Status_Type;
   end Status_Protected;

   protected body Status_Protected is
      procedure Lock is
      begin
         Status_State := Locked;
      end Lock;
      procedure Unlock is
      begin
         Status_State := Unlocked;
      end Unlock;
      function Get_Status return Status_Type is
      begin
         return Status_State;
      end Get_Status;
   end Status_Protected;

   -- the protected state of the direction of a train
   protected type Direction_Protected is
      -- sets the direction state of train to forward
      procedure Set_Forward;
      -- sets the direction state of train to backward
      procedure Set_Backward;
      -- returns the direction of the train
      function Get_Direction return Direction_Type;
   private
      Direction_State : Direction_Type := Forward;
   end Direction_Protected;

   protected body Direction_Protected is
      procedure Set_Forward is
      begin
         Direction_State := Forward;
      end Set_Forward;
      procedure Set_Backward is
      begin
         Direction_State := Backward;
      end Set_Backward;
      function Get_Direction return Direction_Type is
      begin
         return Direction_State;
      end Get_Direction;
   end Direction_Protected;

   -- the protected locked states of the train
   protected type Locks_Protected is
      -- tells train that it has been locked by indicated reason
      procedure Locked_By (Reason : in Stop_Reason);
      -- tells train that is has been unlocked by indicated reason
      procedure Unlocked_By (Reason : in Stop_Reason);
      -- tells train that it has been locked by indicated turnout
      procedure Turnout_Locked (Turnout : in Layout.Turnout_ID);
      -- returns if train is locked for the indicated reason
      function Get_Reason_Status (Reason : Stop_Reason) return Boolean;
      -- tells train that it has been unlocked by indicated turnout
      procedure Turnout_Unlocked (Turnout : in Layout.Turnout_ID);
      -- returns if train is locked by an indicated turnout
      function Get_Turnout_Status (Turnout : in Layout.Turnout_ID)
                                   return Boolean;
      -- tells train that it has been locked by indicated block
      procedure Block_Locked (Block : in Layout.Block_ID);
      -- returns what block is locking train
      function Get_Locked_Block return Layout.Block_ID;
      -- returns all lock info for the specific train
      function Get_Lock_Rec return Stop_Rec;
   private
      Lock_Info : Stop_Rec;
   end Locks_Protected;

   protected body Locks_Protected is
      procedure Locked_By (Reason : in Stop_Reason) is
      begin
         Lock_Info.Reasons (Reason) := True;
      end Locked_By;
      procedure Unlocked_By (Reason : in Stop_Reason) is
      begin
         Lock_Info.Reasons (Reason) := False;
      end Unlocked_By;
      function Get_Reason_Status (Reason : Stop_Reason) return Boolean is
      begin
         return Lock_Info.Reasons (Reason);
      end Get_Reason_Status;
      procedure Turnout_Locked (Turnout : in Layout.Turnout_ID) is
      begin
         Lock_Info.Turnouts (Turnout) := True;
      end Turnout_Locked;
      procedure Turnout_Unlocked (Turnout : in Layout.Turnout_ID) is
      begin
         Lock_Info.Turnouts (Turnout) := False;
      end Turnout_Unlocked;
      function Get_Turnout_Status (Turnout : in Layout.Turnout_ID)
                                   return Boolean is
      begin
         return Lock_Info.Turnouts (Turnout);
      end Get_Turnout_Status;
      procedure Block_Locked (Block : Layout.Block_ID) is
      begin
         Lock_Info.Block := Block;
      end Block_Locked;
      function Get_Locked_Block return Layout.Block_ID is
      begin
         return Lock_Info.Block;
      end Get_Locked_Block;
      function Get_Lock_Rec return Stop_Rec is
      begin
         return Lock_Info;
      end Get_Lock_Rec;
   end Locks_Protected;

   -- Represents the information of a train
   type Train_Rec is
      record
         Loco_Name        : Locomotives.Loco_String;
         Min_Throttle     : Locomotives.Percent;
         Cab              : Cabs.Control_Cab_ID;
         Ability          : Ability_Protected;
         Status           : Status_Protected;
         Direction        : Direction_Protected;
         Locked           : Locks_Protected;
      end record;

   -- Represents an array of train recs
   type Trains_Array is array (Train_ID) of Train_Rec;

   -- Represents the trains and all their information
   Train_States : Trains_Array;

--------------------------------------------------------------------------------

   -- Helper function to change the force turnout
   procedure Change_Turnout (Train     : in Train_ID;
                             Block     : in Layout.Block_ID;
                             Direction : in Layout.Block_Polarity) is

      -- Represents the terminator at the end of the given block
      Terminator : Layout.Terminator_Type;
      -- Represents the turnout at the end of the given block
      Turnout    : Layout.Turnout_ID;


   begin

      -- Find the terminator at the end of the block
      Terminator := Layout.Next_Term (Block     => Block,
                                      Direction => Direction);

      if Terminator = Layout.Turnout then
         -- If the terminator is a turnout then see if its a force turnout
         Turnout := Layout.End_Block (Block     => Block,
                                      Direction => Direction);

         if Layout.Is_Force_Turnout (Block     => Block,
                                     Direction => Direction) then
            -- If it is a force turnout set it accordingly
            Turnouts.Set (Requestor => Train,
                          Turnout   => Turnout,
                          Direction => Layout.Force_Turnout
                            (Block     => Block,
                             Direction => Direction).Direction);
         elsif Layout.Is_Joint_Turnout (Turnout   => Turnout,
                                        Direction => Turnouts.Status
                                          (Turnout).Desired) then
            Turnouts.Set (Requestor => Train,
                          Turnout   => Layout.Joint_Turnout (Turnout),
                          Direction => Turnouts.Status (Turnout).Desired);
         end if;
      end if;
   end Change_Turnout;

   -- Helper function to get the next block
   procedure Get_Next_Block (Block     : in Layout.Block_ID;
                             Direction : in Layout.Block_Polarity;
                             Return_Block : out Layout.Block_ID;
                             Return_Dir   : out Layout.Block_Polarity) is

      -- Represents the terminator at the end of the given block
      Terminator    : Layout.Terminator_Type;
      -- Represents the turnout at the end of a given block
      Turnout       : Layout.Turnout_ID;
      -- Represents the status of the turnout
      Status        : Turnouts.Status_Rec;
      -- Represents the limb of a turnout
      Turn_Dir      : Layout.Turnout_Limb;


   begin
      -- Get the terminator at the end of the block
      Terminator := Layout.Next_Term (Block     => Block,
                                      Direction => Direction);
      if Terminator = Layout.Block then
         -- If the terminator is a block then return the block
         Return_Block := Layout.End_Block (Block     => Block,
                                           Direction => Direction);
         if Layout.Is_Reverse (Layout.Sensor_Number (Block_1 => Block,
                                                     Block_2 => Return_Block))
         then
            Return_Dir := Layout.Opposite (Direction);
         else
            Return_Dir := Direction;
         end if;
      elsif Terminator = Layout.Turnout then
         -- If the terminator is a turnout return the block at the end of the
         -- turnout
         Turnout := Layout.End_Block (Block     => Block,
                                      Direction => Direction);
         if Layout.Is_Force_Turnout (Block     => Block,
                                     Direction => Direction) then
            Return_Block := Layout.End_Turnout (Turnout   => Turnout,
                                                Direction => Layout.Common);
         else
            Status := Turnouts.Status (Turnout => Turnout);
            if Status.Current = Turnouts.Fully_Left or
              Status.Current = Turnouts.Moving_Left then
               Turn_Dir := Layout.Left;
            else
               Turn_Dir := Layout.Right;
            end if;
            Return_Block := Layout.End_Turnout (Turnout   => Turnout,
                                                Direction => Turn_Dir);
         end if;
         if Layout.Is_Reverse (Layout.Sensor_Number (Block_1 => Block,
                                                     Block_2 => Return_Block))
         then
            -- If the sensor is reversing return the opposite direction of the
            -- current block
            Return_Dir := Layout.Opposite (Direction);
         else
            -- else return the direction of the current block
            Return_Dir := Direction;
         end if;
      end if;
   end Get_Next_Block;

   -- Represents an array of block lists
   type Block_Array is array (Train_ID) of Layout.Block_List (6);

   -- Represents the protected block arrays for all the trains
   protected Occupied_Blocks is
      -- Clears the blocks for the given train
      procedure Clear (Train : in Train_ID);
      -- Removes a block from the block array
      procedure Remove (Train : in Train_ID);
      -- Adds a block to the block array
      procedure Add (Train : in Train_ID;
                     Block : in Layout.Block_Rec);
      -- Returns the size of the block array
      function Size (Train : in Train_ID) return Positive;
      -- Gets a block from the block array
      function Get_Block (Train : in Train_ID;
                          Block : in Positive) return Layout.Block_ID;
      -- Gets the direction from the block array
      function Get_Direction (Train : in Train_ID;
                              Block : in Positive) return Layout.Block_Polarity;
      -- Change the direction of the train
      procedure Change_Direction (Train : in Train_ID);
      -- Determine which train triggered a sensor
      function Which_Train (Block_1 : in Layout.Block_ID;
                            Block_2 : in Layout.Block_ID) return Train_ID;
   private
      -- Holds the blocks that are currently occupied by the train
      My_Blocks  : Block_Array;
   end Occupied_Blocks;

   protected body Occupied_Blocks is

      procedure Clear (Train : in Train_ID) is
      begin
         -- Remove all the block from the block array
         -- Each iteration, remove one block from the array
         for Index in 1 .. My_Blocks (Train).Size loop
            Occupied_Blocks.Remove (Train => Train);
         end loop;
      end Clear;

      procedure Remove (Train : in Train_ID) is
      begin
         -- Set the power to null
         Blocks.Power_Block (Block     => My_Blocks (Train).Items
                             (My_Blocks (Train).Size).Block,
                             Direction => Layout.Normal,
                             Cab       => 0);
         -- Free the block
         Blocks.Free (Block     => My_Blocks (Train).Items
                      (My_Blocks (Train).Size).Block,
                      Requestor => Train);
         -- Call block freed
         Block_Freed (Train => Train,
                      Block => My_Blocks (Train).Items
                      (My_Blocks (Train).Size).Block);
         -- Decrement the size of the block array
         My_Blocks (Train).Size := My_Blocks (Train).Size - 1;
         if Train_States (Train).Locked.Get_Reason_Status
                                       (Reason => Lost_Caboose) then
            -- If the train is stopped because of the lost caboose then see
            -- if block size is less than 5 and is so call retrieved stock
            if My_Blocks (Train).Size <= 5 then
               -- If the block size is now less then 5 release the train
               Retrieved_Stock (Train);
            end if;
         end if;
         -- Display the updated state
         Display.Put (Train  => Train,
                      Blocks => My_Blocks (Train));
      end Remove;

      procedure Add (Train : in Train_ID;
                     Block : in Layout.Block_Rec) is
      begin
         if My_Blocks (Train).Size + 1 > 5 then
            -- If the block size > 5 then signal for lost rolling stock
            Lost_Rolling_Stock (Train);
         end if;

         -- Shift the blocks in the array over one
         My_Blocks (Train).Items (2 .. My_Blocks (Train).Size + 1) :=
           My_Blocks (Train).Items (1 .. My_Blocks (Train).Size);
         -- Add the new block to the front of the array
         My_Blocks (Train).Items (1) := Block;
         -- Power the new block
         Blocks.Power_Block (Block     => Block.Block,
                             Direction => Block.Direction,
                             Cab       => Cabs.Cab_ID (Train));
         -- Add to the size of the list
         My_Blocks (Train).Size := My_Blocks (Train).Size + 1;
         -- Display the new block list to the screen
         Display.Put (Train  => Train,
                      Blocks => My_Blocks (Train));
      end Add;

      function Size (Train : in Train_ID) return Positive is
      begin
         -- Return the size of the train
         return My_Blocks (Train).Size;
      end Size;

      function Get_Block (Train : in Train_ID;
                          Block : in Positive) return Layout.Block_ID is
      begin
         -- Return the block at the given index
         return My_Blocks (Train).Items (Block).Block;
      end Get_Block;

      function Get_Direction (Train : in Train_ID;
                              Block : in Positive)
                              return Layout.Block_Polarity is
      begin
         -- Return the direction at the given index
         return My_Blocks (Train).Items (Block).Direction;
      end Get_Direction;

      procedure Change_Direction (Train : in Train_ID) is

         -- Represents the reversed list
         Reversed      : Layout.Block_List (My_Blocks (Train).Size);
         -- Represents the index of the reversed block list
         Index_2       : Positive := 1;
         -- Represents the block to be freed/reserved
         Block         : Layout.Block_ID;
         -- Represents th direction of the block thats to be freed/reserved
         Direction     : Layout.Block_Polarity;
         -- Represents the success of the reservation
         Successful    : Boolean;

      begin
         -- Get the block in front of the locomotive
         Get_Next_Block (Block        => My_Blocks (Train).Items
                            (1).Block,
                         Direction    => My_Blocks (Train).Items
                            (1).Direction,
                         Return_Block => Block,
                         Return_Dir   => Direction);
         -- Free the block in front of the locomotive
         Blocks.Free (Block     => Block,
                      Requestor => Train);
         -- Signal all the trains that a block has been freed
         Block_Freed (Train => Train,
                      Block => Block);
         -- Get the block behind the caboose
         Get_Next_Block (Block        => My_Blocks (Train).Items
                         (My_Blocks (Train).Size).Block,
                         Direction    => Layout.Opposite
                          (My_Blocks (Train).Items
                         (My_Blocks (Train).Size).Direction),
                         Return_Block => Block,
                         Return_Dir   => Direction);
         -- Reserve the block behind the caboose
         Blocks.Reserve (Block     => Block,
                         Requestor => Train,
                         Success   => Successful);
         -- Reverse the blocks and direction in the block array
         -- Each iteration, reverse the one block and direction
         for Index_1 in reverse 1 .. My_Blocks (Train).Size loop
            Reversed.Items (Index_1).Block :=
              My_Blocks (Train).Items (Index_2).Block;
            Reversed.Items (Index_1).Direction :=
              Layout.Opposite (My_Blocks (Train).Items (Index_2).Direction);
            Index_2 := Index_2 + 1;
         end loop;
         -- Power the new array
         -- Each iteration, power one block in the array
         for Index in 1 .. My_Blocks (Train).Size loop
            My_Blocks (Train).Items (Index) := Reversed.Items (Index);
            Blocks.Power_Block
              (Block     => My_Blocks (Train).Items (Index).Block,
               Direction => My_Blocks (Train).Items (Index).Direction,
               Cab       => Cabs.Cab_ID (Train));
         end loop;
         -- Change the force turnout at the end if one exists
         Change_Turnout (Train     => Train,
                         Block     => My_Blocks (Train).Items (1).Block,
                         Direction => My_Blocks (Train).Items (1).Direction);
         -- If the reservation was unsuccessful signal block reserved
         if not Successful then
            Block_Reserved (Train => Train,
                            Block => Block);
         end if;
      end Change_Direction;

      function Which_Train (Block_1 : in Layout.Block_ID;
                            Block_2 : in Layout.Block_ID) return Train_ID is
      begin
         -- Search all the trains and determine which train is on the
         -- given blocks
         for Train in Train_ID'Range loop
            for Index in 1 .. My_Blocks (Train).Size loop
               if Block_1 = My_Blocks (Train).Items (Index).Block or
                 Block_2 = My_Blocks (Train).Items (Index).Block then
                  return Train;
               end if;
            end loop;
         end loop;
         -- If none of the blocks correspond to a train return 1(program will
         -- respond to the issue later)
         return 1;
      end Which_Train;
   end Occupied_Blocks;


   function Get_Blocks (Train  : in  Train_ID) return Layout.Block_List is

      -- Represents the block list to be returned
      Blocks : Layout.Block_List (6);

   begin
      -- Get the blocks and direction from the block array
      -- Each iteration, get one block and its direction
      for Block in 1 .. Occupied_Blocks.Size (Train) loop
         Blocks.Items (Block).Block := Occupied_Blocks.Get_Block (Train,
                                                                  Block);
         Blocks.Items (Block).Direction := Occupied_Blocks.Get_Direction (Train,
                                                                    Block);
      end loop;
      -- Assign the correct blocks size
      Blocks.Size := Occupied_Blocks.Size (Train);
      -- Return the block list
      return Blocks;
   end Get_Blocks;

   -- initialize all locked substates to false (not locked)
   procedure Initialize_Locks (Train : in Train_ID) is
   begin
      for Reason in Stop_Reason loop
         -- initialize all lock reason to not locked
         Train_States (Train).Locked.Unlocked_By (Reason => Reason);
         -- initialize all turnouts to not locking train
         for Turnout in Layout.Turnout_ID loop
            Train_States (Train).Locked.Turnout_Unlocked (Turnout => Turnout);
         end loop;
      end loop;
      Cabs.Set_Limit (Cab   => Train_States (Train).Cab,
                      Value => 0);
   end Initialize_Locks;

   -- checks to see if all locked substates are unlocked and changes to unlocked
   procedure Check_Locked_States (Train : in Train_ID) is
      Ready     : Boolean := True;
   begin
      -- check to see if there is any reason the train is stopped
      for Reason in Stop_Reason loop
         if Train_States (Train).Locked.Get_Reason_Status (Reason) then
            Ready := False;
         end if;
      end loop;
      -- if the train is ready to be unlocked, unlock it
      if Ready then
         Train_States (Train).Status.Unlock;

         Cabs.Set_Limit (Cab   => Train_States (Train).Cab,
                      Value => 100);
      end if;
   end Check_Locked_States;

   -- procedure for initializing the train
   procedure Initialize_Train (Train       : in  Train_ID;
                               My_Blocks   : in  Layout.Block_List;
                               My_Turnouts : in  Layout.Search.Turnout_List;
                               Loco        : in  Locomotives.Loco_Rec;
                               Successful  : out Boolean) is

      -- Represents a block on the track
      Block         : Layout.Block_ID;
      -- Represents the opposite direction of the caboose
      Direction     : Layout.Block_Polarity;
      -- Represents the block underneatht the locomotive
      Loco_Block    : Layout.Block_ID;
      -- Represents the direction of the locomotives block
      Loco_Dir      : Layout.Block_Polarity;
      -- Represents the turnout at the end of a block
      Turnout       : Layout.Turnout_ID;
      -- Represents the terminator at the end of a block
      Terminator    : Layout.Terminator_Type;

   begin
      Successful := True;
      Occupied_Blocks.Clear (Train);
      for Block in 1 .. My_Blocks.Size loop
         Occupied_Blocks.Add (Train => Train,
                              Block => My_Blocks.Items (Block));
         Blocks.Reserve (Block     => My_Blocks.Items (Block).Block,
                         Requestor => Train,
                         Success   => Successful);
         if not Successful then
            for Block in 1 .. My_Blocks.Size loop
               Blocks.Free (Block     => My_Blocks.Items (Block).Block,
                            Requestor => Train);
            end loop;
            Occupied_Blocks.Clear (Train => Train);
            exit;
         end if;
      end loop;
      Loco_Block := Occupied_Blocks.Get_Block (Train => Train,
                                               Block => 1);
      Loco_Dir   := Occupied_Blocks.Get_Direction (Train => Train,
                                                   Block => 1);
      Terminator := Layout.Next_Term (Block     => Loco_Block,
                                      Direction => Loco_Dir);
      if Terminator = Layout.Turnout then
         Turnout := Layout.End_Block (Block     => Loco_Block,
                                      Direction => Loco_Dir);
         if Layout.Is_Force_Turnout (Block     => Loco_Block,
                                     Direction => Loco_Dir) then
               Turnouts.Set (Requestor => Train,
                             Turnout   => Turnout,
                             Direction => Layout.Force_Turnout
                               (Block     => Loco_Block,
                                Direction => Loco_Dir).Direction);
         end if;
      end if;
      if Successful then
         Get_Next_Block (Block        => Loco_Block,
                         Direction    => Loco_Dir,
                         Return_Block => Block,
                         Return_Dir   => Direction);
         Blocks.Reserve (Block     => Block,
                         Requestor => Train,
                         Success   => Successful);
      end if;
      if Train = 2 then
         if Train_States (Train).Loco_Name = Train_States (1).Loco_Name then
            Successful := False;
         end if;
      end if;
      if Train = 3 then
         if Train_States (Train).Loco_Name = Train_States (1).Loco_Name then
            Successful := False;
         end if;
         if Train_States (Train).Loco_Name = Train_States (2).Loco_Name then
            Successful := False;
         end if;
      end if;
      if Successful then
         for Index in 1 .. My_Turnouts.Size loop
            Turnouts.Set (Requestor => Train,
                          Turnout   => My_Turnouts.Items (Index).Turnout,
                          Direction => My_Turnouts.Items (Index).Direction);
         end loop;
         Train_States (Train).Loco_Name             := Loco.Name;
         Train_States (Train).Min_Throttle          := Loco.Minimum_Throttle;
         Train_States (Train).Cab                   := Cabs.Cab_ID (Train);
         Train_States (Train).Direction.Set_Forward;
         Release_Train (Train);
      end if;
   end Initialize_Train;

   -- procedure for releasing a train
   procedure Release_Train (Train : in Train_ID) is
   begin
      -- if the train is disabled
      if Train_States (Train).Ability.Get_Ability = Disabled then
         -- change the ability state to enabled
         Train_States (Train).Ability.Enable;
         -- initialize the status state to unlocked
         Train_States (Train).Status.Unlock;
      end if;
   end Release_Train;

   -- procedure for holding the train
   procedure Hold_Train (Train : in Train_ID) is
   begin
      -- change the ability state to disabled
      Train_States (Train).Ability.Disable;
   end Hold_Train;

   -- procedure for stopping a train
   procedure Stop_Train (Train : in Train_ID) is
   begin
      -- if train is enabled
      if Train_States (Train).Ability.Get_Ability = Enabled then
         -- if the train is entering a new locked state, initialize substates
         if Train_States (Train).Status.Get_Status = Unlocked then
            Initialize_Locks (Train);
         end if;
         -- change the status state of train to locked
         Train_States (Train).Status.Lock;
         -- change locked substates to locked by dispatcher (rest are unlocked)
         Train_States (Train).Locked.Locked_By (Dispatcher_Request);
         -- output the proper mesage to the display
         Display.Put (Train  => Train,
                      Status => Train_States (Train).Locked.Get_Lock_Rec);
         -- give the user a verbal message of the stop
         DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                          ("Train " & Train_ID'Image (Train) & " has been" &
                           " stopped for an emergency stop!"),
                           Voice => DoubleTalk.Vader);

      end if;
   end Stop_Train;

   -- procedure for the dispatcher to unlock the train
   procedure Dispatcher_Unlock (Train : in Train_ID) is
   begin
      -- if train is enabled and locked by dispatcher
      if Train_States (Train).Ability.Get_Ability = Enabled and
         Train_States (Train).Locked.Get_Reason_Status
                                    (Dispatcher_Request) then
         -- change locked by dispatcher substate to not locked by dispatcher
         Train_States (Train).Locked.Unlocked_By (Dispatcher_Request);
         -- check to see if the train is ready to be unlocked
         Check_Locked_States (Train);
         -- if the train is locked by the system then the stock is retrieved
         if Train_States (Train).Locked.Get_Reason_Status (Lost_Caboose) then
            Retrieved_Stock (Train);
         end if;
         -- output the proper mesage to the display
         Display.Put (Train  => Train,
                      Status => Train_States (Train).Locked.Get_Lock_Rec);
         -- supply the user with a verbal message of the unlock
         DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                          ("Train " & Train_ID'Image (Train) & " has been " &
                           "unlocked by the dispatcher."),
                           Voice  => DoubleTalk.Vader);
      end if;
   end Dispatcher_Unlock;

   -- procedure for locking the train due to lost rolling stock
   procedure Lost_Rolling_Stock (Train : in Train_ID) is
   begin
      -- if train is enabled
      if Train_States (Train).Ability.Get_Ability = Enabled then
         -- if the train is entering a new locked state, initialize substates
         if Train_States (Train).Status.Get_Status = Unlocked then
            Initialize_Locks (Train);
         end if;
         -- change the status state of train to locked
         Train_States (Train).Status.Lock;
         -- change locked substate to locked by dispatcher (rest are unlocked)
         Train_States (Train).Locked.Locked_By (Dispatcher_Request);
         -- change locked substate to locked by system (rest are unlocked)
         Train_States (Train).Locked.Locked_By (Lost_Caboose);
         -- output the proper mesage to the display
         Display.Put (Train  => Train,
                      Status => Train_States (Train).Locked.Get_Lock_Rec);
         -- supply user with a verbal message of lost stock
         DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                          ("Train " & Train_ID'Image (Train) &
                           " has been stopped. It has lost rolling " &
                           "stock."),
                           Voice => DoubleTalk.Vader);
      end if;
   end Lost_Rolling_Stock;

   -- procedure for unlocking due to fixed lost rolling stock
   procedure Retrieved_Stock (Train : in Train_ID) is
   begin
      -- if train is enabled and locked by system
      if Train_States (Train).Ability.Get_Ability = Enabled and
         Train_States (Train).Locked.Get_Reason_Status (Lost_Caboose) then
         -- change locked by system substate to not locked by system
         Train_States (Train).Locked.Unlocked_By (Lost_Caboose);
         -- check to see if the train is ready to be unlocked
         Check_Locked_States (Train);
         -- output the proper mesage to the display
         Display.Put (Train  => Train,
                      Status => Train_States (Train).Locked.Get_Lock_Rec);
      end if;
   end Retrieved_Stock;

   -- procedure to lock the train due to a failed turnout
   procedure Turnout_Failed (Train   : in Request_ID;
                             Turnout : in Layout.Turnout_ID) is
   begin
      -- if the turnout fails when the dispatcher requests a turnout stop trains
      if Train = 0 then
         for My_Train in Train_ID'Range loop
            if Train_States (My_Train).Ability.Get_Ability = Enabled then
               DoubleTalk.Speak (Phrase =>
                                 DoubleTalk.Phrase_Strings.To_Bounded_String
                                 ("Turnout Failed. Stopping all trains."),
                                 Voice  => DoubleTalk.Vader);
               Stop_Train (Train => My_Train);
            end if;
         end loop;
      else
         -- if train is enabled
         if Train_States (Train).Ability.Get_Ability = Enabled then
            -- if the train is entering a new locked state, initialize substates
            if Train_States (Train).Status.Get_Status = Unlocked then
               Initialize_Locks (Train);
            end if;
            -- change the status state of train to locked
            Train_States (Train).Status.Lock;
            -- change the locked substate to locked by turnout (rest are unlock)
            Train_States (Train).Locked.Locked_By (Turnout_Failure);
            Train_States (Train).Locked.Turnout_Locked (Turnout);
            -- output the proper mesage to the display
            Display.Put (Train  => Train,
                         Status => Train_States (Train).Locked.Get_Lock_Rec);
            -- supplies user with a verbal message about failed turnout
            DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.
                              To_Bounded_String ("Train " &
                              Train_ID'Image (Train) &
                              " has been stopped by turnout " &
                              Layout.Turnout_ID'Image (Turnout)),
                              Voice => DoubleTalk.Vader);
         end if;
      end if;
   end Turnout_Failed;

   -- procedure for unlocking the train for a fixed turnout
   procedure Turnout_Fixed (Train   : in Request_ID;
                            Turnout : in Layout.Turnout_ID) is
   begin
      -- If requestor is dispatcher fix all the trains
      if Train = 0 then
         DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                           ("Turnout Fixed"),
                           Voice  => DoubleTalk.Vader);
      else
         -- if train is enabled and locked by turnout
         if Train_States (Train).Ability.Get_Ability = Enabled and
            Train_States (Train).Locked.Get_Reason_Status (Turnout_Failure) then
            -- change locked by turnout substate to not locked by turnout
            Train_States (Train).Locked.Unlocked_By (Turnout_Failure);
            Train_States (Train).Locked.Turnout_Unlocked (Turnout);
            -- if there are any remaining locked turnouts, train is locked
            for Turnout in Layout.Turnout_ID loop
               if Train_States (Train).Locked.Get_Turnout_Status (Turnout) then
                  Train_States (Train).Locked.Locked_By (Turnout_Failure);
               end if;
            end loop;
            -- check to see if the train is ready to be unlocked
            Check_Locked_States (Train);
            -- output the proper mesage to the display
            Display.Put (Train  => Train,
                         Status => Train_States (Train).Locked.Get_Lock_Rec);
         end if;
      end if;
   end Turnout_Fixed;

   -- procedure for locking a train due to a failed block reservation
   procedure Block_Reserved (Train : in Train_ID;
                             Block : in Layout.Block_ID) is
   begin
      -- if train is enabled
      if Train_States (Train).Ability.Get_Ability = Enabled then
         -- if the train is entering a new locked state, initialize substates
         if Train_States (Train).Status.Get_Status = Unlocked then
            Initialize_Locks (Train);
         end if;
         -- change the status state of train to locked by indicated block
         Train_States (Train).Status.Lock;
         Train_States (Train).Locked.Block_Locked (Block => Block);
         -- change the locked substate to locked by block (rest are unlocked)
         Train_States (Train).Locked.Locked_By (Reservation_Failure);
         -- output the proper mesage to the display
         Display.Put (Train  => Train,
                      Status => Train_States (Train).Locked.Get_Lock_Rec);
         DoubleTalk.Speak (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                          ("Train " & Train_ID'Image (Train) & " has been" &
                           " stopped. It can't reserve block " &
                           Layout.Block_ID'Image (Block)),
                           Voice => DoubleTalk.Vader);
      end if;
   end Block_Reserved;

   -- procedure for unlocking a train if a block is freed
   procedure Block_Freed (Train : in Request_ID;
                          Block : in Layout.Block_ID) is
      Success : Boolean;
   begin
      if Train /= 0 and then
        Train_States (Train).Locked.Get_Reason_Status (Reservation_Failure) then
         if Train_States (Train).Locked.Get_Locked_Block = Block then
            -- change locked by block substate to unlocked by block
            Train_States (Train).Locked.Unlocked_By (Reservation_Failure);
            -- check to see if the train is ready to be unlocked
            Check_Locked_States (Train);
            -- output the proper mesage to the display
            Display.Put (Train  => Train,
                         Status => Train_States (Train).Locked.Get_Lock_Rec);
         end if;
      else
         -- if train is enabled and locked by block
         for My_Train in Train_ID'Range loop
            -- If the is locked because of a reservation failure check the
            -- train
            if Train_States (My_Train).Locked.Get_Reason_Status
                                      (Reservation_Failure) then
               if Train_States (My_Train).Locked.Get_Locked_Block = Block then
                  -- Change locked by block substate to unlocked by block
                  Train_States (My_Train).Locked.Unlocked_By
                                                (Reservation_Failure);

                  -- Change the turnout at the end of the newly reserved
                  -- block if one exists
                  Change_Turnout (Train     => My_Train,
                                  Block     => Occupied_Blocks.Get_Block
                                    (Train => My_Train,
                                     Block => 1),
                                  Direction => Occupied_Blocks.Get_Direction
                                    (Train => My_Train,
                                     Block => 1));
                  -- Reserve the next block
                  Blocks.Reserve (Block     => Block,
                                  Requestor => My_Train,
                                  Success   => Success);

                  -- Check to see if the train is ready to be unlocked
                  if Success then
                     Check_Locked_States (My_Train);
                  end if;

                  -- output the proper mesage to the display
                  Display.Put (Train  => My_Train,
                               Status => Train_States
                                        (My_Train).Locked.Get_Lock_Rec);
                  exit;
               end if;
            end if;
         end loop;
      end if;
   end Block_Freed;

   -- procedure for setting the train backwards
   procedure Set_Backward (Train : in Train_ID) is

      -- Represents the sensor the train is on top of (possibly)
      Loco_Sensor    : Layout.Hall_ID;
      -- Represents the sensor the caboose is on top of (possibly)
      Caboose_Sensor : Layout.Hall_ID;
      -- Represents the block in front of the caboose
      My_Block       : Layout.Block_ID;
      -- Represents the direction of the block in front of the train
      My_Direction   : Layout.Block_Polarity;
      -- Represents the block to add to the caboose
      Block          : Layout.Block_Rec;

   begin
      -- if train is enabled
      if Train_States (Train).Ability.Get_Ability = Enabled then
         -- if train is forward
         if Train_States (Train).Direction.Get_Direction = Forward then
            if Occupied_Blocks.Size (Train) /= 1 then
               -- If the block size is greater then one there exists the
               -- possibility a train is on top of a sensor
               Loco_Sensor := Layout.Sensor_Number
                 (Block_1 => Occupied_Blocks.Get_Block
                  (Train => Train,
                   Block => 1),
                  Block_2 => Occupied_Blocks.Get_Block
                   (Train => Train,
                    Block => 2));
            end if;

            -- Get the block behind the caboose
            Get_Next_Block (Block        => Occupied_Blocks.Get_Block
                             (Train => Train,
                              Block => Occupied_Blocks.Size (Train)),
                            Direction    => Layout.Opposite
                              (Occupied_Blocks.Get_Direction
                                 (Train => Train,
                                  Block => Occupied_Blocks.Size (Train))),
                            Return_Block => My_Block,
                            Return_Dir   => My_Direction);

            -- Determine the sensor that is behind the caboose
            Caboose_Sensor := Layout.Sensor_Number
              (Block_1 => Occupied_Blocks.Get_Block
               (Train => Train,
                Block => Occupied_Blocks.Size (Train)),
               Block_2 => My_Block);
            -- Change direction of train to backward
            Train_States (Train).Direction.Set_Backward;
            -- Change the direction of the occupied block
            Occupied_Blocks.Change_Direction (Train);
            -- Determine if the train is on a sensor
            if Occupied_Blocks.Size (Train) /= 1 then
               if Halls.Is_Triggered (Loco_Sensor) then
               -- If the train is on top of a hall sensor then remove the block
               -- in front of it
                  Occupied_Blocks.Remove (Train);
               end if;
            end if;

            -- Determine if the caboose is on a sensor
            if Halls.Is_Triggered (Caboose_Sensor) then
               -- If the caboose is on top of a hall sensor then add the
               -- block in front of it
               Block := (Block     => My_Block,
                         Direction => My_Direction);
               Occupied_Blocks.Add (Train => Train,
                                    Block =>  Block);
            end if;
            -- Display the train blocks and direction
            Display.Put (Train => Train,
                         Blocks => Get_Blocks (Train));
            Display.Put (Train     => Train,
                         Direction => Trains.Backward);
         end if;
      end if;
   end Set_Backward;

   -- procedure for setting the train forward
   procedure Set_Forward (Train : in Train_ID) is

      -- Represents the sensor the train is on top of (possibly)
      Loco_Sensor    : Layout.Hall_ID;
      -- Represents the sensor the caboose is on top of (possibly)
      Caboose_Sensor : Layout.Hall_ID;
      -- Represents the block in front of the caboose
      My_Block       : Layout.Block_ID;
      -- Represents the direction of the block in front of the train
      My_Direction   : Layout.Block_Polarity;
      -- Represents the block to add to the caboose
      Block          : Layout.Block_Rec;

   begin
      -- if train is enabled
      if Train_States (Train).Ability.Get_Ability = Enabled then
         -- if the train is backward
         if Train_States (Train).Direction.Get_Direction = Backward then
            -- Determine the sensor in between the loco block and the block
            -- behind the loco
            if Occupied_Blocks.Size (Train) /= 1 then
               -- If the block size is greater then one there exists the
               -- possibility a train is on top of a sensor
               Loco_Sensor := Layout.Sensor_Number
                 (Block_1 => Occupied_Blocks.Get_Block
                  (Train => Train,
                   Block => 1),
                  Block_2 => Occupied_Blocks.Get_Block
                   (Train => Train,
                    Block => 2));
            end if;

            -- Get the block behind the caboose
            Get_Next_Block (Block        => Occupied_Blocks.Get_Block
                             (Train => Train,
                              Block => Occupied_Blocks.Size (Train)),
                            Direction    => Layout.Opposite
                              (Occupied_Blocks.Get_Direction
                                 (Train => Train,
                                  Block => Occupied_Blocks.Size (Train))),
                            Return_Block => My_Block,
                            Return_Dir   => My_Direction);

            -- Determine the sensor that is behind the caboose
            Caboose_Sensor := Layout.Sensor_Number
              (Block_1 => Occupied_Blocks.Get_Block
               (Train => Train,
                Block => Occupied_Blocks.Size (Train)),
               Block_2 => My_Block);
            -- Change direction of train to forward
            Train_States (Train).Direction.Set_Forward;
            -- Change the direction of the occupied blocks
            Occupied_Blocks.Change_Direction (Train);
            -- Determine if the train is on a sensor
            if Occupied_Blocks.Size (Train) /= 1 then
               if Halls.Is_Triggered (Loco_Sensor) then
               -- If the train is on top of a hall sensor then remove the block
               -- in front of it
                  Occupied_Blocks.Remove (Train);
               end if;
            end if;

            -- Determine if the caboose is on a sensor
            if Halls.Is_Triggered (Caboose_Sensor) then
               -- If the caboose is on top of a hall sensor then add the
               -- block in front of it
               Block := (Block     => My_Block,
                         Direction => My_Direction);
               Occupied_Blocks.Add (Train => Train,
                                    Block =>  Block);
            end if;
            -- Turn the bell off
            Sound.Bell_Off (Dallee.Dallee_Num (Train));
            -- Display the train blocks and direction
            Display.Put (Train => Train,
                         Blocks => Get_Blocks (Train));
            Display.Put (Train     => Train,
                         Direction => Trains.Forward);
         end if;
      end if;
   end Set_Forward;

   -- procedure for changing a turnout
   procedure Change_Turnout (Train     : in Train_ID;
                             Direction : in Layout.Turn_Choice) is

      -- Represents the turnout to change
      Turnout        : Layout.Turnout_ID;
      -- Represents the joint turnout attached to the turnout to change
      Joint_Turnout  : Layout.Turnout_ID;
      -- Represents the terminator at the end of the loco
      Terminator     : Layout.Terminator_Type;
      -- Represents the status of the turnout to be changed
      Status         : Turnouts.Status_Rec;
      --Represents the block to free at the end of the opposite direction of
      -- a turnout
      Block_To_Free  : Layout.Block_ID;
      -- Represent the block occupied by loco
      Loco_Block     : Layout.Block_ID;
      -- Represents the direction of the block the locomotive is on
      Loco_Dir       : Layout.Block_Polarity;
      -- Represent the direction of the turnout where the block is to be freed
      Free_Direction : Layout.Turnout_Limb;
      -- Represent the direction of the turnout where the blok is to be reserved
      Reserve_Direction : Layout.Turnout_Limb;
      -- Represents whether the reserve was successful or not
      Successful     : Boolean;

   begin
      -- Get the block the loco is on
      Loco_Block := Occupied_Blocks.Get_Block (Train => Train,
                                               Block => 1);
      -- Get the direction of the block
      Loco_Dir   := Occupied_Blocks.Get_Direction (Train => Train,
                                                   Block => 1);
      -- Determine the next choice turnout
      Turnout := Layout.Next_Choice_Turnout (Block     => Loco_Block,
                                             Direction => Loco_Dir);
      -- Determine the state of the turnout
      Status := Turnouts.Status (Turnout => Turnout);
      -- If the state of the turnout is not currently set to or moving to the
      -- direction given then attempt to change the turnout
      if Status.Desired /= Direction then
         -- Determine the terminator at the end of the current block
         Terminator := Layout.Next_Term (Block     => Loco_Block,
                                         Direction => Loco_Dir);

         -- If the terminator is a choice turnout then free the block on the
         -- end of the set limb and reserve the block on the end of desired
         -- limb
         if Terminator = Layout.Turnout and then not Layout.Is_Force_Turnout
           (Block     => Loco_Block, Direction => Loco_Dir) then

            -- If the turnout is a joint turnout then change both limbs
            if Layout.Is_Joint_Turnout (Turnout   => Turnout,
                                        Direction => Direction) then
               Joint_Turnout := Layout.Joint_Turnout (Turnout => Turnout);
               Blocks.Reserve (Block     => Layout.End_Turnout
                               (Turnout   => Joint_Turnout,
                                Direction => Layout.Common),
                               Requestor => Train,
                               Success   => Successful);
               -- If the reservation was successuful then free the proper blocks
               -- and set the tunrouts
               if Successful then
                  if Direction = Layout.Left then
                     Free_Direction := Layout.Right;
                  else
                     Free_Direction := Layout.Left;
                  end if;
                  Block_To_Free := Layout.End_Turnout
                    (Turnout   => Turnout,
                     Direction => Free_Direction);
                  Blocks.Free (Block     => Block_To_Free,
                               Requestor => Train);
                  Block_Freed (Train => Train,
                               Block => Block_To_Free);
                  Turnouts.Set (Requestor => Train,
                                Turnout   => Joint_Turnout,
                                Direction => Direction);
                  Turnouts.Set (Requestor => Train,
                                Turnout   => Turnout,
                                Direction => Direction);
               -- else tell the operator turnout couldn't move
               else
                  DoubleTalk.Speak
                    (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                       ("Couldn't change turnout"),
                     Voice  => DoubleTalk.Vader);
               end if;
            -- else attmept to change the turnout
            else
               if Direction = Layout.Left then
                  Reserve_Direction := Layout.Left;
               else
                  Reserve_Direction := Layout.Right;
               end if;
               Blocks.Reserve (Block     => Layout.End_Turnout
                               (Turnout   => Turnout,
                                Direction => Reserve_Direction),
                               Requestor => Train,
                               Success   => Successful);
               -- If the reservation was successful change the turnout adn free
               -- the proper blocks
               if Successful then
                  if Direction = Layout.Left then
                     Free_Direction := Layout.Right;
                  else
                     Free_Direction := Layout.Left;
                  end if;
                  Block_To_Free := Layout.End_Turnout
                    (Turnout   => Turnout,
                     Direction => Free_Direction);
                  Blocks.Free (Block     => Block_To_Free,
                               Requestor => Train);
                  Block_Freed (Train => Train,
                               Block => Block_To_Free);
                  Turnouts.Set (Requestor => Train,
                                Turnout   => Turnout,
                                Direction => Direction);
               -- else tell the operator couldn't move
               else
                  DoubleTalk.Speak
                    (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                       ("Couldn't change turnout"),
                     Voice  => DoubleTalk.Vader);
               end if;
            end if;
         -- else the turnout is faraway and doesn't need to reserve/free blocks
         else
            -- If the turnout is a joint turnout reserve and free the common
            -- limbs and if successful change the turnout and free the common
            -- limbs
            if Layout.Is_Joint_Turnout (Turnout   => Turnout,
                                        Direction => Direction) then
               Joint_Turnout := Layout.Joint_Turnout (Turnout => Turnout);
               Blocks.Reserve (Block     => Layout.End_Turnout
                               (Turnout   => Joint_Turnout,
                                Direction => Layout.Common),
                               Requestor => Train,
                               Success   => Successful);
               if Successful then
                  Blocks.Reserve (Block     => Layout.End_Turnout
                                  (Turnout   => Turnout,
                                   Direction => Layout.Common),
                                  Requestor => Train,
                                  Success   => Successful);
                  if Successful then
                     Blocks.Free (Block     => Layout.End_Turnout
                                  (Turnout   => Turnout,
                                   Direction => Layout.Common),
                                  Requestor => Train);
                     Blocks.Free (Block     => Layout.End_Turnout
                                  (Turnout   => Joint_Turnout,
                                   Direction => Layout.Common),
                                  Requestor => Train);
                     Turnouts.Set (Requestor => Train,
                                   Turnout   => Joint_Turnout,
                                   Direction => Direction);
                     Turnouts.Set (Requestor => Train,
                                   Turnout   => Turnout,
                                   Direction => Direction);
                  else
                     Blocks.Free (Block     => Layout.End_Turnout
                               (Turnout   => Joint_Turnout,
                                Direction => Layout.Common),
                                  Requestor => Train);
                     DoubleTalk.Speak
                       (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                          ("Couldn't change turnout"),
                        Voice  => DoubleTalk.Vader);
                  end if;
               else
                  DoubleTalk.Speak
                    (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                       ("Couldn't change turnout"),
                     Voice  => DoubleTalk.Vader);
               end if;
            else
               -- else reserve the common limb of the turnout and if successful
               -- change the turnout limb and free the common limb
               Blocks.Reserve (Block     => Layout.End_Turnout
                               (Turnout   => Turnout,
                                Direction => Layout.Common),
                               Requestor => Train,
                               Success   => Successful);
               if Successful then
                  Blocks.Free (Block     => Layout.End_Turnout
                               (Turnout   => Turnout,
                                Direction => Layout.Common),
                               Requestor => Train);
                  Turnouts.Set (Requestor => Train,
                                Turnout   => Turnout,
                                Direction => Direction);
               else
                  DoubleTalk.Speak
                    (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                       ("Couldn't change turnout"),
                     Voice  => DoubleTalk.Vader);
               end if;
            end if;
         end if;
      end if;
   end Change_Turnout;

   -- procedure for sounding the horn
   procedure Sound_Horn (Train : in Train_ID) is
   begin
      -- Sound the given trains horn
      Sound.Sound_Horn (Unit   => Sound.Installed_Range (Train),
                        Signal => Sound.Approach_Highway);
   end Sound_Horn;

   -- procedure to return the given trains min throttle
   function Get_Min_Throttle (Train : in Train_ID)
                              return Common_Units.Percent is
   begin
      -- Return the given train minimum throttle
      return Common_Units.Percent (Train_States (Train).Min_Throttle);
   end Get_Min_Throttle;

   -- procedure for setting the speed of the train(utilizes smart throttle
   -- feature)
   procedure Set_Speed (Train : in Train_ID;
                        Speed : in Common_Units.Percent) is

      -- Equation for computing speed for the lower threshhold
      Lower_Throttle_Multiplier : constant Common_Units.Percent :=
         Common_Units.Percent (Train_States (Train).Min_Throttle) / 10.0;

      -- Equation for computing speed for the upper threshhold
      Upper_Throttle_Multiplier : constant Common_Units.Percent :=
        Common_Units.Percent ((100 - Train_States (Train).Min_Throttle)) / 90.0;

   begin
      if Speed < 10.0 then
         -- If speed is less than 10 use the lower throttle equation
         Cabs.Set (Cab => Train_States (Train).Cab,
                   Value => Integer (Lower_Throttle_Multiplier * Speed));
      else
         -- else speed is greater than 90 so use higher throttle equation
         Cabs.Set (Cab   => Train_States (Train).Cab,
                   Value => Integer (Train_States (Train).Min_Throttle +
           (Locomotives.Percent ((Speed - 10.0) * Upper_Throttle_Multiplier))));
      end if;
      -- Output the speed to the display
      Display.Put (Train    => Train,
                   Throttle => Speed);
   end Set_Speed;

   -- procedure to be called when a hall is triggered
   procedure Hall_Triggered (Hall : in Layout.Hall_ID) is

      -- Represents the terminator at the end of the the loco
      Terminator : Layout.Terminator_Type;
      -- Represents the blocks seperated by the sensor
      Block_1 : Layout.Block_ID;
      Block_2 : Layout.Block_ID;
      -- Represents the train that triggered the hall sensor
      Train   : Train_ID;
      -- Represent the direction of the locomotive\
      Loco_Direction : Layout.Block_Polarity;
      -- Represents the turnout at the end of the loco
      Turnout : Layout.Turnout_ID;
      -- Represents the block at the end of the loco
      Block   : Layout.Block_ID;
      -- Represents the ID and direction of the force turnout at the end of the
      -- loco
      FTurnout : Layout.Turnout_Rec;
      -- Represents the direction and status of the choice turnout
      CTurnout : Turnouts.Status_Rec;
      -- Represents the ID of the joint turnout
      JTurnout : Layout.Turnout_ID;
      -- Represents the block to add and power
      Block_To_Power : Layout.Block_Rec;
      -- Represents the limb to search down if the turnout is a choice turnout
      Limb : Layout.Turn_Choice;
      -- Represents whehter the reserve was successful or not
      Successful : Boolean;

   begin
      -- Determine what blocks the sensor seperates
      Layout.Seperates (Sensor  => Hall,
                        Block_1 => Block_1,
                        Block_2 => Block_2);
      -- Determine which train triggered the sensor
      Train := Occupied_Blocks.Which_Train (Block_1, Block_2);
      -- Set the direction of the loco
      Loco_Direction := Occupied_Blocks.Get_Direction (Train => Train,
                                                       Block => 1);
      -- Determine whether the sensor was triggered by the loco or caboose
      if Blocks.Is_Powered (Block_1) and Blocks.Is_Powered (Block_2) then
         -- If the caboose triggered the sensor free the block, remove it, and
         -- and set the power to the null cab
         Occupied_Blocks.Remove (Train => Train);
      elsif not Blocks.Is_Powered (Block_1)
        and not Blocks.Is_Powered (Block_2) then
         DoubleTalk.Speak
                       (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String
                          ("Unknown sensor trigger, stopping all trains!"),
                        Voice  => DoubleTalk.Vader);
         for Train in Train_ID'Range loop
            Stop_Train (Train);
         end loop;
      else
         -- Else the loco triggered the sensor, add the next block, and power
         -- it in the correct direction. Also set any turnouts in the current
         -- path. Also reserve the next block in the path

         -- Determine if the sensor is reversed
         if Layout.Is_Reverse (Sensor => Hall) then
            -- If reverse, reverse the direction of the block to power
            Block_To_Power.Direction := Layout.Opposite
              (Direction => Loco_Direction);
         else
            -- else keep the direction the same as the loco
            Block_To_Power.Direction := Loco_Direction;
         end if;

         -- Determine which block to add and power
         if Blocks.Is_Powered (Block_1) then
            -- If block 1 is powered then block 2 is the block to power
            Block_To_Power.Block := Block_2;
         else
            -- else block 1 is the block to power
            Block_To_Power.Block := Block_1;
         end if;

         -- Add the block to power to the block list and power the block
         Occupied_Blocks.Add (Train => Train,
                              Block => Block_To_Power);

         -- Determine the terminator at the end of the loco
         Terminator := Layout.Next_Term (Block     => Block_To_Power.Block,
                                         Direction => Block_To_Power.Direction);
         -- Check to see what kind of terminator was at the end of the loco
         if Terminator = Layout.Block then
            -- If it was a block, reserve the block
            Block := Layout.End_Block (Block     => Block_To_Power.Block,
                                       Direction => Block_To_Power.Direction);
            Blocks.Reserve (Block     => Block,
                            Requestor => Train,
                            Success   => Successful);
            -- Check to see if the reserve was successful
            if not Successful then
               -- If it wasn't successful, call the block reserved function
               Block_Reserved (Train, Block);
            end if;
         elsif Terminator = Layout.Turnout then
            -- Elsif the terminator is a turnout, check for the kind of turnout

            -- Determine the turnout id at the end of the loco
            Turnout := Layout.End_Block (Block     => Block_To_Power.Block,
                                         Direction => Block_To_Power.Direction);
            -- Check for the kind of turnout
            if Layout.Is_Force_Turnout (Block     => Block_To_Power.Block,
                                        Direction => Block_To_Power.Direction)
            then
               FTurnout := Layout.Force_Turnout
                 (Block     => Block_To_Power.Block,
                  Direction => Block_To_Power.Direction);

               -- If force turnout, reserve the block on its common limb and
               -- set it in the proper direction
               Block := Layout.End_Turnout (Turnout   => Turnout,
                                            Direction => Layout.Common);
               Blocks.Reserve (Block     => Block,
                               Requestor => Train,
                               Success   => Successful);
               -- Check to see if the reserve was successful
               if Successful then
                  -- If it was successful than change the turnout
                  Turnouts.Set (Requestor => Train,
                                Turnout   => FTurnout.Turnout,
                                Direction => FTurnout.Direction);
               else
                  -- If it wasn't successful, call the block reserved function
                  Block_Reserved (Train, Block);
               end if;
            else
               -- Else the turnout is a choice turnout

               -- Get the current position of the choice turnout
               CTurnout := Turnouts.Status (Turnout => Turnout);
               if CTurnout.Current = Turnouts.Fully_Left or
                 CTurnout.Current = Turnouts.Moving_Left then
                  -- If left or moving left, limb to search = left
                  Limb := Layout.Left;
               elsif CTurnout.Current = Turnouts.Fully_Right or
                 CTurnout.Current = Turnouts.Moving_Right then
                  -- If right or moving right, limb to search = right
                  Limb := Layout.Right;
               end if;

               -- Determine if it is a joint turnout on the limb to search
               if Layout.Is_Joint_Turnout (Turnout   => Turnout,
                                           Direction => Limb) then
                  -- If the turnout is a joint turnout, then reserve the block
                  -- on its common limb and if successful set the turnout
                  JTurnout := Layout.Joint_Turnout (Turnout => Turnout);
                  Block := Layout.End_Turnout (Turnout   => JTurnout,
                                               Direction => Layout.Common);
                  Blocks.Reserve (Block     => Block,
                                  Requestor => Train,
                                  Success   => Successful);
                  -- Check to see if the reserve was successful
                  if Successful then
                     -- If it was successful than change the turnout
                     Turnouts.Set (Requestor => Train,
                                   Turnout   => JTurnout,
                                   Direction => Limb);
                  else
                     -- If not successful, call the block reserved function
                     Block_Reserved (Train, Block);
                  end if;
               else
                  -- Else the turnout is just a choice turnout and it reserves
                  -- the block on the end of its current limb
                  if Limb = Layout.Right then
                     Block := Layout.End_Turnout (Turnout   => Turnout,
                                                  Direction => Layout.Right);
                  else
                     Block := Layout.End_Turnout (Turnout   => Turnout,
                                                  Direction => Layout.Left);
                  end if;
                  Blocks.Reserve (Block     => Block,
                                  Requestor => Train,
                                  Success   => Successful);
                  -- Check to see if the reserve was successful
                  if not Successful then
                     -- If not successful, call the block reserved function
                     Block_Reserved (Train, Block);
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Hall_Triggered;

   -- function to return the current loco
   function Get_Loco (Train : in Train_ID) return Locomotives.Loco_Rec is

      -- Represents the locomotive to return
      Locomotive : Locomotives.Loco_Rec;

   begin
      -- Assign the name and minimum throttle then return the locomotive rec
      Locomotive.Name := Train_States (Train).Loco_Name;
      Locomotive.Minimum_Throttle := Train_States (Train).Min_Throttle;
      return Locomotive;
   end Get_Loco;

   -- function to return the current direction of the train
   function Get_Direction (Train : in Train_ID) return Direction_Type is
   begin
      -- Return the direction of the train
      return Train_States (Train).Direction.Get_Direction;
   end Get_Direction;

   -- procedure for restarting the train states, blocks, etc.
   procedure Restart is
   begin
      -- Stop all trains
      for Cab in Cabs.Control_Cab_ID loop
         Cabs.Set_Limit (Cab   => Cab,
                         Value => 0);
      end loop;
      -- Clear all train lists
      for Train in Train_ID'Range loop
         Occupied_Blocks.Clear (Train);
         Hold_Train (Train);
      end loop;
   end Restart;

begin

   -- Set callbacks and interrupts
   Turnouts.Set_Failure_Callback (To => Turnout_Failed'Access);
   Turnouts.Set_Recovery_Callback (To => Turnout_Fixed'Access);
   Turnouts.Set_Change_Callback (To => Display.Put'Access);
   Halls.Enable (Callback => Trains.Operations.Hall_Triggered'Access);

end Trains.Operations;
