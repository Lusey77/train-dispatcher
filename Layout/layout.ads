--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested  By : Kaleb Luse -----------------------------------------------------
-- Updated    : 17 Feb 15 ------------------------------------------------------
--------------------------------------------------------------------------------
package Layout is
   pragma Pure (Layout);

   -- This package implements the map class and defines the necassary
   -- types and operations needed for the map class.

   -- Represents the types of terminators on the map
   type Terminator_Type is (Block, Turnout, Dead_End);
   -- Represents the polarity/direction of the block
   type Block_Polarity  is (Normal, Reverze);
   -- Represents the turnout limbs
   type Turnout_Limb    is (Left, Common, Right);
   -- Represents the direction of the turnout limb
   type Turn_Choice     is (Left, Right);

   -- Represents the unique ID for a block
   type Block_ID   is range 1 .. 40;
   -- Represents the unique ID for a turnout
   type Turnout_ID is range 1 .. 26;
   -- Represents the unique ID for a sensor
   type Hall_ID    is range 1 .. 51;

   -- Represents the types of terminators on the end of blocks
   type Terminator_Rec (Term : Terminator_Type := Block) is
      record
         case Term is
            when Block =>
               Block       : Block_ID;
            when Turnout =>
               Turnout     : Turnout_ID;
            when Dead_End =>
               null;
         end case;
      end record;

   -- Represents a turnout
   type Turnout_Rec is   -- Elements in the list of blocks
      record
         Turnout   : Turnout_ID;
         Direction : Turn_Choice;
      end record;

   type Block_Rec is   -- Elements in the list of blocks
      record
         Block     : Block_ID;
         Direction : Block_Polarity;
      end record;
   type Block_Array is array (Positive range <>) of Block_Rec;

   -- The value of the record discriminant Max_Size determines the maximum
   -- number of blocks in the list.  This discriminant is set when a variable
   -- of type Block_List is declared
   type Block_List (Max_Size : Positive) is
      record
         Size  : Natural := 0;
         Items : Block_Array (1 .. Max_Size);
      end record;

--------------------------------------------------------------------------------
   -- Preconditions  : None
   --
   -- Postconditions : A block, turnout, or dead_end is returned
   --
   -- Purpose        : Operation to determine terminator type at the end of a
   --                  block
   function Next_Term (Block     : in Block_ID;
                       Direction : in Block_Polarity) return Terminator_Type;

   -- Preconditions  : None
   --
   -- Postconditions : The block_id at the end of the turnout is returned
   --
   -- Purpose        : Operation to determine block at the end of a turnout
   function End_Turnout (Turnout   : in Turnout_ID;
                         Direction : in Turnout_Limb) return Block_ID;

   -- Preconditions  : Only a block with a block at the given end is passsed in
   --
   -- Postconditions : The block_is at the end of the block is returned
   --
   -- Purpose        : Operation to determine the block number at the end of a
   --                  given block
   function End_Block (Block     : in Block_ID;
                       Direction : in Block_Polarity) return Block_ID;

   -- Preconditions  : Only a block with a turnout at the given end is passed in
   --
   -- Postconditions : The turnout_id at the end of the block is returned
   --
   -- Purpose        : Operation to determine the turnout number at the end of a
   --                  given block
   function End_Block (Block     : in Block_ID;
                       Direction : in Block_Polarity) return Turnout_ID;

   -- Preconditions  : A sensor must exist between the two blocks passed in
   --
   -- Postconditions : The id of the sensor seperating the blocks is passed out
   --
   -- Purpose        : Operation to determine the sensor number in between two
   --                  given blocks
   function Sensor_Number (Block_1 : in Block_ID;
                           Block_2 : in Block_ID) return Hall_ID;

   -- Preconditions  : None
   --
   -- Postconditions : Return whether the sensor is reversing or not
   --
   -- Purpose        : Operation to determine whether a sensor is reversed
   --                  or not
   function Is_Reverse (Sensor : in Hall_ID) return Boolean;

   -- Preconditions  : None
   --
   -- Postconditions : Returns whether the block is a cross block or not
   --
   -- Purpose        : Operation that determines whether a block is a cross
   --                  block or not
   function Is_Cross_Block (Block : in Block_ID) return Boolean;

   -- Preconditions  : None
   --
   -- Postconditions : Return the blocks a block is crossing over
   --
   -- Purpose        : Operation that determines the blocks that another block
   --                  crosses over
   function Cross_Block (Block : in Block_ID) return Block_List;

   -- Preconditions  : None
   --
   -- Postconditions : Return the opposite of the given block polarity
   --
   -- Purpose        : Operation to return the opposite of a given block
   --                  direction
   function Opposite (Direction : in Block_Polarity) return Block_Polarity;

   -- Preconditions  : None
   --
   -- Postconditions : Return the opposite of the given turn choice
   --
   -- Purpose        : Operation to return the opposite of a given turn choice
   function Opposite (Direction : in Turn_Choice) return Turn_Choice;

   -- Preconditions  : None
   --
   -- Postconditions : Return the two blocks the seperate the given sensor
   --
   -- Purpose        : Operation to return the ID's of the two blocks seperated
   --                  by a sensor
   procedure Seperates (Sensor  : in  Hall_ID;
                        Block_1 : out Block_ID;
                        Block_2 : out Block_ID);

   -- Preconditions  : The block_id passed in must have a turnout at the given
   --                  end of it
   --
   -- Postconditions : Return whether or not the turnout is a force turnout
   --
   -- Purpose        : Operation to determine if there is a force turnout at the
   --                  end of block
   function Is_Force_Turnout (Block     : in Block_ID;
                              Direction : in Block_Polarity) return Boolean;

   -- Preconditions  : The block passed in must have a force turnout on the
   --                  given direction of it
   --
   -- Postconditions : The id and turn choice of the turnout are passed out
   --
   -- Purpose        : Operation to determine the ID of the force turnout at
   --                  the end of a block and the direction it should be set to
   function Force_Turnout (Block     : in Block_ID;
                           Direction : in Block_Polarity) return Turnout_Rec;

   -- Preconditions  : None
   --
   -- Postconditions : Return whether the turnout limb is a joint turnout or not
   --
   -- Purpose        : Operation to determine if the turnout limb is a
   --                  joint turnout
   function Is_Joint_Turnout (Turnout   : in Turnout_ID;
                              Direction : in Turn_Choice) return Boolean;

   -- Preconditions  : Only a joint turnout is passed in
   --
   -- Postconditions : The id of the joint turnout at the end of the turnout
   --                  is returned
   --
   -- Purpose        : Operation to determine the ID of the joint turnout
   function Joint_Turnout (Turnout   : in Turnout_ID) return Turnout_ID;

   -- Preconditions  : None
   --
   -- Postconditions : The id of the next choice turnout is returned
   --
   -- Purpose        : Operation to find the next choice turnout from the
   --                  given block
   function Next_Choice_Turnout (Block     : in Block_ID;
                                 Direction : in Block_Polarity)
                                 return Turnout_ID;
end Layout;

