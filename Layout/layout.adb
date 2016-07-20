--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested  By : Kaleb Luse -----------------------------------------------------
-- Updated    : 17 Feb 15 ------------------------------------------------------
--------------------------------------------------------------------------------
package body Layout is

   -- Represents an array of Block_ID's whose row corresponds to its splitting
   -- sensor
   type Sensor_Array  is array (Hall_ID, 1 .. 2) of Block_ID;

   -- Represents an array indexed by Block_ID and Polarity and whose values
   -- give the type of terminator at the end of a block
   type Term_Array    is array (Block_ID, Block_Polarity) of Terminator_Rec;

   -- Represents an array indexed by Turnout_ID and Turnout_Limb and whose
   -- values represent the blocks on the corresponding limb of the turnout
   type Turnout_Array is array (Turnout_ID, Turnout_Limb) of Block_ID;

   -- Represents an array of turnouts indexed by block_id and polarity
   type CTurnout_Array is array (Block_ID, Block_Polarity) of Turnout_ID;

   -- An array that holds the next choice turnout for the given block and
   -- direction
   Next_CTurnout_Array : constant CTurnout_Array :=
                                      ((06, 13), (06, 03), (12, 03), (12, 18),
                                       (12, 18), (12, 22), (12, 23), (12, 10),
                                       (15, 10), (15, 13), (03, 13), (16, 13),
                                       (02, 16), (05, 16), (05, 04), (17, 04),
                                       (17, 07), (17, 08), (21, 08), (21, 19),
                                       (21, 20), (12, 20), (11, 20), (11, 20),
                                       (05, 20), (05, 14), (05, 10), (20, 04),
                                       (15, 17), (17, 15), (01, 16), (21, 01),
                                       (21, 01), (21, 01), (21, 09), (21, 01),
                                       (21, 01), (21, 01), (21, 12), (12, 26));

   -- Represents all the turnouts and their corresponding blocks in the layout
   Turnouts : constant Turnout_Array := ((13, 31, 12), (02, 13, 14),
                                         (13, 02, 01), (26, 15, 14),
                                         (16, 15, 28), (03, 02, 17),
                                         (16, 17, 02), (30, 18, 17),
                                         (34, 35, 38), (39, 08, 07),
                                         (25, 24, 28), (09, 08, 27),
                                         (09, 10, 29), (25, 26, 27),
                                         (11, 10, 12), (36, 31, 32),
                                         (04, 18, 19), (18, 04, 03),
                                         (35, 20, 19), (39, 21, 20),
                                         (22, 21, 23), (22, 06, 05),
                                         (06, 07, 40), (40, 40, 40),
                                         (40, 40, 40), (40, 40, 40));

   -- Represents all the sensors and their corresponding blocks in the layout
   Sensors  : constant Sensor_Array  := ((01, 11), (31, 13), (31, 12),
                                         (31, 36), (31, 32), (10, 11),
                                         (10, 12), (13, 14), (13, 02),
                                         (02, 01), (15, 14), (15, 26),
                                         (36, 37), (33, 32), (15, 16),
                                         (15, 28), (26, 25), (26, 27),
                                         (10, 09), (10, 29), (17, 16),
                                         (17, 02), (02, 03), (33, 34),
                                         (37, 38), (28, 24), (25, 24),
                                         (27, 08), (09, 08), (29, 30),
                                         (08, 39), (08, 07), (18, 17),
                                         (18, 30), (35, 38), (35, 34),
                                         (24, 23), (04, 03), (04, 18),
                                         (19, 18), (20, 19), (20, 35),
                                         (21, 20), (21, 39), (05, 04),
                                         (21, 23), (21, 22), (06, 22),
                                         (06, 05), (07, 06), (07, 40));

   -- Represents the terminators at the end of the blocks for our layout
   Map : constant Term_Array := (((Turnout, 03), (Block,   11)),
                                 ((Turnout, 06), (Turnout, 03)),
                                 ((Turnout, 18), (Turnout, 06)),
                                 ((Block,   05), (Turnout, 18)),
                                 ((Turnout, 22), (Block,   04)),
                                 ((Turnout, 23), (Turnout, 22)),
                                 ((Turnout, 10), (Turnout, 23)),
                                 ((Turnout, 12), (Turnout, 10)),
                                 ((Turnout, 13), (Turnout, 12)),
                                 ((Turnout, 15), (Turnout, 13)),
                                 ((Block,   01), (Turnout, 15)),
                                 ((Turnout, 01), (Turnout, 15)),
                                 ((Turnout, 02), (Turnout, 01)),
                                 ((Turnout, 04), (Turnout, 02)),
                                 ((Turnout, 05), (Turnout, 04)),
                                 ((Turnout, 07), (Turnout, 05)),
                                 ((Turnout, 08), (Turnout, 07)),
                                 ((Turnout, 17), (Turnout, 08)),
                                 ((Turnout, 19), (Turnout, 17)),
                                 ((Turnout, 20), (Turnout, 19)),
                                 ((Turnout, 21), (Turnout, 20)),
                                 ((Turnout, 22), (Turnout, 21)),
                                 ((Block,   24), (Turnout, 21)),
                                 ((Turnout, 11), (Block,   23)),
                                 ((Turnout, 14), (Turnout, 11)),
                                 ((Turnout, 04), (Turnout, 14)),
                                 ((Turnout, 14), (Turnout, 12)),
                                 ((Turnout, 11), (Turnout, 05)),
                                 ((Turnout, 13), (Block,   30)),
                                 ((Turnout, 08), (Block,   29)),
                                 ((Turnout, 01), (Turnout, 16)),
                                 ((Block,   33), (Turnout, 16)),
                                 ((Block,   34), (Block,   32)),
                                 ((Turnout, 09), (Block,   33)),
                                 ((Turnout, 19), (Turnout, 09)),
                                 ((Block,   37), (Turnout, 16)),
                                 ((Block,   38), (Block,   36)),
                                 ((Turnout, 09), (Block,   37)),
                                 ((Turnout, 20), (Turnout, 10)),
                                 ((Turnout, 23), (Term => Dead_End)));

   ---------------
   -- Next_Term --
   ---------------

   function Next_Term (Block     : in Block_ID;
                       Direction : in Block_Polarity) return Terminator_Type is
   begin

      return Map (Block, Direction).Term;

   end Next_Term;

   -----------------
   -- End_Turnout --
   -----------------

   function End_Turnout (Turnout   : in Turnout_ID;
                         Direction : in Turnout_Limb) return Block_ID is
   begin
      return Turnouts (Turnout, Direction);
   end End_Turnout;

   ---------------
   -- End_Block --
   ---------------
   -- Preconditions : Only blocks with blocks attached to them are passed in

   function End_Block (Block     : in Block_ID;
                       Direction : in Block_Polarity) return Block_ID is
   begin
      return Map (Block, Direction).Block;
   end End_Block;

   ---------------
   -- End_Block --
   ---------------
   -- Preconditions : Only blocks with turnouts attached to them are passed in

   function End_Block (Block     : in Block_ID;
                       Direction : in Block_Polarity) return Turnout_ID is
   begin
      return Map (Block, Direction).Turnout;
   end End_Block;

   -------------------
   -- Sensor_Number --
   -------------------

   function Sensor_Number (Block_1 : in Block_ID;
                           Block_2 : in Block_ID) return Hall_ID is

      Sensor : Hall_ID;

   begin
      for Row in Hall_ID'Range loop
         if (Block_1 = Sensors (Row, 1) and Block_2 = Sensors (Row, 2)) or
            (Block_1 = Sensors (Row, 2) and Block_2 = Sensors (Row, 1)) then
            Sensor := Row;
         end if;
      end loop;
      return Sensor;
   end Sensor_Number;

   ----------------
   -- Is_Reverse --
   ----------------

   function Is_Reverse (Sensor : in Hall_ID) return Boolean is
   begin
      return (Sensor = 3 or Sensor = 4 or Sensor = 5 or Sensor = 26 or
         Sensor = 30 or Sensor = 31);
   end Is_Reverse;

   function Is_Cross_Block (Block : in Block_ID) return Boolean is
   begin
      return Block = 39 or Block = 23 or Block = 30 or
        Block = 24 or Block = 29 or Block = 8;
   end Is_Cross_Block;

   -----------------
   -- Cross_Block --
   -----------------

   function Cross_Block (Block : in Block_ID) return Block_List is

      Blocks : Block_List (Max_Size => 2);

   begin
      if Block = 39 or Block = 23 then
         Blocks.Size := 2;
         Blocks.Items := ((39, Normal), (23, Normal));
      elsif Block = 30 or Block = 24 then
         Blocks.Size := 2;
         Blocks.Items := ((30, Normal), (24, Normal));
      elsif Block = 29 or Block = 8 then
         Blocks.Size := 2;
         Blocks.Items := ((29, Normal), (8, Normal));
      else
         Blocks.Size := 1;
         Blocks.Items := ((Block, Normal), (Block, Normal));
      end if;
      return Blocks;
   end Cross_Block;

   --------------
   -- Opposite --
   --------------

   function Opposite (Direction : in Block_Polarity) return Block_Polarity is

      Opposite : Block_Polarity;

   begin
      if Direction = Normal then
         Opposite := Reverze;
      else
         Opposite := Normal;
      end if;
      return Opposite;
   end Opposite;

   --------------
   -- Opposite --
   --------------

   function Opposite (Direction : in Turn_Choice) return Turn_Choice is

      Opposite : Turn_Choice;

   begin
      if Direction = Left then
         Opposite := Right;
      else
         Opposite := Left;
      end if;
      return Opposite;
   end Opposite;

   ---------------
   -- Seperates --
   ---------------


   procedure Seperates (Sensor  : in  Hall_ID;
                        Block_1 : out Block_ID;
                        Block_2 : out Block_ID) is

   begin
      Block_1 := Sensors (Sensor, 1);
      Block_2 := Sensors (Sensor, 2);
   end Seperates;

   ----------------------
   -- Is_Force_Turnout --
   ----------------------

   function Is_Force_Turnout (Block     : in Block_ID;
                              Direction : in Block_Polarity) return Boolean is
   begin
      return ((Block = Turnouts (Map (Block, Direction).Turnout, Left)) or
         (Block = Turnouts (Map (Block, Direction).Turnout, Right)));
   end Is_Force_Turnout;

   -------------------
   -- Force_Turnout --
   -------------------
   -- Preconditions : Only blocks with force turnouts at the end are passed in

   function Force_Turnout (Block     : in Block_ID;
                           Direction : in Block_Polarity) return Turnout_Rec is

      Turnout : Turnout_Rec;

   begin
      if Block = Turnouts (Map (Block, Direction).Turnout, Left) then
         Turnout := (Map (Block, Direction).Turnout, Left);
      else
         Turnout := (Map (Block, Direction).Turnout, Right);
      end if;
      return Turnout;
   end Force_Turnout;

   -------------------
   -- Joint_Turnout --
   -------------------
   -- Preconditions : Only joint turnouts are passed in

   function Joint_Turnout (Turnout : in Turnout_ID) return Turnout_ID is

      Joint_Turnout : Turnout_ID;

   begin
      if Turnout = 2 then
         Joint_Turnout := 3;
      elsif Turnout = 3 then
         Joint_Turnout := 2;
      elsif Turnout = 6 then
         Joint_Turnout := 7;
      elsif Turnout = 7 then
         Joint_Turnout := 6;
      elsif Turnout = 17 then
         Joint_Turnout := 18;
      elsif Turnout = 18 then
         Joint_Turnout := 17;
      end if;
      return Joint_Turnout;
   end Joint_Turnout;

   ----------------------
   -- Is_Joint_Turnout --
   ----------------------

   function Is_Joint_Turnout (Turnout   : in Turnout_ID;
                              Direction : in Turn_Choice) return Boolean is
   begin
      return (Turnout = 2 and Direction = Left) or
              (Turnout = 3 and Direction = Left) or
              (Turnout = 6 and Direction = Right) or
              (Turnout = 7 and Direction = Right) or
              (Turnout = 17 and Direction = Left) or
              (Turnout = 18 and Direction = Left);
   end Is_Joint_Turnout;


   -------------------------
   -- Next_Choice_Turnout --
   -------------------------

   function Next_Choice_Turnout (Block     : in Block_ID;
                                 Direction : in Block_Polarity)
   return Turnout_ID is

   begin
      return Next_CTurnout_Array (Block, Direction);
   end Next_Choice_Turnout;
end Layout;
