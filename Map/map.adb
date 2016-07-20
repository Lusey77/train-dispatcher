-- Assumptions : A block can only cross over one other block
-- Trains cannot run into themselves

package body Map is

   -- Represents the types of terminators on the map
   type Terminator_Type is (Block, Force_Turnout, Choice_Turnout,
                            Joint_Turnout, Dead_End);
   -- Represents the polarity/direction of the block
   -- type Block_Polarity  is (Normal, Reverze);
   -- Represents the turnout limbs
   -- type Turnout_Limb    is (Left, Right, Common);
   -- Represents the direction of the turnout limb
   -- subtype Turn_Choice is Turnout_Limb range Left .. Right;

   -- Represents the number of blocks on the map
   Num_Blocks : constant Integer := 40;

   -- Represents the number of turnouts on the map
   Num_Turnouts : constant Integer := 26;

   -- Represents the number of sensors on the map
   -- Num_Sensors : constant Integer := 51;

   -- Represents the unique ID for a block
   type Block_ID   is range 1 .. Num_Blocks;

   -- Represents the unique ID for a turnout
   type Turnout_ID is range 1 .. Num_Turnouts;

   -- Represents the unique ID for a sensor
   -- type Hall_ID    is range 1 .. Num_Sensors;

   -- Represents the max range for the length of a block
   type Length_Type is range 1 .. 1_000_000;

   -- Represents a percentage
   type Percent is delta 1.0 / 10.0 range 0.0 .. 100.0;

   type Terminator_Rec (Term : Terminator_Type := Dead_End) is
      record
         case Term is
            when Block =>
               Block      : Block_ID;
            when Choice_Turnout | Joint_Turnout =>
               C_Turnout    : Turnout_ID;
               Left_Limb  : Block_ID;
               Right_Limb : Block_ID;
            when Force_Turnout =>
               F_Turnout : Turnout_ID;
               F_Block   : Block_ID;
            when Dead_End =>
               null;
         end case;
      end record;

   type Block_Rec is
      record
         My_Length       : Length_Type;
         Norm_Term       : Terminator_Rec;
         Rev_Term        : Terminator_Rec;
      end record;

   type Map_Type is array (Block_ID) of Block_Rec;

   Map : Map_Type := ((My_Length      => 1_000_000,
                       Norm_Term      =>
                         (Term       => Force_Turnout,
                          F_Turnout  => 3,
                          F_Block    => 2),
                       Rev_Term       =>
                         (Term  => Block,
                          Block => 11)),
                       (My_Length      => 1_000_000,
                        Norm_Term      =>
                          (Term       => Joint_Turnout,
                           C_Turnout  => 6,
                           Left_Limb  => 3,
                           Right_Limb => 17),
                        Rev_Term       =>
                          (Term       => Joint_Turnout,
                           C_Turnout  => 3,
                           Left_Limb  => 13,
                           Right_Limb => 1)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 18,
                            F_Block    => 4),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 6,
                            F_Block    => 2)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term  => Block,
                            Block => 5),
                         Rev_Term       =>
                           (Term       => Joint_Turnout,
                            C_Turnout  => 18,
                            Left_Limb  => 18,
                            Right_Limb => 3)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 22,
                            F_Block    => 6),
                         Rev_Term       =>
                           (Term  => Block,
                            Block => 4)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 23,
                            F_Block    => 7),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 22,
                            Left_Limb  => 22,
                            Right_Limb => 5)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 10,
                            F_Block    => 8),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 23,
                            Left_Limb  => 6,
                            Right_Limb => 40)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 12,
                            Left_Limb  => 9,
                            Right_Limb => 27),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 10,
                            Left_Limb  => 39,
                            Right_Limb => 7)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 13,
                            F_Block    => 10),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 12,
                            F_Block    => 8)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 15,
                            Left_Limb  => 12,
                            Right_Limb => 11),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 13,
                            Left_Limb  => 9,
                            Right_Limb => 29)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term  => Block,
                            Block => 1),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 15,
                            F_Block    => 10)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 1,
                            F_Block    => 31),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 15,
                            F_Block    => 10)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Joint_Turnout,
                            C_Turnout  => 2,
                            Left_Limb  => 2,
                            Right_Limb => 14),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 1,
                            F_Block    => 31)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 4,
                            F_Block    => 15),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 2,
                            F_Block    => 13)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 5,
                            Left_Limb  => 16,
                            Right_Limb => 28),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 4,
                            Left_Limb  => 26,
                            Right_Limb => 14)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 7,
                            F_Block    => 17),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 5,
                            F_Block    => 15)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 8,
                            F_Block    => 18),
                         Rev_Term       =>
                           (Term       => Joint_Turnout,
                            C_Turnout  => 7,
                            Left_Limb  => 16,
                            Right_Limb => 2)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Joint_Turnout,
                            C_Turnout  => 17,
                            Left_Limb  => 4,
                            Right_Limb => 19),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 8,
                            Left_Limb  => 30,
                            Right_Limb => 17)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 19,
                            F_Block    => 20),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 17,
                            F_Block    => 18)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 20,
                            F_Block    => 21),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 19,
                            Left_Limb  => 35,
                            Right_Limb => 19)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 21,
                            Left_Limb  => 22,
                            Right_Limb => 23),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 20,
                            Left_Limb  => 39,
                            Right_Limb => 20)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 22,
                            F_Block    => 6),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 21,
                            F_Block    => 21)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term  => Block,
                            Block => 24),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 21,
                            F_Block    => 21)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 11,
                            Left_Limb  => 25,
                            Right_Limb => 28),
                         Rev_Term       =>
                           (Term  => Block,
                            Block => 23)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 14,
                            F_Block    => 26),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 11,
                            F_Block    => 24)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 4,
                            F_Block    => 15),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 14,
                            Left_Limb  => 25,
                            Right_Limb => 27)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 14,
                            F_Block    => 26),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 12,
                            F_Block    => 8)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 11,
                            F_Block    => 24),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 5,
                            F_Block    => 15)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 13,
                            F_Block    => 10),
                         Rev_Term       =>
                           (Term  => Block,
                            Block => 30)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 8,
                            F_Block    => 18),
                         Rev_Term       =>
                           (Term  => Block,
                            Block => 29)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 1,
                            Left_Limb  => 13,
                            Right_Limb => 12),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 16,
                            Left_Limb  => 36,
                            Right_Limb => 32)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term  => Block,
                            Block => 33),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 16,
                            F_Block    => 31)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term  => Block,
                            Block => 34),
                         Rev_Term       =>
                           (Term  => Block,
                            Block => 32)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 9,
                            F_Block    => 35),
                         Rev_Term       =>
                           (Term  => Block,
                            Block => 33)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 19,
                            F_Block    => 20),
                         Rev_Term       =>
                           (Term       => Choice_Turnout,
                            C_Turnout  => 9,
                            Left_Limb  => 34,
                            Right_Limb => 38)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term  => Block,
                            Block => 37),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 16,
                            F_Block    => 31)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term  => Block,
                            Block => 38),
                         Rev_Term       =>
                           (Term  => Block,
                            Block => 36)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 9,
                            F_Block    => 35),
                         Rev_Term       =>
                           (Term  => Block,
                            Block => 37)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 20,
                            F_Block    => 21),
                         Rev_Term       =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 10,
                            F_Block    => 8)),
                        (My_Length      => 1_000_000,
                         Norm_Term      =>
                           (Term       => Force_Turnout,
                            F_Turnout  => 23,
                            F_Block    => 7),
                         Rev_Term       => (Term => Dead_End)));

   procedure Test is

   begin
      null;
   end Test;

end Map;
