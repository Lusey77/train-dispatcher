--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By  : John McCormick ------------------------------------------------
-- Updated     : March 2008 ----------------------------------------------------
--------------------------------------------------------------------------------
package Locomotives is

   pragma Pure (Locomotives);

   -- This package defines the characteristics of the locomotives in the lab

   -- Types for locomotive descriptive information
   subtype Loco_String is String (1 .. 40);
   subtype Percent     is Integer range 0 .. 100;

   type Loco_Rec is
      record
         Name             : Loco_String;
         Minimum_Throttle : Percent;
      end record;


   -- An array of locomotive descriptions
   type Loco_Array is array (Positive range <>) of Loco_Rec;
   Available_Locos : constant Loco_Array :=
                       (1  => ("Burlington           EMD GP40      #187 ", 20),
                        2  => ("Burlington Northern  EMD GP30      #2236", 55),
                        3  => ("Burlington Northern  ALCO C-424    #4246", 20),
                        4  => ("Delaware & Hudson    ALCO C-424    #451 ", 24),
                        5  => ("Delaware & Hudson    Baldwin RF16  #1205", 32),
                        6  => ("New York Central     Hudson 4-6-4  #5442", 36),
                        7  => ("Santa Fe             GE U23B       #2150", 21),
                        8  => ("Santa Fe             ALCO RS1      #2397", 22),
                        9  => ("Santa Fe             ALCO S-2      #2352", 38),
                        10 => ("Union Pacific        GE U23B       #542 ", 25));
end Locomotives;
