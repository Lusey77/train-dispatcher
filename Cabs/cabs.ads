package Cabs is

   pragma Elaborate_Body (Cabs);

   -- Cabs are the power sources that are connected to
   -- blocks of track to power the trains.

   -- Cab output to the blocks can be set to a
   -- given percentage of maximum output.

   -- Cabs have a limit on their settings.  Attempts to
   -- set a cab above its limit results in setting the
   -- cab to its limit.

   -- The limit of each cab is initially zero.

   -- Written by John W. McCormick March 2002


   ----------------------------------------------------------------------------
   -- All cabs including null (zero voltage) cabs
   type Cab_ID is range 0 .. 7;

   -- Cabs whose power we can set
   subtype Control_Cab_ID is Cab_ID range 1 .. 6;

   Null_Cab     : constant Cab_ID := 0;  -- Cabs that can supply only zero
   Aux_Null_Cab : constant Cab_ID := 7;  -- volts to the blocks

   subtype Percent is Integer range 0 .. 100;

   ----------------------------------------------------------------------------
   procedure Set (Cab   : in Control_Cab_ID;
                  Value : in Percent);
   -- Set the output of the Cab
   --
   -- Preconditions  : None
   --
   -- Postconditions : The Cab output is set to the minimum of Value
   --                  and the cab's current limit

   ----------------------------------------------------------------------------
   procedure Get (Cab   : in  Cab_ID;
                  Value : out Percent);
   -- Get the output for Cab
   --
   -- Preconditions  : none
   --
   -- Postconditions : The Cab output (percent of maximum possible) is returned

   ----------------------------------------------------------------------------
   procedure Set_Limit (Cab   : in Control_Cab_ID;
                        Value : in Percent);
   -- Set the limit of the Cab
   --
   -- Preconditions  : None
   --
   -- Postconditions : The Cab's limit is set to Value percent of
   --                     Maximum Possible
   --                  If the current power level of Cab is greater than Value,
   --                     the power level is reduced to Value

   ----------------------------------------------------------------------------
   procedure Get_Limit (Cab   : in  Cab_ID;
                        Value : out Percent);
   -- Get the output for Cab
   --
   -- Preconditions  : none
   --
   -- Postconditions : The Cab's limit(percent of maximum possible) is returned


end Cabs;
