with Layout;
package Motors is

   pragma Elaborate_Body;

   -- Written by John McCormick, February 2002
   -- Additional documentation, May 2008

   -- Assumption for Layout.Turn_Choice
   --     Left is stored as 0
   --     Right is stored as 1

   ----------------------------------------------------------------------------
   procedure Set (Motor     : in Layout.Turnout_ID;
                  Direction : in Layout.Turn_Choice);
   -- Set the direction of the motor for the given turnout ID
   --
   -- Preconditions  : None
   --
   -- Postconditions : Motor is set to the given direction


   ----------------------------------------------------------------------------
   function In_Position (Motor : in Layout.Turnout_ID) return Boolean;
   -- Checks the status of the turnout
   --
   -- Preconditions  : None
   --
   -- Postconditions : Returns True if the motor has reached the position
   --                  requested in the last call to Set

end Motors;
