with Dallee;  -- Low level driver for the Dallee sound units
package Sound is

   -- This package provides Bell and Horn operations
   --
   -- Dallee hardware provides three types of sounds:
   --
   --    Diesel horn
   --    Bell
   --    Prime Mover - eight notches (levels) of diesel engine sounds
   --
   -- This package includes only horn and bell operations.  The primer mover
   -- sounds are hardwired directly to the output of the cabs so that the
   -- engine sounds are proportional to the cab voltages.
   -- Toggle switches on the sound panel can disable the prime mover sounds.
   -- A disabled prime mover is set to the lowest notch regardless
   -- of cab voltage.

   -- The sound board can handle up to four Dallee sound units
   -- Only three sound units are currently installed
   subtype Installed_Range is Dallee.Dallee_Num range 1 .. 3;

   type Horn_Signal is (Stop, Approach_Station, Warning, Start,
                        Approach_Highway, Approach_Crossing, Livestock);

   ----------------------------------------------------------------------------
   procedure Sound_Horn (Unit   : in Installed_Range;
                         Signal : in Horn_Signal);
   -- Request that Signal be sounded on the given Unit
   --
   -- Preconditions  : none
   --
   -- Postconditions : if Unit is not active from a previous request
   --                     it will sound the given Signal.
   --                  if Unit is active from a previous request
   --                     the current request will be ignored

   ----------------------------------------------------------------------------
   procedure Bell_On (Unit : in Installed_Range);
   -- Turn on the Unit's bell
   --    When enabled, the bell will begin ringing when the prime
   --    mover sound is at notch 4 or below.  Once ringing begins,
   --    it is only stopped by disabling it with a call to Bell_Off.
   --
   -- Preconditions  : none
   --
   -- Postconditions : Enables the given Unit's bell.
   --

   ----------------------------------------------------------------------------
   procedure Bell_Off (Unit : in Installed_Range);
   -- Turn off the Unit's bell
   --
   -- Preconditions  : none
   --
   -- Postconditions : The given Unit's bell is disabled.
   --                  The bell will not sound.

end Sound;
