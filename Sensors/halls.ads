with Layout;
package Halls is

-- This package provides the Hall objects that sense the
-- position of the trains.  These active objects are implemented as
-- tasks that send information to the trains when a Hall sensor is
-- triggered. The implementation uses interrupts with IRQ 5 and IRQ 7.

   -- Written by John McCormick, March 2002, Modified April 2008

   subtype Hall_ID is Layout.Hall_ID;

   ----------------------------------------------------------------------------
   procedure Initialize;
   -- Set up the Hall sensor interface electronics.
   --
   -- May be called anytime you wish to reset the Hall sensor electronics to
   -- a known, valid state.
   --
   -- Preconditions   : none
   --
   -- Postconditions  : The Hall sensor interface electronics are initialized.
   --                   Hall sensor interrupts are disabled.


   ----------------------------------------------------------------------------
   type Callback_Ptr is access procedure (Hall : in Hall_ID);

   procedure Enable (Callback : in not null Callback_Ptr);
   -- Enable Hall sensor interrupts.
   --
   -- Preconditions : Initialize was called sometime previously
   --
   -- When a Hall interrupt occurs, the procedure designated by Callback is
   -- called with the ID number of the Hall sensor that was triggered.
   --
   -- Warnings : Multiple Hall tasks in this package may call procedure Callback
   --            concurrently.
   --            Blocking in procedure Callback will delay the processing of the
   --            next interrupt.  Do not do any unnecessary operations in
   --            procedure Callback.

   ----------------------------------------------------------------------------
   procedure Disable;
   -- Disable Hall sensor interrupts
   --
   -- Preconditions : Initialize was called sometime previously
   --
   -- When disabled, the triggering of a Hall sensor will be ignored
   --
   -- Note: The Initialize procedure disables Hall sensor interrupts

   ----------------------------------------------------------------------------
   function Is_Triggered (Hall : in Hall_ID) return Boolean;
   -- Returns True if the given Hall sensor currently has a magnet
   -- over it.  Useful for the rare case when a train stops with a magnet
   -- directly over a Hall sensor and then changes its direction.
   --
   -- Preconditions : Initialize was called sometime previously
   --                 Interrupts need not be enabled for this function
   --                 to work correctly

end Halls;
