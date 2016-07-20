--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Ethan Morisette ------------------------------------------------
-- Updated    : 12 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------

generic
   type Element_Type is private;
package Bounded_Queue is

   type Queue_Type (Max_Size : Positive) is tagged limited private;

   OVERFLOW  : exception;
   UNDERFLOW : exception;

   ----------------------------------------------------------------------------
   procedure Clear (Queue : in out Queue_Type);

   procedure Enqueue (Queue : in out Queue_Type;
                      Item  : in     Element_Type);

   procedure Dequeue (Queue : in out Queue_Type;
                      Item  :    out Element_Type);

   function Full (Queue : in Queue_Type) return Boolean;

   function Empty (Queue : in Queue_Type) return Boolean;

private

   type Queue_Array is array (Positive range <>) of Element_Type;
   type Queue_Type (Max_Size : Positive) is tagged limited
      record
         Count : Natural := 0;
         Front : Positive := 1;
         Rear  : Positive := Max_Size;
         Items : Queue_Array (1 .. Max_Size);
      end record;

end Bounded_Queue;
