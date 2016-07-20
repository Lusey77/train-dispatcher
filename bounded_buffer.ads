with FIFO_Queue;

package Bounded_Buffers is

   Value : Positive;

   protected type Bounded_Buffer (Max_Size : Positive) is
      procedure Clear;
      -- delete all of the items in the buffer
      procedure Put (Item : in Positive);
      -- add a value to the buffer
      entry Take (Item : out Positive);
      -- remove a value from the buffer
   private
      Buffer : FIFO_Queue.Queue_Type (Max_Size);
   end Bounded_Buffer;
end Bounded_Buffers;

