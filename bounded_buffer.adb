package body Bounded_Buffers is
   protected body Bounded_Buffer is

      procedure Clear is
      begin
         FIFO_Queue.Clear (Buffer);
      end Clear;

      procedure Put (Item : in Positive) is
      begin
         if FIFO_Queue.Full (Buffer) then
            FIFO_Queue.Dequeue (Queue => Buffer,
                                Item  => Value);
         end if;

         FIFO_Queue.Enqueue (Queue => Buffer,
                             Item  => Item);
      end Put;

      entry Take (Item : out Positive)
        when not FIFO_Queue.Empty (Buffer) is
      begin
         FIFO_Queue.Dequeue (Queue => Buffer,
                             Item  => Item);
      end Take;

   end Bounded_Buffer;
end Bounded_Buffers;



