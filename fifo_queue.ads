package FIFO_Queue is

   subtype Element_Type is Integer;

   type Queue_Array is array (Positive range <>) of Element_Type;
   type Queue_Type (Max_Size : Positive) is
      record
         Count : Natural  := 0;
         Front : Positive := 1;
         Rear  : Positive := Max_Size;
         Items : Queue_Array (1 .. Max_Size);
      end record;

   Overflow  : exception;
   Underflow : exception;

   procedure Clear (Queue : in out Queue_Type);

   procedure Enqueue (Queue : in out Queue_Type;
                      Item  : in     Element_Type);
   -- Overflow is raised on attempt to enqueue an element onto a full queue

   procedure Dequeue (Queue : in out Queue_Type;
                      Item  :    out Element_Type);
   -- Underflow is raised on attempt to dequeue an element onto a empty queue

   function Full (Queue : in Queue_Type) return Boolean;

   function Empty (Queue : in Queue_Type) return Boolean;

end FIFO_Queue;
