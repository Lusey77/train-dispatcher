
package body FIFO_Queue is

   procedure Enqueue (Queue : in out Queue_Type;
                      Item  : in     Element_Type) is
   begin
      if Queue.Count = Queue.Max_Size then
         raise Overflow;
      else
         Queue.Rear := Queue.Rear rem Queue.Max_Size + 1;
         Queue.Items (Queue.Rear) := Item;
         Queue.Count := Queue.Count + 1;
      end if;
   end Enqueue;

   procedure Dequeue (Queue : in out Queue_Type;
                      Item  : out    Element_Type) is
   begin
      if Queue.Count = 0 then
         raise Underflow;
      else
         Item := Queue.Items (Queue.Front);
         Queue.Front := Queue.Front rem Queue.Max_Size + 1;
         Queue.Count := Queue.Count - 1;
      end if;
   end Dequeue;

   function Full (Queue : in Queue_Type) return Boolean is
   begin
      return Queue.Count = Queue.Max_Size;
   end Full;

   function Empty (Queue : in Queue_Type) return Boolean is
   begin
      return Queue.Count = 0;
   end Empty;

   procedure Clear (Queue : in out Queue_Type) is
   begin
      Queue.Count := 0;
      Queue.Front := 1;
      Queue.Rear  := Queue.Max_Size;
   end Clear;

end FIFO_Queue;
