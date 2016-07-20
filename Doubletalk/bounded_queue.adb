--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Ethan Morisette ------------------------------------------------
-- Updated    : 12 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------

package body Bounded_Queue is

   -----------
   -- Clear --
   -----------

   procedure Clear (Queue : in out Queue_Type) is
   begin
      Queue.Count := 0;
      Queue.Front := 1;
      Queue.Rear  := Queue.Max_Size;
   end Clear;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue (Queue : in out Queue_Type;
                      Item  : in     Element_Type) is
   begin
      if Queue.Count = Queue.Max_Size then
         raise OVERFLOW;
      else
         Queue.Rear := Queue.Rear rem Queue.Max_Size + 1;
         Queue.Items (Queue.Rear) := Item;
         Queue.Count := Queue.Count + 1;
      end if;
   end Enqueue;

   -------------
   -- Dequeue --
   -------------

   procedure Dequeue (Queue : in out Queue_Type;
                      Item  :    out Element_Type) is
   begin
      if Queue.Count = 0 then
         raise UNDERFLOW;
      else
         Item := Queue.Items (Queue.Front);
         Queue.Front := Queue.Front rem Queue.Max_Size + 1;
         Queue.Count := Queue.Count - 1;
      end if;
   end Dequeue;

   ----------
   -- Full --
   ----------

   function Full (Queue : in Queue_Type) return Boolean is
   begin
      return Queue.Count = Queue.Max_Size;
   end Full;

   -----------
   -- Empty --
   -----------

   function Empty (Queue : in Queue_Type) return Boolean is
   begin
      return Queue.Count = 0;
   end Empty;

end Bounded_Queue;
