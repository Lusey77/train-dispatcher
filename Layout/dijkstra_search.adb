with Layout;
with Ada.Text_IO;
with Blocks;
procedure Dijkstra_Search is

   type Search_Rec (Splits : Boolean) is
      record
         Previous_Node : Positive;
         Block_Info    : Block_Info_Rec;
         case Splits is
            when TRUE =>
               Left_Node  : Positive;
               Right_Node : Positive;
            when FALSE =>
               Next_Node  : Positive;
         end case;
      end record;










































   type Search_Rec is
      record
         Distance      : Integer;
         Previous_Node : Layout.Block_ID;
      end record;

   type Block_Array is array (Layout.Block_ID) of Boolean;
   type Distance_Array is array (Layout.Block_ID) of Search_Rec;

   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);


   Search_Tree : Distance_Array;

   procedure Check_Distance (Current_Node : in Layout.Block_ID;
                             Block        : in Layout.Block_ID) is



   begin
      if Search_Tree (Current_Node).Distance + Blocks.Length (Block) <
        Search_Tree (Block).Distance then
         Search_Tree (Block).Distance := Search_Tree (Current_Node).Distance +
         Blocks.Length (Block);
         Search_Tree (Block).Previous_Node := Current_Node;
      end if;
   end Check_Distance;

   Visited_Blocks   : Block_Array;
   Unvisited_Blocks : Block_Array;
   Current_Node     : Layout.Block_ID;
   Starting_Node    : Layout.Block_ID;
   Ending_Node      : Layout.Block_ID;
   Block            : Layout.Block_ID;
   Norm_Term        : Layout.Terminator_Type;
   Rev_Term         : Layout.Terminator_Type;


begin


   -- Initialize the search tree
   for Index in Layout.Block_ID'Range loop
      Search_Tree (Index).Distance := 100_000_000;
      Unvisited_Blocks (Index) := True;
      Visited_Blocks (Index) := False;
   end loop;


   Ada.Text_IO.Put_Line ("Please enter the block you wish to start at");
   Block_IO.Get (Starting_Node);
   Ada.Text_IO.Put_Line ("Please enter the block you wish to end at");
   Block_IO.Get (Ending_Node);


   -- Get starting node and ending node
   Search_Tree (Starting_Node).Distance := 0;
   Unvisited_Blocks (Starting_Node) := False;
   Visited_Blocks (Starting_Node) := True;
   Current_Node := Starting_Node;


   loop
      Norm_Term := Layout.Next_Term (Block => Current_Node,
                                     Direction   => Layout.Normal);
      Rev_Term  := Layout.Next_Term (Block => Current_Node,
                                     Direction   => Layout.Reverze);
      case Norm_Term is
         when Layout.Block =>
            Block := Layout.End_Block (Block     => Current_Node,
                                       Direction => Layout.Normal);
            Check_Distance (Current_Node, Block);
            Block := Layout.End_Block (Block     => Current_Node,
                                       Direction => Layout.Reverze);
            Check_Distance (Current_Node, Block);
         when Layout.Turnout =>
            null;
         when Layout.Dead_End =>
            null;
      end case;
   end loop;



end Dijkstra_Search;
