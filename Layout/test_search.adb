--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Travis Sullivan ------------------------------------------------
-- Tests      : Layout.Search --------------------------------------------------
-- Updated    : 17 Feb 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Layout;
with Layout.Search;

procedure Test_Search is

   -- Instantiate a package for IO with blocks
   package Block_IO      is new Ada.Text_IO.Integer_IO
     (Num => Layout.Block_ID);

   --------------------
   -- Test Variables --
   --------------------

   Loco     : Layout.Block_ID;
   Caboose  : Layout.Block_ID;
   Blocks   : Layout.Search.Block_List (Max_Size => 5);
   Turnouts : Layout.Search.Turnout_List (Max_Size => 5);
   Success  : Boolean;

begin

      -- Get the location of the locomotive and caboose
      Ada.Text_IO.Put ("Input the location of the locomotive: ");
      Block_IO.Get (Loco);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Input the location of the caboose: ");
      Block_IO.Get (Caboose);
      Ada.Text_IO.New_Line;

      -- Test the funtion
      Layout.Search.Blocks_Beneath (Loco     => Loco,
                                    Caboose  => Caboose,
                                    Blocks   => Blocks,
                                    Turnouts => Turnouts,
                                    Success  => Success);

      -- Output the results
      for Count in 1 .. Blocks.Size loop
         Ada.Integer_Text_IO.Put (Integer  (Blocks.Items (Count).Block), 1);
         Ada.Text_IO.Put_Line (" " & Layout.Block_Polarity'Image (Blocks.Items
        (Count).Direction));
      end loop;


      for Count in 1 .. Turnouts.Size loop
         Ada.Integer_Text_IO.Put (Integer (Turnouts.Items (Count).Turnout), 1);
         Ada.Text_IO.Put_Line (" " & Layout.Turn_Choice'Image (Turnouts.Items
        (Count).Direction));
      end loop;

      -- Output if successful or not
      if Success then
         Ada.Text_IO.Put_Line ("Successful");
      else
         Ada.Text_IO.Put_Line ("Failure");
      end if;
end Test_Search;
