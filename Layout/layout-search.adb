--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested  By : Travis Sullivan ------------------------------------------------
-- Updated    : 17 Feb 15 ------------------------------------------------------
--------------------------------------------------------------------------------
package body Layout.Search is

   procedure Blocks_Beneath (Loco     : in     Block_ID;
                             Caboose  : in     Block_ID;
                             Blocks   :    out Block_List;
                             Turnouts :    out Turnout_List;
                             Success  :    out Boolean) is

      -- Represents the next terminatorat the end of a block
      Next : Terminator_Type;

      --------------------------------------------------------------------------
      -- Appends a block to the block list
      procedure Append_Block (Block     : in Block_ID;
                              Direction : in Block_Polarity) is

      begin
         Blocks.Size := Blocks.Size + 1;
         Blocks.Items (Blocks.Size) := (Block, Direction);
      end Append_Block;
      --------------------------------------------------------------------------
      -- Removes a block from the block list
      procedure Remove_Block is

      begin
         Blocks.Size := Blocks.Size - 1;
      end Remove_Block;
      --------------------------------------------------------------------------
      -- Appends a turnout to the turnout list
      procedure Append_Turnout (Turnout   : in Turnout_ID;
                                Direction : in Turn_Choice) is

      begin
         Turnouts.Size := Turnouts.Size + 1;
         Turnouts.Items (Turnouts.Size) := (Turnout, Direction);
      end Append_Turnout;
      --------------------------------------------------------------------------
      -- Removes a turnout from the turnout list
      procedure Remove_Turnout is

      begin
         Turnouts.Size := Turnouts.Size - 1;
      end Remove_Turnout;
      --------------------------------------------------------------------------
      -- Determines the direction of the next block to be added to the list
      function Direction return Block_Polarity is

      begin

         if Is_Reverse (Sensor_Number (Blocks.Items (Blocks.Size).Block,
           Caboose)) then
            return Opposite (Blocks.Items (Blocks.Size).Direction);
         else
            return Blocks.Items (Blocks.Size).Direction;
         end if;
      end Direction;
      --------------------------------------------------------------------------

   begin
      --------------------------------------------------------------------------
      -- Determine if this is the first iteration or if its the first time    --
      -- after reversing the direction                                        --
      --------------------------------------------------------------------------
      if Blocks.Size = 0 then
         -----------------------------------------------------------------------
         -- If first time then append the caboose and normal direction        --
         -----------------------------------------------------------------------
         Append_Block (Caboose, Normal);
      elsif Caboose = Blocks.Items (1).Block then
         -----------------------------------------------------------------------
         -- Elsif first time after reversing the direction don't append to    --
         -- the list just do nothing                                          --
         -----------------------------------------------------------------------
         null;
      else
         -----------------------------------------------------------------------
         -- Else append the block                                             --
         -----------------------------------------------------------------------
         Append_Block (Caboose, Direction);
      end if;

      --------------------------------------------------------------------------
      -- Determine if we are at the base case                                 --
      --------------------------------------------------------------------------
      if Caboose = Loco or Blocks.Size >= Blocks.Max_Size then
         -----------------------------------------------------------------------
         -- If we are at the base case then change success to reflect results --
         -----------------------------------------------------------------------
         Success := Caboose = Loco;
      else
         -----------------------------------------------------------------------
         -- Else continue on with search                                      --
         -----------------------------------------------------------------------

         -----------------------------------------------------------------------
         -- Get the next terminator                                           --
         -----------------------------------------------------------------------
         Next := Next_Term (Block     => Caboose,
                            Direction => Blocks.Items (Blocks.Size).Direction);

         -----------------------------------------------------------------------
         -- Based on what the terminator is the search will execute a         --
         -- different branch                                                  --
         -----------------------------------------------------------------------
         case Next is
            when Block =>
               -- Search the block at the end of the block
               Blocks_Beneath (Loco      => Loco,
                               Caboose   => End_Block (Caboose, Blocks.Items
                                  (Blocks.Size).Direction),
                               Blocks    => Blocks,
                               Turnouts  => Turnouts,
                               Success   => Success);
               -----------------------------------------------------------------
               -- If not successful then remove the block from the list       --
               -----------------------------------------------------------------
               if not Success then
                  Remove_Block;
               end if;
            when Turnout =>
               -----------------------------------------------------------------
               -- Determine what kind of turnout we have                      --
               -----------------------------------------------------------------
               if Is_Force_Turnout (Caboose, Blocks.Items
                  (Blocks.Size).Direction) then
                  --------------------------------------------------------------
                  -- If F_Turnout then append to the list the turnout and its --
                  -- corresponding direction                                  --
                  --------------------------------------------------------------
                  -- Append the turnout to the turnout list
                  Turnouts.Size := Turnouts.Size + 1;
                  Turnouts.Items (Turnouts.Size) := (Force_Turnout
                     (Caboose, Blocks.Items (Blocks.Size).Direction));
                  -- Search the block at the end of the turnout
                  Blocks_Beneath (Loco      => Loco,
                                  Caboose   => End_Turnout (Turnouts.Items
                                     (Turnouts.Size).Turnout, Common),
                                  Blocks    => Blocks,
                                  Turnouts  => Turnouts,
                                  Success   => Success);
                  --------------------------------------------------------------
                  -- If not successful then remove the block and turnout from --
                  -- the list                                                 --
                  --------------------------------------------------------------
                  if not Success then
                     Remove_Block;
                     Remove_Turnout;
                  end if;
               else
                  --------------------------------------------------------------
                  -- Else append the turnout to the list with a fixed left    --
                  -- direction to search the left                             --
                  --------------------------------------------------------------
                  Append_Turnout (End_Block (Caboose, Blocks.Items
                     (Blocks.Size).Direction), Left);
                  --------------------------------------------------------------
                  -- If the turnout is a joint turnout then add the correct   --
                  -- turnout and direction that corresponds with it           --
                  --------------------------------------------------------------
                  if Is_Joint_Turnout (Turnouts.Items
                     (Turnouts.Size).Turnout, Left) then
                     -----------------------------------------------------------
                     -- If the turnout is a joint turnout then add the correc --
                     -- turnout and direction that corresponds with it        --
                     -----------------------------------------------------------
                     Append_Turnout (Joint_Turnout (Turnouts.Items
                        (Turnouts.Size).Turnout), Left);
                     Blocks_Beneath (Loco      => Loco,
                                     Caboose   => End_Turnout (Turnouts.Items
                                        (Turnouts.Size - 1).Turnout, Left),
                                     Blocks    => Blocks,
                                     Turnouts  => Turnouts,
                                     Success   => Success);
                  else
                     -----------------------------------------------------------
                     -- Else continue on with the search of the left limb     --
                     -----------------------------------------------------------
                     Blocks_Beneath (Loco      => Loco,
                                     Caboose   => End_Turnout (Turnouts.Items
                                        (Turnouts.Size).Turnout, Left),
                                     Blocks    => Blocks,
                                     Turnouts  => Turnouts,
                                     Success   => Success);
                  end if;

                  --------------------------------------------------------------
                  -- If not successful then remove block and search the right --
                  -- limb                                                     --
                  --------------------------------------------------------------
                  if not Success then
                     Remove_Block;
                     -----------------------------------------------------------
                     -- If the turnout was a joint turnout on its left end    --
                     -- remove the turnout from the list                      --
                     -----------------------------------------------------------
                     if Is_Joint_Turnout (Turnouts.Items
                        (Turnouts.Size).Turnout, Left) then
                        Remove_Turnout;
                     end if;
                     -- Change the turnout direction
                     Turnouts.Items (Turnouts.Size).Direction := Right;
                     -----------------------------------------------------------
                     -- If the turnout is a joint turnout then add the correc --
                     -- turnout and direction that corresponds with it        --
                     -----------------------------------------------------------
                     if Is_Joint_Turnout (Turnouts.Items
                        (Turnouts.Size).Turnout, Right) then
                        --------------------------------------------------------
                        -- If the turnout is a joint turnout then add the     --
                        -- correct turnout and direction that corresponds     --
                        -- with it                                            --
                        --------------------------------------------------------
                        Append_Turnout (Joint_Turnout (Turnouts.Items
                           (Turnouts.Size).Turnout), Right);
                        Blocks_Beneath (Loco      => Loco,
                                        Caboose   => End_Turnout (Turnouts.Items
                                           (Turnouts.Size - 1).Turnout, Right),
                                        Blocks    => Blocks,
                                        Turnouts  => Turnouts,
                                        Success   => Success);
                     else
                        --------------------------------------------------------
                        -- Else continue on with the search of the right limb --
                        --------------------------------------------------------
                        Blocks_Beneath (Loco      => Loco,
                                        Caboose   => End_Turnout (Turnouts.Items
                                           (Turnouts.Size).Turnout, Right),
                                        Blocks    => Blocks,
                                        Turnouts  => Turnouts,
                                        Success   => Success);
                     end if;

                     -----------------------------------------------------------
                     -- If not successful remove the block and turnout        --
                     -----------------------------------------------------------
                     if not Success then
                        Remove_Block;
                        --------------------------------------------------------
                        -- If the turnout had a joint tunrout on its right    --
                        -- limb then remove the turnout                       --
                        --------------------------------------------------------
                        if Is_Joint_Turnout (Turnouts.Items
                           (Turnouts.Size).Turnout, Right) then
                           Remove_Turnout;
                        end if;
                        Remove_Turnout;
                     end if;
                  end if;
               end if;
            when Dead_End =>
               -- If dead end that part of the search is over
               Success := False;
         end case;
      end if;
      --------------------------------------------------------------------------
      -- If we have completed the search of the normal end of the caboose     --
      -- then we need to switch directions                                    --
      --------------------------------------------------------------------------
      if ((Caboose = Blocks.Items (1).Block) and (not Success))
         and (Blocks.Items (1).Direction = Normal) then
         Remove_Block;
         Append_Block (Caboose, Reverze);
         Blocks_Beneath (Loco      => Loco,
                         Caboose   => Caboose,
                         Blocks    => Blocks,
                         Turnouts  => Turnouts,
                         Success   => Success);
         -- If not successful then remove the block
         if not Success then
            Remove_Block;
         end if;
      end if;
   end Blocks_Beneath;
end Layout.Search;



