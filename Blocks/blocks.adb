--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested  By : Travis Sullivan ------------------------------------------------
-- Updated    : 20 Apr 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Port_IO;
with Trains;
with Layout;
with System;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Cabs;
with Console_Management;
use  Console_Management; -- Allow colors to be used without prefixing
use type Trains.Request_ID;
use type Layout.Block_ID;
use type Port_IO.Address_Range;
use type Cabs.Cab_ID;
package body Blocks is

   -- Represents the array used to store the block lengths
   type Block_Length_Array is array (Layout.Block_ID) of Natural;

   -- Represents the record that will hold information about a block
   type Block_Data is
      record
         Times_Reserved : Natural;
         Requestor      : Trains.Request_ID;
      end record;

   -- Represents the array used to make block reservations
   type Block_Reservation is array (Layout.Block_ID) of Block_Data;

   Block_Lengths : Block_Length_Array;

   -- Provides protected operations for reserving and freeing blocks
   protected Block_Operations is
      -- Frees the given block
      procedure Free_Block (Block     : in Layout.Block_ID;
                            Requestor : in Trains.Request_ID);
      -- Reserves the given block
      procedure Reserve_Block (Block     : in  Layout.Block_ID;
                               Requestor : in  Trains.Request_ID;
                               Success   : out Boolean);
      -- Prints the reserved blocks for debugging
      procedure Print_Blocks;
   private
      -- Represents the variable that holds all of layouts blocks
      Reservations : Block_Reservation := (others => (Times_Reserved => 0,
                                                      Requestor      => 0));
   end Block_Operations;

   protected body Block_Operations is
      procedure Reserve_Block (Block     : in  Layout.Block_ID;
                               Requestor : in  Trains.Request_ID;
                               Success   : out Boolean) is
         -- Represents a block list for storing any cross blocks
         Crossing      : Layout.Block_List (Max_Size => 2);
         -- Represents the crossed block
         Crossed_Block : Layout.Block_ID;
      begin
         if Reservations (Block).Times_Reserved = 0 then
            -- If the block hasn't been reserved yet reserve it
            if Layout.Is_Cross_Block (Block => Block) then
               -- If the block is a cross block reserve the cross block too
               Crossing := Layout.Cross_Block (Block);
               -- Determine which block is the crossed block
               if Crossing.Items (1).Block = Block then
                  Crossed_Block := Crossing.Items (2).Block;
               else
                  Crossed_Block := Crossing.Items (1).Block;
               end if;
               -- Reserve the blocks
               Reservations (Block) := (1, Requestor);
               Reservations (Crossed_Block) := (1, Requestor);
            else
               -- Reserve the block
               Reservations (Block) := (1, Requestor);
            end if;
            Success := True;
         elsif Reservations (Block).Requestor = Requestor then
            -- elsif the block is already reserved by the current requestor
            -- add to the number of times requested
            if Layout.Is_Cross_Block (Block => Block) then
               -- If the block is a cross block then add the cross block
               Crossing := Layout.Cross_Block (Block);
               -- Determine which block is the crossed block
               if Crossing.Items (1).Block = Block then
                  Crossed_Block := Crossing.Items (2).Block;
               else
                  Crossed_Block := Crossing.Items (1).Block;
               end if;
               -- Add to the number of times reserved
               Reservations (Block).Times_Reserved :=
                 Reservations (Block).Times_Reserved + 1;
               Reservations (Crossed_Block).Times_Reserved :=
                 Reservations (Crossed_Block).Times_Reserved + 1;
            else
               -- Add to the number of times reserved
               Reservations (Block).Times_Reserved :=
                 Reservations (Block).Times_Reserved + 1;
            end if;
            Success := True;
         else
            -- else the reserve was unsuccessful
            Success := False;
         end if;
-- Test Utility
--           Console_Management.Clear_Screen;
--           Print_Blocks;
      end Reserve_Block;

      procedure Free_Block (Block     : in     Layout.Block_ID;
                            Requestor : in     Trains.Request_ID) is

         -- Represents a block list for storing any cross blocks
         Crossing      : Layout.Block_List (Max_Size => 2);
         -- Represents the crossed block
         Crossed_Block : Layout.Block_ID;

      begin
         if Requestor = 0 then
            Reservations (Block) := (0, 0);
            if Layout.Is_Cross_Block (Block => Block) then
               -- If the block is a cross block subtract the number of times
               -- the cross block is reserved by one as well
               Crossing := Layout.Cross_Block (Block);
               if Crossing.Items (1).Block = Block then
                  Crossed_Block := Crossing.Items (2).Block;
               else
                  Crossed_Block := Crossing.Items (1).Block;
               end if;
               Reservations (Crossed_Block) := (0, 0);
            end if;
         elsif Reservations (Block).Requestor = Requestor then
            -- If the dispatcher requests to free a block or the a train
            -- requests for free its block free the block
            if Reservations (Block).Times_Reserved /= 0 then
               -- If the block has been reserved multiple times subtract the
               -- times reserved by one
               Reservations (Block).Times_Reserved :=
                    Reservations (Block).Times_Reserved - 1;
               if Layout.Is_Cross_Block (Block => Block) then
                  -- If the block is a cross block subtract the number of times
                  -- the cross block is reserved by one as well
                  Crossing := Layout.Cross_Block (Block);
                  if Crossing.Items (1).Block = Block then
                     Crossed_Block := Crossing.Items (2).Block;
                  else
                     Crossed_Block := Crossing.Items (1).Block;
                  end if;
                  Reservations (Crossed_Block).Times_Reserved :=
                    Reservations (Crossed_Block).Times_Reserved - 1;
                  if Reservations (Crossed_Block).Times_Reserved = 0 then
                     -- If the crossed block has been completely freed give it
                     -- back to the dispatcher
                     Reservations (Crossed_Block) := (0, 0);
                  end if;
               end if;
            end if;
            if Reservations (Block).Times_Reserved = 0 then
               -- If the block has been completely freed give it back to the
               -- dispatcher
               Reservations (Block) := (0, 0);
            end if;
-- Test Utility
--              Console_Management.Clear_Screen;
--              Print_Blocks;
         end if;
      end Free_Block;

      procedure Print_Blocks is
      begin
         --Debug screen
         -----------------------------------------------------------------------
         -- Runs the systems                                                  --
         -----------------------------------------------------------------------
         -- Each iteration, goes through block information                    --
         -----------------------------------------------------------------------
         for Block in Layout.Block_ID loop
            -- Changes color based on who has it reserved
            if (Reservations (Block).Requestor) = 1 then
               Console_Management.Set_Text_Color (LightGreen);
            elsif (Reservations (Block).Requestor) = 2 then
               Console_Management.Set_Text_Color (LightBlue);
            elsif (Reservations (Block).Requestor) = 3 then
               Console_Management.Set_Text_Color (Red);
            end if;
            -- Outputs the block
            Ada.Text_IO.Put ("  Block: ");
            Ada.Integer_Text_IO.Put (Integer (Block), 1);
            -- Who requested it (0 if not)
            Ada.Text_IO.Put ("  Requestor: ");
            Ada.Integer_Text_IO.Put
              (Integer (Reservations (Block).Requestor), 1);
            -- How many times (0 if not)
            Ada.Text_IO.Put ("  Times: ");
            Ada.Integer_Text_IO.Put (Reservations (Block).Times_Reserved, 1);
            if Block rem 2 = 0 then
               Ada.Text_IO.New_Line;
            else
               Ada.Text_IO.Set_Col (40);
            end if;
            Console_Management.Set_Text_Color (White);
         end loop;
      end Print_Blocks;
   end Block_Operations;

   -- Type that represnts the data at a port address
   type Port_Rec is
      record
         Even_Cab_Num   : Cabs.Cab_ID;
         Even_Direction : Layout.Block_Polarity;
         Odd_Cab_Num    : Cabs.Cab_ID;
         Odd_Direction  : Layout.Block_Polarity;
      end record;

   -- Represents how in memory to store the port record
   for Port_Rec use
      record
         Even_Cab_Num   at 0 range 0 .. 2;
         Even_Direction at 0 range 3 .. 3;
         Odd_Cab_Num    at 0 range 4 .. 6;
         Odd_Direction  at 0 range 7 .. 7;
      end record;

   -- Represents how in memory the port rec should be stored
   for Port_Rec'Size use 8;
   for Port_Rec'Bit_Order use System.Low_Order_First;

   -- Represents funtions to convert bytes into records and vice versa
   function To_Port_Rec is new Ada.Unchecked_Conversion
      (Source => Port_IO.Byte, Target => Port_Rec);
   function To_Byte is new Ada.Unchecked_Conversion
      (Source => Port_Rec, Target => Port_IO.Byte);

   -- Represents the base address of the ports
   Base : constant Port_IO.Address_Range := 16#200#;

   -----------------
   -- Power_Block --
   -----------------

   procedure Power_Block (Block     : in Layout.Block_ID;
                          Direction : in Layout.Block_Polarity;
                          Cab       : in Cabs.Cab_ID) is

      -- Represents the base address for a given block
      Board_Base : constant Port_IO.Address_Range :=
               Base + Port_IO.Address_Range ((Block - 1) / 12 * 8);
      -- Represents the position
      Base_Pos     : constant Port_IO.Address_Range :=
                       Port_IO.Address_Range ((Block - 1) rem 12) / 2;
      -- Represents the address of the port
      Port_Address : Port_IO.Address_Range;
      -- Represents the data to be written to the port
      Port_Data    : Port_Rec;

   begin
      if Base_Pos >= 3 then
         -- If the position of the base is greater than 3 add one to the base
         -- to skip the input output base
         Port_Address := Board_Base + Base_Pos + 1;
      else
         -- else add the base position to the board base
         Port_Address := Board_Base + Base_Pos;
      end if;

      -- Read the value from the register
      Port_Data := To_Port_Rec (Port_IO.In_Byte (Address => Port_Address));

      -- Make the changes to the port rec
      if (Block rem 2) = 0 then
         -- If the block to be changed is even change the even variables
         Port_Data.Even_Cab_Num := Cab;
         Port_Data.Even_Direction := Direction;
      else
         -- else change the odd variables
         Port_Data.Odd_Cab_Num := Cab;
         Port_Data.Odd_Direction := Direction;
      end if;

      -- Write out the new data
      Port_IO.Out_Byte (Address => Port_Address,
                        Data    => To_Byte (Port_Data));
   end Power_Block;

   ----------------
   -- Is_Powered --
   ----------------

   function Is_Powered (Block : in Layout.Block_ID) return Boolean is

      -- Represents the base address for a given block
      Board_Base : constant Port_IO.Address_Range :=
               Base + Port_IO.Address_Range ((Block - 1) / 12 * 8);
      -- Represents the position
      Base_Pos     : constant Port_IO.Address_Range :=
                       Port_IO.Address_Range ((Block - 1) rem 12) / 2;
      -- Represents the address of the port
      Port_Address : Port_IO.Address_Range;
      -- Represents the data to be written to the port
      Port_Data    : Port_Rec;

   begin
      if Base_Pos >= 3 then
         -- If the position of the base is greater than 3 add one to the base
         -- to skip the input output base
         Port_Address := Board_Base + Base_Pos + 1;
      else
         -- else add the base position to the board base
         Port_Address := Board_Base + Base_Pos;
      end if;

      -- Read the value from the register
      Port_Data := To_Port_Rec (Port_IO.In_Byte (Address => Port_Address));

      -- Return whether the block is powered or not
      if Block rem 2 = 0 then
         return not (Port_Data.Even_Cab_Num = 0 or Port_Data.Even_Cab_Num = 7);
      else
         return not (Port_Data.Odd_Cab_Num = 0 or Port_Data.Odd_Cab_Num = 7);
      end if;
   end Is_Powered;

   procedure Reserve (Block     : in  Layout.Block_ID;
                      Requestor : in  Trains.Request_ID;
                      Success   : out Boolean) is
   begin
      Block_Operations.Reserve_Block (Block      => Block,
                                      Requestor => Requestor,
                                      Success   => Success);
   end Reserve;

   procedure Free (Block     : in     Layout.Block_ID;
                   Requestor : in     Trains.Request_ID) is
   begin
      Block_Operations.Free_Block (Block     => Block,
                                   Requestor => Requestor);
   end Free;

   procedure Length (Block : in Layout.Block_ID) is

   begin

      return Block_Lengths (Block);

   end Length;

   procedure Print_Blocks is
   begin
      Block_Operations.Print_Blocks;
   end Print_Blocks;

begin
   -- Initiate the registers
   for Count in 1 .. 8 loop
      -- Write to the proper I/O register
      Port_IO.Out_Byte (Address => Base + Port_IO.Address_Range (
                        (Count * 4) - 1), -- I/O registers occur every 4 bytes
                         Data    => 128); -- 128 sets them all to output
   end loop;
   -- Power all blocks to null and the normal direction
   for Block in Layout.Block_ID loop
      Power_Block (Block     => Block,
                   Direction => Layout.Normal,
                   Cab       => 0);
   end loop;
end Blocks;
