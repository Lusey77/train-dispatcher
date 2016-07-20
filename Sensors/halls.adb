with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with System;
with MaRTE_Hardware_Interrupts;
with Port_IO;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;  -- for subtype int
with MaRTE.Direct_IO;  use MaRTE.Direct_IO;

package body Halls is

   -- Written by John McCormick, April 2002, modified April 2008

   use type Port_IO.Address_Range;
   use type Port_IO.Byte;
   use type Hall_ID;

   package MHI renames MaRTE_Hardware_Interrupts;

   ----------------------------------------------------------------------------
   -- The procedure to call to process a triggered Hall sensor
   ----------------------------------------------------------------------------

   Process : Callback_Ptr;

   ----------------------------------------------------------------------------
   -- Types for the Measurement Computing CIO-INT32 I/O Boards
   ----------------------------------------------------------------------------

   type Board_ID is (Board_One, Board_Two);  -- Two boards in the system
   type Chip_ID  is (Chip_One, Chip_Two);    -- Two Z8536 chips on each board
   -- We use three ports (A, B, and Control/Status)
   -- on each Z8536 chip.  We do not use port C.
   type    Port_ID   is (A, B, CS);
   subtype Data_Port is Port_ID range A .. B;

   -- I/O Addresses for the Z8536 chips' External Registers we use
   BASE1 : constant Port_IO.Address_Range := 16#250#; -- Board 1 base address
   BASE2 : constant Port_IO.Address_Range := 16#258#; -- Board 2 base address
   type Address_Array is array (Board_ID, Chip_ID, Port_ID)
                                                      of Port_IO.Address_Range;
   Address_Of : constant Address_Array :=
                (Board_One => (Chip_One => (A  => BASE1 + 2,
                                            B  => BASE1 + 1,
                                            CS => BASE1 + 3),
                               Chip_Two => (A  => BASE1 + 6,
                                            B  => BASE1 + 5,
                                            CS => BASE1 + 7)),
                 Board_Two => (Chip_One => (A  => BASE2 + 2,
                                            B  => BASE2 + 1,
                                            CS => BASE2 + 3),
                               Chip_Two => (A  => BASE2 + 6,
                                            B  => BASE2 + 5,
                                            CS => BASE2 + 7)));

   -- Board interrupt settings
   IRQ5 : constant MHI.Hardware_Interrupt := MHI.PARALLEL2_INTERRUPT;
   IRQ7 : constant MHI.Hardware_Interrupt := MHI.PARALLEL1_INTERRUPT;

   type IRQ_Array is array (Board_ID) of MHI.Hardware_Interrupt;
   IRQ : constant IRQ_Array := (Board_One => IRQ5,
                                Board_Two => IRQ7);


   ----------------------------------------------------------------------------
   -- The addresses of the 48 Z8536 internal registers are specified by 6 bits
   type Z8536_Address is new Port_IO.Byte range 2#000000# .. 2#111111#;

   -- The Z8536 internal registers are one byte each
   subtype Z8536_Data is Port_IO.Byte;

   -- An array of 8 bits packed into a byte.
   -- Used to check individual bits in a byte.
   type Bit_Number_Range is mod 8;
   type Bit_Array is array (Bit_Number_Range) of Boolean;
   for Bit_Array'Component_Size use 1;
   for Bit_Array'Size use 8;


   ----------------------------------------------------------------------------
   -- Each Hall sensor is connected to a bit of a data port
   -- on a Z8536 chip on a CIO-INT board.  The following record
   -- stores the connection information for one Hall sensor.
   ----------------------------------------------------------------------------
   type Connection_Rec is
      record
         Bit_Num : Bit_Number_Range;
         Port    : Data_Port;
         Chip    : Chip_ID;
         Board   : Board_ID;
      end record;

   -- Layout the record so that we can easily use an unchecked conversion to
   -- convert between a connection record and a Hall number
   for Connection_Rec use
      record
         Bit_Num at 0 range 0 .. 2;
         Port    at 0 range 3 .. 3;
         Chip    at 0 range 4 .. 4;
         Board   at 0 range 5 .. 7;
      end record;
   for Connection_Rec'Size use 8;


   ----------------------------------------------------------------------------
   -- The addresses of the Z8536 internal registers used in this package
   ----------------------------------------------------------------------------

   MIC  : constant Z8536_Address := 2#000000#;  -- Master Interrupt Control
   MCC  : constant Z8536_Address := 2#000001#;  -- Master Configuration Control

   PAMS : constant Z8536_Address := 2#100000#;  -- Port A Mode Specification
   PBMS : constant Z8536_Address := 2#101000#;  -- Port B Mode Specification

   PACS : constant Z8536_Address := 2#001000#;  -- Port A Command and Status
   PBCS : constant Z8536_Address := 2#001001#;  -- Port B Command and Status

   PADD : constant Z8536_Address := 2#100011#;  -- Port A Data Direction
   PBDD : constant Z8536_Address := 2#101011#;  -- Port B Data Direction

   PAPP : constant Z8536_Address := 2#100101#;  -- Port A Pattern Polarity
   PBPP : constant Z8536_Address := 2#101101#;  -- Port B Pattern Polarity

   PAPT : constant Z8536_Address := 2#100110#;  -- Port A Pattern Transistion
   PBPT : constant Z8536_Address := 2#101110#;  -- Port B Pattern Transistion

   PAPM : constant Z8536_Address := 2#100111#;  -- Port A Pattern Mask
   PBPM : constant Z8536_Address := 2#101111#;  -- Port B Pattern Mask


   -- Constant arrays of addresses are used to select the correct address

   -- Addresses of the two Command and Status Registers on each chip
   type Register_Address_Array is array (Data_Port) of Z8536_Address;
   Address_Of_CS_Reg : constant
                       Register_Address_Array := (A => PACS, B => PBCS);

   -- Addresses of the Pattern Specification Registers
   --   2 ports (A and B)
   --   3 registers per port
   --    (Pattern Mask, Pattern Transistion, Pattern Polarity)
   type Pattern_Register is (PM, PT, PP);
   type Pattern_Register_Array is array (Data_Port, Pattern_Register)
                               of Z8536_Address;
   Address_Of_Pat_Reg : constant Pattern_Register_Array :=
                         (A => (PM => PAPM, PT => PAPT, PP => PAPP),
                          B => (PM => PBPM, PT => PBPT, PP => PBPP));


   ----------------------------------------------------------------------------
   -- Data (commands) for the Z8536 internal registers used in this package
   ----------------------------------------------------------------------------

   -- Master Interrupt Control Register - MIC
      ---------------------------------------

   Reset          : constant Z8536_Data := 2#00000001#;
   Clear_Reset    : constant Z8536_Data := 2#00000000#;
      -- Bits set are
      --    RESET  - Reset the Z8536 chip          (bit 0)

   Master_Disable : constant Z8536_Data := 2#00111010#; -- Run w/o interrupts
   Master_Enable  : constant Z8536_Data := 2#10111010#; -- Run with interrupts
      -- Bits set are
      --    MIE    - Master Interrupt Enabled      (bit 7)
      --    NV     - No Vector output on interupt  (bit 5) see note below
      --    PA VIS - Port A Vector Includes Status (bit 4)
      --    PB VIS - Port B Vector Includes Status (bit 3)
      --    RJA    - Right Justified Address       (bit 1)
      -- Note
      -- The Intel hardware does not support the reading of interrupt
      -- vectors during the exchange of interrupt request and
      -- interrupt acknowledge signals.  Instead, this hardware uses
      -- interrupt levels (IRQs) to vector the interrupt to a handler.
      -- The vector that would have been output by the Z8536 chip can be
      -- obtained by reading an Interrupt Vector Register (PAIV or PBIV).


   -- Master Configuration Control Register - MCC
      -------------------------------------------

   Disable_A_B : constant Z8536_Data := 2#00000000#;
   Enable_A_B  : constant Z8536_Data := 2#10000100#;
      -- Bits set are
      --    PBE    - Port B Enable   (bit 7)
      --    PAE    - Port A Enable   (bit 2)


   -- Port Mode Specification Registers - PAMS & PBMS
      -----------------------------------------------

--     Disable_Match : constant Z8536_Data := 2#00000000#;
   OR_Mode       : constant Z8536_Data := 2#00000100#;
      -- Bits set are
      --    PMS1   - Pattern Mode Specification bit 1   (bit 2)
      --    PMS0   - Pattern Mode Specification bit 0   (bit 1)


   -- Port Command and Status Registers - PACS & PBCS
      -----------------------------------------------

   Set_IE       : constant Z8536_Data := 2#11000000#;
   Clear_IE     : constant Z8536_Data := 2#11100000#;
   Clear_IP_IUS : constant Z8536_Data := 2#00100000#;
--     Clear_IUS    : constant Z8536_Data := 2#01100000#;
--     Clear_IP     : constant Z8536_Data := 2#10100000#;


   -- Port Data Direction Registers - PADD & PBDD
      -------------------------------------------

   All_Input : constant Z8536_Data := 2#11111111#;


   -- Pattern Specifications involve setting 3 different registers for each port
   --    Pattern Polarity Registers    - PAPP & PBPP
   --    Pattern Transistion Registers - PAPT & PBPT
   --    Pattern Mask Registers        - PAPM & PBPM
      ----------------------------------------------

   Zeros : constant Z8536_Data := 2#00000000#;
   Ones  : constant Z8536_Data := 2#11111111#;

   -- Values of the three pattern registers for specifying matching
   type Pattern_Spec_Array is array (Pattern_Register) of Z8536_Data;
   -- Interrupt when bit is 1
--     Match_One            : constant Pattern_Spec_Array :=
--                                      (PM => Ones,  PT => Zeros, PP => Ones);
   -- Interrupt when bit makes any transition
   Match_Any_Transition : constant Pattern_Spec_Array :=
                                    (PM => Zeros,  PT => Ones, PP => Zeros);


   ----------------------------------------------------------------------------
   -- Buffers shared by interrupt handlers and Hall tasks
   ----------------------------------------------------------------------------

   type Interrupt_Rec is -- Information generated by one interrupt
      record
         Chip       : Chip_ID;     -- The chip that generated the interrupt
         Port       : Data_Port;   -- The port that generated the interrupt
         Port_Value : Z8536_Data;  -- The value of the port when
      end record;                  --   the interrupt was handled

   type Buffer_Index is mod 16;
   type Buffer_Array is array (Buffer_Index) of Interrupt_Rec;
   pragma Volatile_Components (Buffer_Array);

   type Buffer_Rec is  -- The buffer type
      record
         Back  : Buffer_Index := 0;
         Front : Buffer_Index := 0;
         Data  : Buffer_Array;
      end record;
   -- An array of buffers, one buffer for each board
   type Interrupt_Buffers is array (Board_ID) of Buffer_Rec;
   pragma Volatile_Components (Interrupt_Buffers);

   Buffer : Interrupt_Buffers;  -- The actual buffers

   ----------------------------------------------------------------------------
   -- Conversion functions
   ----------------------------------------------------------------------------
   function To_Connection is new Ada.Unchecked_Conversion
                                 (Source => Z8536_Data,
                                  Target => Connection_Rec);
   -- Convert a byte into a connection record

   ------------------------------------
   function To_Z8536_Data is new Ada.Unchecked_Conversion
                                 (Source => Connection_Rec,
                                  Target => Z8536_Data);
   -- Convert a connection record to a byte

   ------------------------------------
   function To_Bit_Array is new Ada.Unchecked_Conversion
                                (Source => Z8536_Data,
                                 Target => Bit_Array);
   -- Convert a byte to an array of bits

   ------------------------------------
   function To_Hall (Connection : in Connection_Rec) return Hall_ID is
   -- Convert a connection record into a Hall number
   begin
      -- Use unchecked to conversion to move the connection rec into a byte,
      -- change from zero based to one based, and cast to a Hall number
      return Hall_ID (To_Z8536_Data (Connection) + 1);
   end To_Hall;
   pragma Inline (To_Hall);

   ------------------------------------
   function To_Connection (Hall : in Hall_ID) return Connection_Rec is
   -- Convert a Hall number into a connection record

      -- Hall number in byte form
      --   Bits 0-2 is the number of the bit in the port connected to Hall
      --   Bit  3   is the Z8536 port connected to Hall
      --   Bit  4   is the Z8536 chip connected to Hall
      --   Bits 5-7 is the CIO-INT board connected to Hall

   begin
      -- Cast the Hall to a byte, change to zero based, and return an
      -- unchecked conversion to a connection rec
      return To_Connection (Z8536_Data (Hall) - 1);
   end To_Connection;
   pragma Inline (To_Connection);


   ----------------------------------------------------------------------------
   -- Z8536 Port and Internal Register I/O operations
   ----------------------------------------------------------------------------

   procedure Read_Port (Board : in  Board_ID;
                        Chip  : in  Chip_ID;
                        Port  : in  Port_ID;
                        Data  : out Z8536_Data) is
   -- Read a Z8536 Port
   begin
      Data := Port_IO.In_Byte (Address => Address_Of (Board, Chip, Port));
   end Read_Port;
   pragma Inline (Read_Port);

   ------------------------------------
   procedure Write_Port (Board : in Board_ID;
                         Chip  : in Chip_ID;
                         Port  : in Port_ID;
                         Data  : in Z8536_Data) is
   -- Write to a Z8536 Port
   begin
      Port_IO.Out_Byte (Address => Address_Of (Board, Chip, Port),
                        Data    => Data);
   end Write_Port;
   pragma Inline (Write_Port);

   ------------------------------------
   procedure Read_Register
             (Board   : in  Board_ID;      -- which CIO-INT board
              Chip    : in  Chip_ID;       -- which Z8536 chip
              Address : in  Z8536_Address; -- which internal register
              Data    : out Z8536_Data) is -- the value read
   -- Read a Z8536 internal register
   --
   -- Preconditions  : The Z8536 chip is in State 0 (see chip manual)
   --
   -- Postconditions : The Z8536 chip is in State 0
   --
   -- Note:  Reading a Z8536 internal register is a two step process:
   --        First, write the address of that register to the chip's
   --               Control/Status register.  This address is stored
   --               in the internal pointer register
   --        Second, read the data from the chip's Control/Status register.
   --               The data read is from the internal register whose
   --               address is in the internal pointer register


      CS_Address : Port_IO.Address_Range; -- Address of this chip's CS register

   begin
      -- Determine the address of this chip's Control/Status Register
      CS_Address := Address_Of (Board, Chip, CS);
      -- Cast and write the Address to the Z8536 internal pointer register
      Port_IO.Out_Byte (Address => CS_Address,
                        Data    => Port_IO.Byte (Address));
      -- Get and return the data in the register
      Data := Port_IO.In_Byte (Address => CS_Address);
   end Read_Register;
   pragma Inline (Read_Register);

   ------------------------------------
   procedure Write_Register
             (Board   : in Board_ID;      -- which CIO-INT board
              Chip    : in Chip_ID;       -- which Z8536 chip
              Address : in Z8536_Address; -- which internal register
              Data    : in Z8536_Data) is -- the value to write
   -- Write to a Z8536 internal register
   --
   -- Preconditions  : The Z8536 chip is in State 0 (see chip manual)
   --
   -- Postconditions : The Z8536 chip is in State 0
   --
   -- Note:  Writing to a Z8536 internal register is a two step process:
   --        First, write the address of that register to the chip's
   --               Control/Status register.  This address is stored
   --               in the internal pointer register
   --        Second, write the data to the chip's Control/Status register.
   --               This data is placed in the internal register whose
   --               address is in the internal pointer register

      CS_Address : Port_IO.Address_Range; -- Address of this chip's CS register

   begin
      -- Determine the address of this chip's Control/Status Register
      CS_Address := Address_Of (Board, Chip, CS);
      -- Cast and write the Address to the Z8536 internal pointer register
      Port_IO.Out_Byte (Address => CS_Address,
                        Data    => Port_IO.Byte (Address));
      -- Write the data to the register
      Port_IO.Out_Byte (Address => CS_Address,
                        Data    => Data);
   end Write_Register;
   pragma Inline (Write_Register);


   ----------------------------------------------------------------------------
   -- Initialization routines
   ----------------------------------------------------------------------------
   procedure Reset_Z8536 (Board : in Board_ID;
                          Chip  : in Chip_ID) is
   -- Do a software reset of the given Z8536 chip using the method given
   -- in Chapter 6 of the Z8536 Technical Manual (see state machine diagram)
   --
   -- Preconditions  : none
   --
   -- Postconditions : The Z8536 chip is in State 0
   --                  All Z8536 internal registers are in their reset state
   --                     (See Z8536 Technical Manual for reset state values)
   --                      In particular, all port enable bits are 0

      Data : Z8536_Data;            -- Scratch data
      pragma Warnings (Off, Data);  -- We don't use the data, so don't warn me
   begin
      -- Read a byte of data from the CS port to insure State 0 or Reset State
      Read_Port (Board => Board, Chip => Chip,
                 Port  => CS,    Data => Data);

      -- If in State 0, the following write sets the internal pointer register
      --                to the Master Interrupt Control Register and changes the
      --                state to State 1.
      -- If in Reset State, the following write will clear the reset and change
      --                the state to State 0.
      Write_Port (Board => Board, Chip => Chip,
                  Port  => CS,    Data => 0);

      -- If in State 0, the following Read will leave us in State 0
      -- If in State 1, the following Read will leave us in State 0
      Read_Port (Board => Board, Chip => Chip,
                 Port  => CS,    Data => Data);

      -- Reset chip by writing Reset to the Master Interrupt Control Register
      -- (changes state to State 1 then to Reset State)
      Write_Register (Board   => Board, Chip => Chip,
                      Address => MIC,   Data => Reset);

      -- Clear the reset (changes state to State 0)
      Write_Port (Board => Board, Chip => Chip,
                  Port  => CS,    Data => Clear_Reset);
   end Reset_Z8536;

   ------------------------------------
   procedure Configure_Z8536_Chips is
   -- Configure both Z8536 chips on both boards
   --
   -- Preconditions  : none
   --
   -- Postconditions : The Z8536 chip is in State 0
   --                  Port A and B "Data Direction" set to input
   --                  Port A and B "Mode" set to OR  (pattern match on any bit)
   --                  Port A and B "Pattern Specification" set to interrupt on
   --                                any transition
   --                  Port A and B are enabled

   begin
      -- Configure both Z8536 chips on both boards
      for Board in Board_ID loop
         for Chip in Chip_ID loop

            -- Reset the chip
            Reset_Z8536 (Board, Chip);

            ---------------------------------------------------------
            -- Set Mode of both ports to bit ports with OR pattern match
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PAMS,  Data => OR_Mode);
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PBMS,  Data => OR_Mode);

            ---------------------------------------------------------
            -- Set Data Direction on both ports to all bits input
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PADD,  Data => All_Input);
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PBDD,  Data => All_Input);

            ---------------------------------------------------------
            -- Set the three Pattern Definition Registers on each of
            -- the two ports to match on any bit transistion
            for Port in Data_Port loop
               for Register in Pattern_Register loop
                  Write_Register
                    (Board   => Board,
                     Chip    => Chip,
                     Address => Address_Of_Pat_Reg (Port, Register),
                     Data    => Match_Any_Transition (Register));
               end loop;
            end loop;

            ---------------------------------------------------------
            -- Enable Ports A and B (do this after configuring ports)
            Write_Register (Board   => Board, Chip => Chip,
                            Address => MCC,   Data => Enable_A_B);
         end loop;
      end loop;
   end Configure_Z8536_Chips;


   ----------------------------------------------------------------------------
   -- Interrupt handler code (generic so we can use same for both boards)
   ----------------------------------------------------------------------------

   generic
      Board : in Board_ID;
   function Handler (Area : in System.Address;
                     Intr : in MHI.Hardware_Interrupt)
                                             return MHI.Handler_Return_Code;

   function Handler (Area : in System.Address;
                     Intr : in MHI.Hardware_Interrupt)
                                             return MHI.Handler_Return_Code is
      pragma Warnings (Off, Area);
      pragma Warnings (Off, Intr);

      Status      : Z8536_Data; -- Value from a Z8536 command & status register
      Status_Bits : Bit_Array;  -- Status as an array of bits
      Value       : Z8536_Data; -- The Value of the Port that interrupted
   begin
      -- Find out which Ports on which Chip have requested an interrupt.
      -- Search from lowest to highest priority (not sure this order is best).
      Chip_Loop :
      for Chip in reverse Chip_ID loop
         Port_Loop :
         for Port in reverse Data_Port loop
            -- Get the status data for this chip and port
            Read_Register (Board   => Board, Chip => Chip,
                           Address => Address_Of_CS_Reg (Port),
                           Data    => Status);
            -- convert byte to array of bits
            Status_Bits := To_Bit_Array (Status);

            -- Did this Chip/Port raise an interrupt?
            if Status_Bits (5) then  -- Bit 5 indicates Interrupt Pending

               -- Get the current value of the port
               Read_Port (Board => Board, Chip => Chip,
                          Port  => Port,  Data => Value);

               -- Put the information into the buffer for
               -- The Hall task To Process
               Buffer (Board).Data (Buffer (Board).Back) := (Chip, Port, Value);
               Buffer (Board).Back := Buffer (Board).Back + 1;

               -- Clear the Interrupt Pending and Interrupt Under Service bits
               -- of the appropriate Command & Status Register
               Write_Register (Board   => Board, Chip => Chip,
                               Address => Address_Of_CS_Reg (Port),
                               Data    => Clear_IP_IUS);
            end if;
         end loop Port_Loop;
      end loop Chip_Loop;
      return MHI.POSIX_INTR_HANDLED_NOTIFY;
   end Handler;

   ----------------------------------------------------------------------------
   -- Instantiate a handler for each of the two boards
   function Board_One_Handler is new Handler (Board => Board_One);
   function Board_Two_Handler is new Handler (Board => Board_Two);

   -- An array of interrupt handler addresses
   type Handler_Access_Array is
                             array (Board_ID) of MHI.Interrupt_Handler_Function;
   Handler_Address : constant Handler_Access_Array :=
                              (Board_One => Board_One_Handler'Access,
                               Board_Two => Board_Two_Handler'Access);


   ----------------------------------------------------------------------------
   -- Tasks that process Hall sensor triggers
   ----------------------------------------------------------------------------
   task type Hall_Task (Board : Board_ID) is
      pragma Priority (System.Priority'Last);
   end Hall_Task;

   task body Hall_Task is

      -- Use an array to store previous port values.
      -- Need previous values to detect a change from 0 to 1.
      type Port_Value_Array is array (Chip_ID, Data_Port) of Z8536_Data;

      Interrupt    : Interrupt_Rec;    -- Data from the interrupt handler
      Previous     : Port_Value_Array; -- Prior values of each port
      Changed_Bits : Bit_Array;        -- Port bits that changed from 0 to 1
      Hall_Info    : Connection_Rec;   -- Connection information for one Hall
      Hall_Number  : Hall_ID;          -- A triggered Hall sensor

      Result : Int range 0 .. 0;  -- Generated exception if not zero
      pragma Warnings (Off, Result);

      -- These values, returned from the call to MHI.Timedwait, are not used
      Handled_Intr    : aliased MHI.Hardware_Interrupt;
      Handled_Handler : aliased MHI.Interrupt_Handler_Function;

   begin
      -- Initialize all previous port states to zero
      Previous := (Chip_ID => (Data_Port => 2#00000000#));

      Hall_Info.Board := Board; -- This task only processes info for this board

      -- Associate the interrupt handler that "feeds" this task
      Result := MHI.Associate (Intr      => IRQ (Board),
                               Handler   => Handler_Address (Board),
                               Area      => System.Null_Address,
                               Area_Size => 0);

      loop -- Forever

         loop -- wait until there is something in the buffer

            -- Check buffer status (need MUTEX)
            Result := MHI.Lock (Intr => IRQ (Board));
            exit when Buffer (Board).Front /= Buffer (Board).Back;
            Result := MHI.Unlock  (Intr => IRQ (Board));

            -- Wait until an interrupt occurs and then test buffer again
            Result := MHI.Timedwait (Flags       => 0,     -- not used
                                     Abs_Timeout => null,  -- wait forever
                                     Intr        => Handled_Intr'Access,
                                     Handler     => Handled_Handler'Access);
         end loop;

         -- At this point the buffer is locked
         -- (We exited the loop before unlocking)

         -- Remove an interrupt record from the buffer
         Interrupt            := Buffer (Board).Data (Buffer (Board).Front);
         Buffer (Board).Front := Buffer (Board).Front + 1;

         -- No longer need the lock on the buffer
         Result := MHI.Unlock  (Intr => IRQ (Board));

         -- Convert interrupt chip & port values into Hall connection data
         Hall_Info.Chip := Interrupt.Chip;
         Hall_Info.Port := Interrupt.Port;

         -- Determine which bits in the port went from 0 at the last
         -- interrupt to 1 this interrupt.
         Changed_Bits := To_Bit_Array
                                   (Interrupt.Port_Value and not
                                    Previous (Interrupt.Chip, Interrupt.Port));

         -- Process any Halls that were triggered (went from 0 to 1)
         for Index in Bit_Number_Range loop
            if Changed_Bits (Index) then
               Hall_Info.Bit_Num := Index;
               Hall_Number := To_Hall (Hall_Info);

               -- Notify the train to update position
               begin
                  Process.all (Hall_Number);
               exception
                  when The_Error : others =>
                     Put ("Procedure Callback raised the exception " &
                          Ada.Exceptions.Exception_Name (The_Error));
                     raise; -- Give up and kill the Hall task
               end;
            end if;
         end loop;

         -- Update the previous port value with the current value
         Previous (Interrupt.Chip, Interrupt.Port) := Interrupt.Port_Value;

      end loop;
   exception
      when The_Error : others =>
         Put ("Hall task " & Board_ID'Image (Board) & " has died " &
              Ada.Exceptions.Exception_Name (The_Error));
   end Hall_Task;

   -- The task objects
   Board_One_Task : Hall_Task (Board_One);
   Board_Two_Task : Hall_Task (Board_Two);


   ----------------------------------------------------------------------------
   -- External Operations defined in the package specfication
   ----------------------------------------------------------------------------

   ------------------------------------
   function Is_Triggered (Hall : in Hall_ID) return Boolean is

      -- CIO-Int Board, Chip, Port, and Bit number to which Hall is connected
      Connection : Connection_Rec;

      -- Data from the Port (which is connected to 8 Hall sensors)
      Port_Byte : Z8536_Data; -- data as byte
      Port_Bits : Bit_Array;  -- data as array of bits

   begin
      -- Transform the Hall number into CIO-INT board connection data
      Connection := To_Connection (Hall);
      -- Read the port to which the Hall is connected
      Read_Port (Board => Connection.Board,
                 Chip  => Connection.Chip,
                 Port  => Connection.Port,
                 Data  => Port_Byte);
      -- Convert the byte of data to an array of bits for easy bit checking
      Port_Bits := To_Bit_Array (Port_Byte);
      -- Return the value of the bit connected to Hall
      return Port_Bits (Connection.Bit_Num);
   end Is_Triggered;

   ------------------------------------
   procedure Enable (Callback : in not null Callback_Ptr) is
   -- Internal
   --    Preconditions  : All Z8536 chips are in State 0
   --    Postconditions : All Z8536 chips are in State 0

      Result : Int range 0 .. 0;  -- Generated exception if not zero
      pragma Warnings (Off, Result);

   begin
      -- Save the pointer to the callback procedure
      Process := Callback;

      -- Enables interrupts for both ports on both Z8536 chips on both boards
      for Board in Board_ID loop
         for Chip in Chip_ID loop

            -- Disable Ports A and B while configuring interrupts
            Write_Register (Board   => Board, Chip => Chip,
                            Address => MCC,   Data => Disable_A_B);

            -- Clear any pending Port A interrupts
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PACS,  Data => Clear_IP_IUS);
            -- Enable Port A interrupts
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PACS,  Data => Set_IE);

            -- Clear any pending Port B interrupts
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PBCS,  Data => Clear_IP_IUS);
            -- Enable Port B interrupts
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PBCS,  Data => Set_IE);

            -- Enable Master (all interrupts) on this chip
            Write_Register (Board   => Board, Chip => Chip,
                            Address => MIC,   Data => Master_Enable);

            -- Enable Ports A and B
            Write_Register (Board   => Board, Chip => Chip,
                            Address => MCC,   Data => Enable_A_B);
         end loop;

         -- Now that the chips are all set up,
         -- Enable IRQ Interrupts for This Board
         Result := MHI.Unlock  (Intr => IRQ (Board));
      end loop;
   exception
      when The_Error : others =>
         Put ("Enabling Halls  has failed " &
              Ada.Exceptions.Exception_Name (The_Error));
         raise;
   end Enable;

   ------------------------------------
   procedure Disable is
   -- Internal
   --    Preconditions  : All Z8536 chips are in State 0
   --    Postconditions : All Z8536 chips are in State 0

      Result : Int range 0 .. 0;  -- Generated exception if not zero
      pragma Warnings (Off, Result);

   begin
      -- Disables interrupts from both ports on both Z8536 chips on both boards
      for Board in Board_ID loop

         -- Disable the IRQ for this board
         Result := MHI.Lock  (Intr => IRQ (Board));

         for Chip in Chip_ID loop

            -- Disable Ports A and B while configuring interrupts
            Write_Register (Board   => Board, Chip  => Chip,
                            Address => MCC,   Data  => Disable_A_B);

            -- Disable Master (all interrupts) on this chip
            Write_Register (Board   => Board, Chip => Chip,
                            Address => MIC,   Data => Master_Disable);

            -- Disable Port A interrupts
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PACS,  Data => Clear_IE);
            -- Clear any pending Port A interrupts
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PACS,  Data => Clear_IP_IUS);

            -- Disable Port B interrupts
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PBCS,  Data => Clear_IE);
            -- Clear any pending Port B interrupts
            Write_Register (Board   => Board, Chip => Chip,
                            Address => PBCS,  Data => Clear_IP_IUS);

            -- Enable Ports A and B
            Write_Register (Board   => Board, Chip => Chip,
                            Address => MCC,   Data => Enable_A_B);
         end loop;
      end loop;

      -- Clear the shared buffers
      for Board in Board_ID loop
         Buffer (Board).Front := 0;
         Buffer (Board).Back  := 0;
      end loop;
   exception
      when The_Error : others =>
         Put ("Disabling Halls  has failed " &
              Ada.Exceptions.Exception_Name (The_Error));
         raise;
   end Disable;


   ----------------------------------------------------------------------------
   procedure Initialize is
   begin
      -- Set up all the chips on all the boards
      Configure_Z8536_Chips;

      -- Clear the shared buffers
      for Board in Board_ID loop
         Buffer (Board).Front := 0;
         Buffer (Board).Back  := 0;
      end loop;

   end Initialize;
end Halls;
