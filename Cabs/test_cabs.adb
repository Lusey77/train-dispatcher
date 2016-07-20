with Cabs;
with Port_IO;
with System;
with Layout;
with Ada.Unchecked_Conversion;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
use type Port_IO.Address_Range;
with MaRTE_OS;
with Hand_Controller;
with Motors;
with Common_Units;
procedure Test_Cabs is

   Base : constant Port_IO.Address_Range := 16#200#;
   type Cab_Rec is
      record
         Even_Cab       : Cabs.Cab_ID;
         Even_Direction : Layout.Block_Polarity;
         Odd_Cab        : Cabs.Cab_ID;
         Odd_Direction  : Layout.Block_Polarity;
      end record;
   for Cab_Rec use
      record
         Even_Cab       at 0 range 0 .. 2;
         Even_Direction at 0 range 3 .. 3;
         Odd_Cab        at 0 range 4 .. 6;
         Odd_Direction  at 0 range 7 .. 7;
      end record;

   for Cab_Rec'Size use 8;
   for Cab_Rec'Bit_Order use System.Low_Order_First;

   function To_Byte is new Ada.Unchecked_Conversion (Source => Cab_Rec,
                                                    Target => Port_IO.Byte);

   My_Cab : Cab_Rec;
   Item   : Cabs.Percent;
   Value  : Common_Units.Percent;

begin

   Motors.Set (Motor     => 3,
            Direction => Layout.Right);

   Motors.Set (Motor     => 18,
               Direction => Layout.Right);

   Motors.Set (Motor     => 22,
               Direction => Layout.Right);

   Motors.Set (Motor     => 10,
               Direction => Layout.Right);

   Motors.Set (Motor     => 6,
               Direction => Layout.Left);
   Motors.Set (Motor     => 23,
               Direction => Layout.Left);
   Motors.Set (Motor     => 12,
               Direction => Layout.Left);
   Motors.Set (Motor     => 13,
               Direction => Layout.Left);
   Motors.Set (Motor     => 15,
               Direction => Layout.Left);

   My_Cab.Even_Direction := Layout.Normal;
   My_Cab.Even_Cab := 1;
   My_Cab.Odd_Direction := Layout.Normal;
   My_Cab.Odd_Cab := 1;

   Port_IO.Out_Byte (Address => Base + 3,
                     Data    => 128);
      Port_IO.Out_Byte (Address => Base + 7,
                     Data    => 128);
      Cabs.Set_Limit (Cab   => 1,
                      Value => 100);
   for Count in 0 .. 2 loop
      Port_IO.Out_Byte (Address => Base + Port_IO.Address_Range (Count),
                        Data    => To_Byte (My_Cab));
   end loop;
   for Count in 4 .. 6 loop
      Port_IO.Out_Byte (Address => Base + Port_IO.Address_Range (Count),
                        Data    => To_Byte (My_Cab));
   end loop;

   loop
      Hand_Controller.Get_Black_Knob (Controller => Hand_Controller.A,
                                      Black_Knob => Value);
      Cabs.Set (Cab   => 1,
                Value => Cabs.Percent (Value));
   end loop;

   Ada.Text_IO.Skip_Line;

--     loop
--        Ada.Text_IO.Put_Line ("Enter in value: ");
--        Ada.Integer_Text_IO.Get (Item);
--
--        Cabs.Set (Cab   => 1,
--                  Value => Item);
--
--        Port_IO.Out_Byte (Address => Base,
--                          Data    => To_Byte (My_Cab));
--
--     end loop;

end Test_Cabs;
