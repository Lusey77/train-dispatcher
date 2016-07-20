--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By  : Daniel Lang ---------------------------------------------------
-- Modified By : John McCormick ------------------------------------------------
-- Updated     : March 2001 ----------------------------------------------------
--------------------------------------------------------------------------------
with Interfaces;    -- Unsigned types.
package Port_IO  is
   pragma Pure (Port_IO);

   -- This package provides access to I/O ports.  Essentially it provides the
   -- Ada programmer with the Intel x86 IN and OUT instructions.

   -- Port addresses
   type Address_Range is new Interfaces.Unsigned_16;

   -- Data for IN and OUT instructions
   type Byte     is new Interfaces.Unsigned_8;
   type Word     is new Interfaces.Unsigned_16;
   type Longword is new Interfaces.Unsigned_32;

   ----------------------------------------------------------------------------
   -- The following procedures write a byte, word, or longword to an I/O port

   procedure Out_Byte (Address : in Address_Range;
                       Data    : in Byte);

   procedure Out_Word (Address : in Address_Range;
                       Data    : in Word);

   procedure Out_Long (Address : in Address_Range;
                       Data    : in Longword);


   ----------------------------------------------------------------------------
   -- The following functions read a byte, word, or longword from an I/O port

   function In_Byte (Address : in Address_Range) return Byte;

   function In_Word (Address : in Address_Range) return Word;

   function In_Long (Address : in Address_Range) return Longword;

end Port_IO;
