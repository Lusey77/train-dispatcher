--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By  : Daniel Lang ---------------------------------------------------
-- Modified By : John McCormick ------------------------------------------------
-- Updated     : March 2001 ----------------------------------------------------
--------------------------------------------------------------------------------
with System.Machine_Code; use System.Machine_Code;
package body Port_IO is

   -- The x86 IN and OUT instructions are accessed through the ASM funtion
   -- found in package System.Machine_Code.

   -- For all routines, we use register DX to hold the address value.
   -- this avoids the hassles of determining if the value is in 0..255,
   -- where we can use immediate mode.

   -- We inline our procedures and functions to avoid
   -- extra overhead for the assembly calls.



   ----------------------------------------------------------------------------
   procedure Out_Byte (Address : in Address_Range;
                       Data    : in Byte) is
   begin
      System.Machine_Code.Asm
        (Template => "outb %%al, %%dx",
         Outputs  =>  No_Output_Operands,
         Inputs   => (Byte'Asm_Input ("a", Data),
                      Address_Range'Asm_Input ("d", Address)),
         Clobber  => "",
         Volatile => True);

   end Out_Byte;
   pragma Inline (Out_Byte);

   ----------------------------------------------------------------------------
   procedure Out_Word (Address : in Address_Range;
                       Data    : in Word) is
   begin
      System.Machine_Code.Asm
        (Template => "outw %%ax, %%dx",
         Outputs  =>  No_Output_Operands,
         Inputs   => (Word'Asm_Input ("a", Data),
                      Address_Range'Asm_Input ("d", Address)),
         Clobber  => "",
         Volatile => True);

   end Out_Word;
   pragma Inline (Out_Word);

   ----------------------------------------------------------------------------
   procedure Out_Long (Address : in Address_Range;
                       Data    : in Longword) is
   begin
      System.Machine_Code.Asm
        (Template => "outl %%eax, %%dx",
         Outputs  =>  No_Output_Operands,
         Inputs   => (Longword'Asm_Input ("a", Data),
                      Address_Range'Asm_Input ("d", Address)),
         Clobber  => "",
         Volatile => True);

   end Out_Long;
   pragma Inline (Out_Long);


   ---------------------------------------------------------------------------
   function In_Byte (Address : in Address_Range) return Byte is
      Result : Byte;
   begin
      System.Machine_Code.Asm
        (Template => "inb %%dx, %%al",
         Outputs  => Byte'Asm_Output ("=a", Result),
         Inputs   => Address_Range'Asm_Input ("d", Address),
         Clobber  => "",
         Volatile => True);

      return Result;
   end In_Byte;
   pragma Inline (In_Byte);

   --------------------------------------------------------------------------
   function In_Word (Address : in Address_Range) return Word is
      Result : Word;
   begin
      System.Machine_Code.Asm
        (Template => "inw %%dx, %%ax",
         Outputs  => Word'Asm_Output ("=a", Result),
         Inputs   => Address_Range'Asm_Input ("d", Address),
         Clobber  => "",
         Volatile => True);

      return Result;
   end In_Word;
   pragma Inline (In_Word);

   --------------------------------------------------------------------------
   function In_Long (Address : in Address_Range) return Longword is
      Result : Longword;
   begin
      System.Machine_Code.Asm
        (Template => "inl %%dx, %%eax",
         Outputs  => Longword'Asm_Output ("=a", Result),
         Inputs   => Address_Range'Asm_Input ("d", Address),
         Clobber  => "",
         Volatile => True);

      return Result;
   end In_Long;
   pragma Inline (In_Long);

end Port_IO;
