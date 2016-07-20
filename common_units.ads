package Common_Units is

   -- Represents a voltage
   type Volts is delta 10.0 / 2.0 ** 12 range -45_000.0 .. 45_000.0;

   -- Represents a percentage
   type Percent is delta 1.0 / 10.0 range 0.0 .. 100.0;

end Common_Units;


