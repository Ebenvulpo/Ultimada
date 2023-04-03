with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

package body Rand is
   ------------------------
   --  Rand Subprograms  --
   ------------------------
   function randomN(low: in Integer; high: in Integer) return Integer is
      subtype randRange is Integer range low..high;
      package R_Int is new Ada.Numerics.Discrete_Random(randRange);
      use R_Int;
      gen : Generator;
   begin
      reset(gen);
      return random(gen);
   end randomN;

   function noise2D(low: in Integer; high: in Integer) return Integer is
   begin
      return 1;
   end noise2D;

   function randomF return Float is
      use Ada.Numerics.Float_Random;
      G : Generator;
   begin
      Reset(G);
      return Random(G);
   end randomF;
end Rand;
