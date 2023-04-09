with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

package body Rand is
   ------------------------
   --  Rand Subprograms  --
   ------------------------
   function RandomN
     (Low  : in Integer;
      High : in Integer)
     return Integer
   is
      subtype RandRange is Integer range Low .. High;

      package R_Int is new Ada.Numerics.Discrete_Random (RandRange);
      use R_Int;

      G : Generator;
   begin
      Reset(G);

      return random(G);
   end RandomN;

   function RandomF return Float is
      use Ada.Numerics.Float_Random;

      G : Generator;
   begin
      Reset(G);

      return Random(G);
   end RandomF;
end Rand;
