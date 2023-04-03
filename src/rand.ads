package Rand is
   ------------------------
   --  Rand Subprograms  --
   ------------------------
   function randomN(low: in Integer; high: in Integer) return Integer;
   function noise2D(low: in Integer; high: in Integer) return Integer;
   function randomF return Float;
end Rand;
