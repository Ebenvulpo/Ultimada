package Rand is
   ------------------------
   --  Rand Subprograms  --
   ------------------------
   function RandomN
     (Low  : in Integer;
      High : in Integer)
     return Integer;

   function RandomF return Float;
end Rand;
