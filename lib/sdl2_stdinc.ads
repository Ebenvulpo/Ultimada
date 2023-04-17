package SDL2_Stdinc is
   pragma Pure;

   type Sint64 is range -2**63 .. 2**63 - 1;
   type Sint32 is range -2**31 .. 2**31 - 1;
   type Sint16 is range -2**15 .. 2**15 - 1;
   type Sint8  is range -2**7  .. 2**7  - 1;

   for Sint64'Size use 64;
   for Sint32'Size use 32;
   for Sint16'Size use 16;
   for Sint8'Size  use 8;

   type Uint64 is mod 2**64;
   type Uint32 is mod 2**32;
   type Uint16 is mod 2**16;
   type Uint8  is mod 2**8;

   for Uint64'Size use 64;
   for Uint32'Size use 32;
   for Uint16'Size use 16;
   for UInt8'Size  use 8;
end SDL2_Stdinc;
