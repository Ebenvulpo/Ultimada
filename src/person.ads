package Person is
   pragma Pure;

   subtype Person_Type is Natural;

   -----------------
   --  Constants  --
   -----------------
   Person_Pink    : constant Person_Type := 0;
   Skeleton_Pink  : constant Person_Type := 1;
   Skeleton2_Pink : constant Person_Type := 2;
   Slime1         : constant Person_Type := 3;
   Slime2         : constant Person_Type := 4;
   Zombie_Pink    : constant Person_Type := 5;

   Person_None    : constant Person_Type := 128;
end Person;
