package Version is
   Major  : constant Natural := 0;
   Minor  : constant Natural := 0;
   Bigfix : constant Natural := 1;

   ---------------------------
   --  Version Subprograms  --
   ---------------------------
   function Get return String;
end Version;
