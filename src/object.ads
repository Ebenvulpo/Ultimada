package Object is
   type Object_Type is tagged limited private;

   subtype Item_Type is Natural;

   -----------------
   --  Constants  --
   -----------------
   Item_Chest        : constant Item_Type := 0;
   Item_Gem_Amethyst : constant Item_Type := 1;
   Item_Gem_Emerald  : constant Item_Type := 2;
   Item_Gem_Ruby     : constant Item_Type := 3;
   Item_Log          : constant Item_Type := 4;
   Item_Snowball     : constant Item_Type := 5;
   Item_Star         : constant Item_Type := 6;
   Item_Star2        : constant Item_Type := 7;

   Item_None         : constant Item_Type := 128;

   ---------------------------------
   --  Initalization Subprograms  --
   ---------------------------------
   procedure Initialize
     (Object : in out Object_Type;
      Item   : in     Item_Type);

   --------------------------
   --  Object Subprograms  --
   --------------------------
   function Get_Type (Object : in Object_Type) return Item_Type;

private
   type Object_Type is tagged limited
      record
	 Item : Item_Type := Item_None;
      end record;
end Object;
