package body Object is
   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Initialize
     (Object : in out Object_Type;
      Item   : in     Item_Type)
   is
   begin
      Object.Item := Item;
   end Initialize;

   --------------------------
   --  Object Subprograms  --
   --------------------------
   function Get_Type (Object : in Object_Type) return Item_Type is
   begin
      return Object.Item;
   end Get_Type;
end Object;
