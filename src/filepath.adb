with SDL2_Filesystem;
with SDL2_Platform;

package body Filepath is
   ----------------
   --  Constants --
   ----------------
   Windows : constant String := "Windows";

   Assets  : constant String := "assets";

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Initialize is
      AP : constant String := SDL2_Filesystem.Get_Base_Path;
   begin
      App_Filepath := SB.To_Bounded_String (AP);
   end Initialize;

   ---------------------------------
   --  Getting Files Subprograms  --
   ---------------------------------
   function Get
     (Name : in String;
      T    : in String)
     return String
   is
      Slash : String := "/";
   begin
      if SDL2_Platform.Get_Platform = Windows then
         Slash := "\";
      end if;
      return SB.To_String (App_Filepath) & Assets & Slash & T & Slash & Name;
   end Get;
end Filepath;
