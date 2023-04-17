with Ada.Strings.Bounded;

package Filepath is
   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Initialize;

   ---------------------------------
   --  Getting Files Subprograms  --
   ---------------------------------
   function Get
     (Name : in String;
      T    : in String)
     return String;

private
   package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (10_000);
   App_Filepath : SB.Bounded_String;
end Filepath;
