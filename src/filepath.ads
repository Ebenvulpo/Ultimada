with Interfaces.C.Strings; use Interfaces.C.Strings;

package Filepath is
   package C renames Interfaces.C;

   ----------------------------------
   --  Initialization Subprograms  --
   ----------------------------------
   procedure Deinitialize;
   procedure Initialize;

   ---------------------------------
   --  Getting Files Subprograms  --
   ---------------------------------
   function Get
     (Name : in String;
      T    : in String)
     return String;

private
   App_Filepath  : C.Strings.chars_ptr := C.Strings.Null_Ptr;
   Platform_Name : C.Strings.chars_ptr := C.Strings.Null_Ptr;
end Filepath;
