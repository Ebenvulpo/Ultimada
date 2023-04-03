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
   function Get_BMP (Name : in String) return String;
   function Get_WAV (Name : in String) return String;

private
   App_Filepath  : C.Strings.chars_ptr := C.Strings.Null_Ptr;
   Platform_Name : C.Strings.chars_ptr := C.Strings.Null_Ptr;
end Filepath;
