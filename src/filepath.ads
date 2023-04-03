with Interfaces.C.Strings; use Interfaces.C.Strings;

package Filepath is
   package C renames Interfaces.C;

   function Get_BMP (Name : in String) return String;
   function Get_WAV (Name : in String) return String;

   procedure Load_Filepath;
   procedure Free_Filepath;

private
   App_Filepath  : C.Strings.chars_ptr := C.Strings.Null_Ptr;
   Platform_Name : C.Strings.chars_ptr := C.Strings.Null_Ptr;
end Filepath;
