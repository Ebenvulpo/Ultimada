with Interfaces.C.Strings; use Interfaces.C.Strings;

package Filepath is
   package C renames Interfaces.C;

   procedure Load_Filepath;
   procedure Free_Filepath;
   function  Get return C.Strings.chars_ptr;

private
   App_Filepath : C.Strings.chars_ptr := C.Strings.Null_Ptr;
end Filepath;
