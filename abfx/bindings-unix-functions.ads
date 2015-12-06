
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Bindings.Unix.Types; use Bindings.Unix.Types;

package Bindings.Unix.Functions is

   function poll(
      fds     : Pollfd_Pointer;
      nfds    : unsigned_long;
      timeout : int) return int;

   function poll(
      fds     : Pollfd_Array;
      nfds    : unsigned_long;
      timeout : int) return int;
   pragma Import(C, poll, "poll");

   function Get_Time return Timeval_Type;

   function "="(t1, t2 : Timeval_Type) return Boolean;
   function "<"(t1, t2 : Timeval_Type) return Boolean;
   function "<="(t1, t2 : Timeval_Type) return Boolean;
   function ">"(t1, t2 : Timeval_Type) return Boolean;
   function ">="(t1, t2 : Timeval_Type) return Boolean;

   function "+"(t1, t2 : Timeval_Type) return Timeval_Type;
   function "-"(t1, t2 : Timeval_Type) return Timeval_Type;

end Bindings.Unix.Functions;

